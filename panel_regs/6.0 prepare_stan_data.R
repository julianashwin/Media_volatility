setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)
require(lfe)



### Load the clean equity and article data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
clean_data <- read.csv(clean_filename, stringsAsFactors = FALSE)


article_variables <- c("Code", "Date", "highlow", "article_id", "headline", "main_text")
article_data <- clean_data[which(!is.na(clean_data$article_id)),article_variables]


############################# Separate out articles and only take first max.n words ############################# 
max.n <- 50

# A function to split out previously collapsed articles
separate_function <- function(article, article_variables){
  articles <- str_split(article$main_text, " \n ---- \n ")
  articles <- articles[[1]][which(articles[[1]] != "")]
  ids <- str_split(article$article_id, " \n ---- \n ")
  ids <- ids[[1]][which(ids[[1]] != "")]
  headlines <- str_split(article$headline, " \n ---- \n ")
  headlines <- headlines[[1]][which(headlines[[1]] != "")]
  temp.n <- length(ids)
  
  temp.df <- as.data.frame(matrix(NA, nrow = temp.n, ncol = length(article_variables)))
  colnames(temp.df) <- article_variables
  temp.df$Code <- article$Code
  temp.df$Date <- article$Date
  temp.df$highlow <- article$highlow
  temp.df$article_id <- ids
  temp.df$headline <- headlines
  temp.df$main_text <- articles
  
  return(temp.df)
}

clean.df <- article_data[0,]
for (i in 1:nrow(article_data)){
  print(i)
  temp.df <- separate_function(article_data[i,], article_variables)
  
  clean.df <- rbind(clean.df, temp.df)
}


### Isolate the top 50(?) words
trunc_fun <- function(x, max.n) {
  ul <- unlist(strsplit(x, split = "\\s+"))
  n <- length(ul) 
  if (n < max.n){
    ul = unlist(strsplit(x, split = "\\s+"))[1:n]
  } else if (n >= max.n){
    ul = unlist(strsplit(x, split = "\\s+"))[1:max.n]
  } else {
    print("an imaginary number of words??")
  }
  
  paste(ul,collapse=" ")
}

article <- clean.df[51,"main_text"]
article
trunc_fun(article, 50)

short_text <- lapply((clean.df$main_text), trunc_fun, max.n = max.n)
clean.df$short_text <- short_text

dupe_ids <- clean.df[duplicated(clean.df[,c("Code", "article_id")]),"article_id"]
dupe.df <- clean.df[which(clean.df$article_id %in% dupe_ids),]
clean.df <- clean.df[!duplicated(clean.df[,c("Code", "article_id")]),]
clean.df <- clean.df[,c("Code", "Date", "highlow", "article_id", "headline", "short_text")]


clean.df.collapsed <- clean.df[0,]
codes <- unique(clean.df$Code)
for (code in codes){
  print(code)
  code_data <- clean.df[which(clean.df$Code == code),]
  code_data$Date <- as.Date(code_data$Date)
  for (d in unique(code_data$Date)){
    data_temp <- code_data[which(code_data$Date == d),]
    
    if(length(data_temp[,1])>1){
      texts <- paste(data_temp$short_text, collapse = " \n ---- \n ")
      headies <- paste(data_temp$headline, collapse = " \n ---- \n ")
      ids <- paste(data_temp$article_id, collapse = " \n ---- \n ")
      data_temp <- data_temp[1,]
      data_temp$short_text <- texts
      data_temp$headline <- headies
      data_temp$article_id <- ids
    }
    
    clean.df.collapsed <- rbind(clean.df.collapsed, data_temp)
  }
  
}

clean.df.collapsed$obs_id <- paste0(clean.df.collapsed$Code, "_", clean.df.collapsed$Date)

clean.df <- clean.df.collapsed

############################# Clean the text data ############################# 

# Remove some characters that can be problematic for tm package
clean.df$short_text <- gsub("-", " ", clean.df$short_text)
clean.df$short_text <- gsub("/'", "", clean.df$short_text)
clean.df$short_text <- gsub("’", "", clean.df$short_text)
clean.df$short_text <- gsub("‘", "", clean.df$short_text)
clean.df$short_text <- gsub("“", "", clean.df$short_text)
clean.df$short_text <- gsub("”", "", clean.df$short_text)
clean.df$short_text <- gsub("€", " ", clean.df$short_text)
clean.df$short_text <- gsub("£", " ", clean.df$short_text)
clean.df$short_text <- gsub(",", " ", clean.df$short_text)
clean.df$short_text <- gsub(":", " ", clean.df$short_text)



articles.corpus <- Corpus(VectorSource(unlist(clean.df[, "short_text"])))
# Preliminary cleaning
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, stripWhitespace)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removeNumbers)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removePunctuation)
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, content_transformer(tolower))
inspect(articles.corpus[[700]])
articles.corpus <- tm_map(articles.corpus, removeWords, stopwords("english"))
inspect(articles.corpus[[700]])
articles.corpus <-  tm_map(articles.corpus, stemDocument)
inspect(articles.corpus[[700]])





############################# Reduce dimensionality ############################# 

### Convert to document-term-matrix
articles.dtm <- DocumentTermMatrix(articles.corpus, control = list(minWordLength = 3))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
articles.dtm$dimnames$Docs <- clean.df$obs_id
print(paste("Dimensions of articles.dtm are", dim(articles.dtm)[1], "documents and", 
            dim(articles.dtm)[2], "words in vocab"))
total_vocab <- articles.dtm$dimnames$Terms


### Calculate the tf and df score for each term
term_freq <- col_sums(articles.dtm) # Total number of times a term appears
doc_freq <- col_sums(articles.dtm > 0) # Total number of documents it appears i
cor.test(term_freq, doc_freq)

# Which terms appear in three or fewer documents
rare_terms <- articles.dtm[,doc_freq <= 3]$dimnames$Terms
print(rare_terms[1:100])

# Remove terms which are mentioned in 3 or fewer documents
articles.dtm <- articles.dtm[,doc_freq > 3]

### Calculate tf-idf
term_tfidf <- tapply(articles.dtm$v/row_sums(articles.dtm)[articles.dtm$i], articles.dtm$j, mean) *
  log2(nDocs(articles.dtm)/col_sums(articles.dtm > 0))
summary(term_tfidf)
quantile(term_tfidf, c(.01, .5, .99)) 

low_terms <- articles.dtm[,term_tfidf < 0.11]$dimnames$Terms
print(low_terms)

# Remove terms with very low tf-idf ranking
articles.dtm <- articles.dtm[,term_tfidf >= 0.1]

# Remove all empty documents
articles.dtm <- articles.dtm[row_sums(articles.dtm) > 0,]

print(paste("After removing rare terms, the dimensions of articles.dtm are now", 
            dim(articles.dtm)[1], "documents and", dim(articles.dtm)[2], "words in vocab"))
clean_vocab <- articles.dtm$dimnames$Terms



### Identify the removed words
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

removed_vocab <- outersect(total_vocab, clean_vocab)



### Now remove the identified words from the documents

# Need to remove the vocab in stages as it is to large for the gsub function
N <- length(removed_vocab)
n <- N/1000
n <- round(n+1,0)
j <- 1
for (i in 1:n){
  print(j)
  if ((j+999) < N){
    articles.corpus <- tm_map(articles.corpus, removeWords, removed_vocab[j:(j+999)])
    inspect(articles.corpus[[700]])
    print(j+999)
  } else if (j < N){
    articles.corpus <- tm_map(articles.corpus, removeWords, removed_vocab[j:N])
    inspect(articles.corpus[[700]])
    print (N)
  }
  j <- j+1000
}
articles.corpus <- tm_map(articles.corpus, stripWhitespace)
inspect(articles.corpus[[700]])


clean.df$clean_text <- sapply(articles.corpus, as.character)
clean.df$clean_text <- str_trim(clean.df$clean_text)
clean.df[700, c("short_text", "clean_text")]

# Remove the empty documents
clean.df <-  clean.df[which(clean.df$clean_text !=""),]



############################# Write to file ############################# 
clean.df <- clean.df %>%
  select(obs_id, Code, Date, highlow, article_id, headline, clean_text)

clean_filename <- paste(clean_dir,"/FT/matched/short_articles.csv", sep="")
write.csv(clean.df, file = clean_filename, row.names = FALSE)
# clean.df <- read.csv(clean_filename, stringsAsFactors = FALSE)




############################# End ############################# 