setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())

require(stringr)
require(tm)
require(tidytext)
require(wordcloud)
require(lfe)
require(plm)
require(slam)

clean_dir <- "~/Documents/DPhil/Clean_Data"
export_dir <- "~/Desktop/"



### Load the clean equity and article data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
full_data <- read.csv(import_filename, stringsAsFactors = FALSE)
full_data$period <- as.integer(as.factor(full_data$Date))
full_data <- pdata.frame(full_data, index = c("Code", "period"))
full_data$Date <- as.Date(full_data$Date)



test <- felm(highlow ~ abs_intra_day + mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)


short_data <- data.frame(full_data[,c("Code", "Date", "mention", "highlow")])


short_data$highlow_lag <- plm::lag(short_data$highlow, 1)

model1 <- felm(highlow ~ mention , data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention| Code, data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention | Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + plm::lag(highlow, 1) | Code + Date, data = full_data)
summary(model4)

stargazer(model1, model2, model3, model4, table.placement = "H", df = FALSE)



### Media effect robustness

model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(model1)
model2 <- felm(highlow ~ ner_mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(model2)
model3 <- felm(highlow ~ head_mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data[which(full_data$highlow <= 25),])
summary(model4)
model5 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data[which(full_data$highlow <= 10),])
summary(model5)
model6 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data[which(full_data$Date <= "2007-01-01"),])
summary(model6)
model7 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data[which(full_data$Date >= "2007-01-01" & 
                                                                                                full_data$Date < "2010-01-01"),])
summary(model7)
model8 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data[which(full_data$Date >= "2010-01-01"),])
summary(model8)
model9 <- felm(Volume ~ mention + plm::lag(Volume, 1:10) | Code + Date, data = full_data)
summary(model9)


stargazer(model1, model2, model3, model4, model5, model6, model7, model8, model9,
          table.placement = "H", df = FALSE, title = "Robustness")



### Persistence controls

full_data$highlow_sqrt <- full_data$highlow^(1/2)
full_data$highlow_sq <- full_data$highlow^(2)
full_data$highlow_32 <- full_data$highlow^(3/2)
full_data$highlow_cb <- full_data$highlow^(3)
full_data$highlow_qrt <- full_data$highlow^(4)
full_data$abs_intra_day_sqrt <- full_data$abs_intra_day^(1/2)
full_data$abs_intra_day_sq <- full_data$abs_intra_day^(2)
full_data$abs_intra_day_32 <- full_data$abs_intra_day^(3/2)
full_data$abs_intra_day_cb <- full_data$abs_intra_day^(3)
full_data$abs_intra_day_qrt <- full_data$abs_intra_day^(4)
full_data$Volume_sqrt <- full_data$Volume^(1/2)
full_data$Volume_sq <- full_data$Volume^(2)
full_data$Volume_32 <- full_data$Volume^(3/2)
full_data$Volume_cb <- full_data$Volume^(3)
full_data$Volume_qrt <- full_data$Volume^(4)
full_data$Turnover_sqrt <- full_data$Turnover^(1/2)
full_data$Turnover_sq <- full_data$Turnover^(2)
full_data$Turnover_32 <- full_data$Turnover^(3/2)
full_data$Turnover_cb <- full_data$Turnover^(3)
full_data$Turnover_qrt <- full_data$Turnover^(4)
full_data$VI_put_sqrt <- full_data$VI_put^(1/2)
full_data$VI_put_sq <- full_data$VI_put^(2)
full_data$VI_put_32 <- full_data$VI_put^(3/2)
full_data$VI_put_cb <- full_data$VI_put^(3)
full_data$VI_put_qrt <- full_data$VI_put^(4)


model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day, 1:10) 
               | Code + Date, data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day, 1:10)
               + plm::lag(Volume, 1:10) + 
                 plm::lag(Turnover, 1:10)| Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day, 1:10)
               + plm::lag(Volume, 1:10) + plm::lag(highlow_sqrt, 1:10) + plm::lag(highlow_sq, 1:10)
               + plm::lag(highlow_32, 1:10) + plm::lag(highlow_cb, 1:10) + plm::lag(highlow_qrt, 1:10)
               + plm::lag(Turnover, 1:10)| Code + Date, data = full_data)
summary(model4)
model5 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day, 1:10)
               + plm::lag(Volume, 1:10) + plm::lag(highlow_sqrt, 1:10) + plm::lag(highlow_sq, 1:10)
               + plm::lag(highlow_32, 1:10) + plm::lag(highlow_cb, 1:10) + plm::lag(highlow_qrt, 1:10)
               + plm::lag(abs_intra_day_sqrt, 1:10) + plm::lag(abs_intra_day_sq, 1:10)
               + plm::lag(abs_intra_day_32, 1:10) + plm::lag(abs_intra_day_cb, 1:10) + plm::lag(abs_intra_day_qrt, 1:10)
               + plm::lag(Volume_sqrt, 1:10) + plm::lag(Volume_sq, 1:10)
               + plm::lag(Volume_32, 1:10) + plm::lag(Volume_cb, 1:10) + plm::lag(Volume_qrt, 1:10)
               + plm::lag(Turnover_sqrt, 1:10) + plm::lag(Turnover_sq, 1:10)
               + plm::lag(Turnover_32, 1:10) + plm::lag(Turnover_cb, 1:10) + plm::lag(Turnover_qrt, 1:10)
               + plm::lag(Turnover, 1:10)| Code + Date, data = full_data)
summary(model5)
model6 <- felm(highlow ~ mention + plm::lag(highlow, 1:20) + plm::lag(abs_intra_day, 1:20)
                  + plm::lag(Volume, 1:20) + plm::lag(highlow_sqrt, 1:20) + plm::lag(highlow_sq, 1:20)
                  + plm::lag(highlow_32, 1:20) + plm::lag(highlow_cb, 1:20) + plm::lag(highlow_qrt, 1:20)
                  + plm::lag(abs_intra_day_sqrt, 1:10) + plm::lag(abs_intra_day_sq, 1:10)
                  + plm::lag(abs_intra_day_32, 1:10) + plm::lag(abs_intra_day_cb, 1:10) + plm::lag(abs_intra_day_qrt, 1:10)
                  + plm::lag(Volume_sqrt, 1:20) + plm::lag(Volume_sq, 1:20)
                  + plm::lag(Volume_32, 1:20) + plm::lag(Volume_cb, 1:20) + plm::lag(Volume_qrt, 1:20)
                  + plm::lag(Turnover_sqrt, 1:20) + plm::lag(Turnover_sq, 1:20)
                  + plm::lag(Turnover_32, 1:20) + plm::lag(Turnover_cb, 1:20) + plm::lag(Turnover_qrt, 1:20)
                  + plm::lag(Turnover, 1:20)| Code + Date, data = full_data)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE, title = "Controlling for the past", out = "past_table.tex")



############################# Clean the text data ############################# 
full_data$clean_text <- full_data$main_text

# Remove some characters that can be problematic for tm package
full_data$clean_text <- gsub("-", " ", full_data$clean_text)
full_data$clean_text <- gsub("/'", "", full_data$clean_text)
full_data$clean_text <- gsub("’", "", full_data$clean_text)
full_data$clean_text <- gsub("‘", "", full_data$clean_text)
full_data$clean_text <- gsub("“", "", full_data$clean_text)
full_data$clean_text <- gsub("”", "", full_data$clean_text)
full_data$clean_text <- gsub("€", " ", full_data$clean_text)
full_data$clean_text <- gsub("£", " ", full_data$clean_text)
full_data$clean_text <- gsub(",", " ", full_data$clean_text)
full_data$clean_text <- gsub(":", " ", full_data$clean_text)

text.df <- full_data[which(!is.na(full_data$article_id)),]
text.df$obs_id <- paste("id_", 1:nrow(text.df))

articles.corpus <- Corpus(VectorSource(unlist(text.df[, "clean_text"])))
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
articles.corpus <- tm_map(articles.corpus, stripWhitespace)
inspect(articles.corpus[[700]])


### Convert to document-term-matrix
articles.dtm <- DocumentTermMatrix(articles.corpus, control = list(minWordLength = 3))

# Make sure each document is labelled with a unique identifier, in order to merge back later if necessary
articles.dtm$dimnames$Docs <- text.df$obs_id
print(paste("Dimensions of articles.dtm are", dim(articles.dtm)[1], "documents and", 
            dim(articles.dtm)[2], "words in vocab"))
total_vocab <- articles.dtm$dimnames$Terms


### Calculate the tf and df score for each term
term_freq <- col_sums(articles.dtm) # Total number of times a term appears
doc_freq <- col_sums(articles.dtm > 0) # Total number of documents it appears i
cor.test(term_freq, doc_freq)
text.df$word_count <- row_sums(articles.dtm)
text.df$clean_text <- sapply(articles.corpus, as.character)
text.df$clean_text <- str_trim(text.df$clean_text)

text.df <- select(text.df, Code, Date, obs_id, clean_text)


undesirable_words_short <- c("www", "ft", "http", "com", "day", "week", "month")

text_tidy <- text.df %>%
  unnest_tokens(word, clean_text) %>% # Break the lyrics into individual words
  filter(!word %in% undesirable_words_short) %>% # Remove undesirables
  filter(!nchar(word) < 2) # Remove one or two character tokens

# Unnest to get a template to bind to
text_loughran <- text_tidy %>%
  inner_join(get_sentiments("loughran"))
text_nrc <- text_tidy %>%
  inner_join(get_sentiments("nrc"))

test.df <- merge(text_loughran, text.df, by = c("obs_id"), all.x = TRUE) 



#### Check that the various categories have been picked up
loughran_plot <- text_loughran %>%
  group_by(sentiment) %>%
  summarise(word_count = n()) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, word_count)) %>%
  ggplot(aes(sentiment, word_count, fill = sentiment)) +
  geom_col() +
  guides(fill = FALSE) +
  #theme_lyrics() +
  labs(x = NULL, y = "Word Count") +
  #scale_y_continuous(limits = c(0, 8000)) +
  ggtitle("Loughran Sentiment") +
  coord_flip()
loughran_plot


# Break down Loughran sentiment by observation
FT_article_sentiment <- text_loughran %>%
  count(sentiment, obs_id) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(polarity = positive - negative,
         percent_positive = positive / (positive + negative) * 100)
FT_article_sentiment$percent_positive <- FT_article_sentiment$percent_positive - mean(FT_article_sentiment$percent_positive, na.rm = TRUE)

FT_article_sentiment <- merge(FT_article_sentiment, text.df, by = "obs_id")

FT_article_sentiment <- merge(full_data, FT_article_sentiment, by = c("Code", "Date"), all.x = TRUE)
FT_article_sentiment <- pdata.frame(FT_article_sentiment, index = c("Code", "Date"))
FT_article_sentiment[which(is.na(FT_article_sentiment$percent_positive)),"percent_positive"] <- 0

test <- felm(highlow ~ abs_intra_day + mention + plm::lag(highlow, 1:10) | Code + Date, data = FT_article_sentiment)
summary(test)
test <- felm(highlow ~ abs_intra_day + mention + percent_positive + plm::lag(highlow, 1:10) | Code + Date, data = FT_article_sentiment)
summary(test)

test <- felm(intra_day ~ mention + plm::lag(percent_positive,0) + plm::lag(intra_day, 1:10) | Code + Date, data = FT_article_sentiment)
summary(test)
test <- felm(pChange ~ mention + plm::lag(percent_positive,0) + plm::lag(pChange, 1:10) | Code + Date, data = FT_article_sentiment)
summary(test)
test <- felm(intra_day ~ mention + plm::lag(percent_positive,-1) + plm::lag(intra_day, 1:10) | Code, data = FT_article_sentiment)
summary(test)
test <- felm(pChange ~ mention + plm::lag(percent_positive,-1) + plm::lag(pChange, 1:10) | Code, data = FT_article_sentiment)
summary(test)


full_data <- FT_article_sentiment

full_data$article_id <- as.character(full_data$article_id)
x <- str_count(full_data$article_id, "[0-9]+")
full_data$mention_num <- x
full_data[which(is.na(full_data$mention_num)),"mention_num"] <- 0
full_data <- pdata.frame(full_data, index = c("Code", "Date"))


test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + mention_num + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + sqrt(mention_num) + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)



export_filename <- paste(clean_dir, "/FT/matched/clean_data_export.csv", sep = "")
write.csv(full_data, export_filename, row.names = FALSE)








