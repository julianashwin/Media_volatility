setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())

#options(mc.cores=parallel::detectCores())

require(rstan)
require(gtools)
require(shinystan)
require(stringr)
require(tm)
#require(tidytext)
#require(tidyr)
require(slam)
require(wordcloud)

clean_dir <- "~/Documents/DPhil/Clean_Data"
export_dir <- "~/Desktop/"


############################# Test case with simulated data ############################# 

V <- 5; # words: river, stream, bank, money, loan
K <- 2; # topics: RIVER, BANK

beta <- array(NA,c(K,V));

beta[1,] = c(0.330, 0.330, 0.330, 0.005, 0.005);
beta[2,] = c(0.005, 0.005, 0.330, 0.330, 0.330);

M <- 150;  # docs
avg_doc_length <- 30;
doc_length <- rpois(M,avg_doc_length);
N <- sum(doc_length);

alpha <- rep(1/K,K);
eta <- rep(1/V,V);

theta <- rdirichlet(M,alpha);

total_doc <- rep("", M)
w <- rep(NA,N);
doc <- rep(NA,N);
n <- 1;
for (m in 1:M) {
  for (i in 1:doc_length[m]) {
    z <- which(rmultinom(1,1,theta[m,]) == 1);
    w[n] <- which(rmultinom(1,1,beta[z,]) == 1);
    doc[n] <- m;
    n <- n + 1;
  }
  words <- as.character(w[(n-doc_length[m]):(n-1)])
  total_doc[m] <- paste(words, collapse = " ")
}


lda_data = list(
  K = K, V = V, M = M, N = N, w = w, doc = doc, alpha = alpha, beta = eta
)
warmup = 100
nsim = 200


start_time <- Sys.time()

fit1 <- stan(
  file = "lda.stan",  # Stan program
  data = lda_data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = warmup,          # number of warmup iterations per chain
  iter = nsim,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 1,             # no progress shown
  algorithm = "NUTS"         # sampling algorithm
)
end_time <- Sys.time()

end_time - start_time

fit_summary <- summary(fit1)
fit_summary$summary["theta[1,1]","mean"]
fit_summary$summary["theta[1,2]","mean"]
theta[1,]

# print(fit1)
print(fit1, pars = c("phi"))

### Plot word clouds 
list_of_draws <- extract(fit1)

phi_samples <- list_of_draws$phi[(warmup+1):nsim,,]
topics <- matrix(NA, nrow = V, ncol = K)
for (i in 1:K){
  topicvector <- phi_samples[,i,]
  topicvector <- colMeans(topicvector)
  sum(topicvector)
  topics[,i] <- topicvector
}
  
vocab <- c("river", "stream", "bank", "money", "loan")

# opar <- par()  
for (i in 1:K){
  # Filename for exported graphic
  file_name <- paste0(export_dir, "LDA/topic", i, "_cloud.png")
  print(paste0("Writing ", file_name))
  
  # Create a term distribution df
  topic.df <- data.frame(term = vocab, p = topics[,i])
  topic.df <- topic.df[order(-topic.df$p),]
  
  # Cut off only the top 50 words
  if (nrow(topic.df) > 50){
    topic.df <- topic.df[1:50,]
  }
  
  
  # Plot the wordclouds
  par(mar = rep(0, 4))
  #png(file_name)
  wordcloud(words = topic.df$term,
            freq = topic.df$p,
            max.words = 50,
            random.order = FALSE,
            rot.per = 0.35,
            colors=brewer.pal(8, "Dark2"),
            scale=c(5,.5))
  #dev.off()
}




############################# Article data ############################# 

### Load the clean equity and article data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename <- paste(clean_dir,"/FT/matched/short_articles.csv", sep="")
text.df <- read.csv(import_filename, stringsAsFactors = FALSE)

text.df <- text.df[sample(1:nrow(text.df), 250, replace = FALSE),]

text.corpus <- Corpus(VectorSource(unlist(text.df[, "clean_text"])))

text.dtm <- DocumentTermMatrix(text.corpus, control = list(minWordLength = 3))
text.vocab <- text.dtm$dimnames$Terms

# Rows are docs and columns are vocab
print(paste("Dimensions of text.dtm are", dim(text.dtm)[1], "documents and", 
            dim(text.dtm)[2], "words in vocab"))
text.dtm$dimnames$Docs <- text.df$obs_id

matrix.dtm <- as.matrix(text.dtm)


### Transform data into correct format for LDA function
V <- ncol(text.dtm) # Vocab length
M <- nrow(text.dtm) # Number of documents
doc_length <- row_sums(text.dtm) # words in each document
N <- sum(doc_length) # Total number of words 
#doc <- text.dtm$i # Document assigned to each word (doesn't work because DTM uses count)

# Get a vector with each word assignment
total_doc <- rep("", M)
w <- rep(NA,N);
doc <- rep(NA,N);

doc_ids <- rep(NA,M)
doc_words <- rep(NA,M)
w <- ""
doc <- ""

total_entries <- as.vector(table(text.dtm$i))
n <- 1;
for (m in 1:M) {
  if (m %% 100 == 0){
    print(paste("Iter", m, "out of", M))
  }
  doc1 <- ""
  w1 <- ""
  for (i in 1:total_entries[m]) {
    
    assertthat::are_equal(text.dtm$i[n], m)
    documents <- str_trim(paste(rep(m, text.dtm$v[n]), collapse = " "))
    words <- str_trim(paste(rep(text.dtm$j[n], text.dtm$v[n]), collapse = " "))
    
    doc1 <- str_trim(paste(doc1, documents, collapse = " "))
    w1 <- str_trim(paste(w1, words, collapse = " "))

    n <- n+1
  }
  doc_ids[m] <- doc1
  doc_words[m] <- w1
  total_doc[m] <- w1
  
  #doc <- paste(doc, doc_ids[m], collapse = " ")
  #w <- paste(w, doc_words[m], collapse = " ")
}
    
doc1 <- str_trim(paste(doc_ids, collapse = " "))
w1 <- str_trim(paste(doc_words, collapse = " "))
doc <- str_split(doc1, " ")[[1]]
w <- str_split(w1, " ")[[1]]

doc <- as.integer(doc)
w <- as.integer(w)


### Priors for the topic model
K <- 5; # topics
alpha <- rep(1/K,K); # Dirichlet prior on document-topic distribution
eta <- rep(1/V,V); # Dirichlet prior on topic distribution


lda_data = list(
  K = K, V = V, M = M, N = N, w = w, doc = doc, alpha = alpha, beta = eta
)
warmup = 200
nsim = 400

start_time <- Sys.time()

fit1 <- stan(
  file = "lda.stan",  # Stan program
  data = lda_data,    # named list of data
  chains = 3,             # number of Markov chains
  warmup = warmup,          # number of warmup iterations per chain
  iter = nsim,            # total number of iterations per chain
  cores = 3,              # number of cores (could use one per chain)
  refresh = 1,             # no progress shown
  algorithm = "NUTS"         # sampling algorithm
)
end_time <- Sys.time()

end_time - start_time

fit_summary <- summary(fit1)
fit_summary$summary["theta[1,1]","mean"]
fit_summary$summary["theta[1,2]","mean"]
theta[1,]

# print(fit1)
print(fit1, pars = c("phi[1,1]", "phi[1,2]", "phi[2,1]", "phi[2,2]"))




### Plot word clouds 
list_of_draws <- extract(fit1)

phi_samples <- list_of_draws$phi[(warmup+1):nsim,,]
topics <- matrix(NA, nrow = V, ncol = K)
for (i in 1:K){
  topicvector <- phi_samples[,i,]
  topicvector <- colMeans(topicvector)
  sum(topicvector)
  topics[,i] <- topicvector
}

text.vocab <- text.dtm$dimnames$Terms
vocab <- text.vocab

# opar <- par()  
for (i in 1:K){
  # Filename for exported graphic
  file_name <- paste0(export_dir, "LDA/topic", i, "_cloud.png")
  print(paste0("Writing ", file_name))
  
  # Create a term distribution df
  topic.df <- data.frame(term = vocab, p = topics[,i])
  topic.df <- topic.df[order(-topic.df$p),]
  
  # Cut off only the top 50 words
  if (nrow(topic.df) > 50){
    topic.df <- topic.df[1:50,]
  }

  # Plot the wordclouds
  par(mar = rep(0, 4))
  #png(file_name)
  wordcloud(words = topic.df$term,
            freq = topic.df$p,
            max.words = 50,
            random.order = FALSE,
            rot.per = 0.35,
            colors=brewer.pal(8, "Dark2"),
            scale=c(5,.5))
  #dev.off()
}



############################# End ############################# 