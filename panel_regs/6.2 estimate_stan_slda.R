setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())

require(rstan)
require(gtools)
require(shinystan)
require(stringr)
require(stargazer)
require(ggplot2)




### Test case with simulated data

V <- 5; # words: river, stream, bank, money, loan
K <- 2; # topics: RIVER, BANK

beta <- array(NA,c(K,V));
beta[1,] = c(0.330, 0.330, 0.330, 0.005, 0.005);
beta[2,] = c(0.005, 0.005, 0.330, 0.330, 0.330);

M <- 100;  # docs
avg_doc_length <- 30;
doc_length <- rpois(M,avg_doc_length);
N <- sum(doc_length);

alpha <- rep(1/K,K);
eta <- rep(1/V,V);

theta <- rdirichlet(M,alpha);

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
}


lda_data = list(
  K = K, V = V, M = M, N = N, w = w, doc = doc, alpha = alpha, beta = eta
)

start_time <- Sys.time()
fit1 <- stan(
  file = "lda.stan",  # Stan program
  data = lda_data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 150,          # number of warmup iterations per chain
  iter = 300,            # total number of iterations per chain
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


### Load the clean equity and article data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename <- paste(clean_dir,"/FT/matched/short_articles.csv", sep="")
text.df <- read.csv(import_filename, stringsAsFactors = FALSE)

text.df <- text.df[1:100,]

text.corpus <- Corpus(VectorSource(unlist(text.df[, "clean_text"])))

text.dtm <- DocumentTermMatrix(text.corpus, control = list(minWordLength = 3))
vocab <- text.dtm$dimnames$Terms

# Rows are docs and columns are vocab
print(paste("Dimensions of text.dtm are", dim(text.dtm)[1], "documents and", 
            dim(text.dtm)[2], "words in vocab"))
text.dtm$dimnames$Docs <- text.df$obs_id

matrix.dtm <- as.matrix(text.dtm)

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



K <- 2; # topics: RIVER, BANK
alpha <- rep(1/K,K); # Dirichlet prior on document-topic distribution
eta <- rep(1/V,V); # Dirichlet prior on topic distribution


beta <- array(NA,c(K,V));

beta[1,1:(V/2)] <- 1.8/V
beta[1,(V/2+1):V] <- 0.2/V
beta[2,1:(V/2)] <- 0.2/V
beta[2,(V/2+1):V] <- 1.8/V
sum(beta)

# docs
avg_doc_length <- 30;
doc_length <- rpois(M,avg_doc_length);
N <- sum(doc_length);


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

num5 <- str_count(total_doc, "5")
num5 <- num5 - mean(num5)

phi <- c(5,-5)
x <- as.matrix(rnorm(M, 0, 1), nrow = M)
y <- theta%*%phi + -2*x + num5 + rnorm(M, 0, 1)
y <- as.vector(y)
mean(y)
lm(y ~ theta[,1] + theta[,2] + x + num5 - 1)
lm(y ~ theta[,1] + theta[,2] + x - 1)

phiprior = c(1,-1)
sigma = 1


cov <- cbind(x,num5)
J = ncol(cov)

lda_data = list(
  K = K, J = J, V = V, M = M, N = N, w = w, doc = doc, alpha = alpha, eta = eta, 
  y = y, phiprior = 0, x= cov, gammaprior = 0, sigma = sigma, sigmaphi = 5
)




