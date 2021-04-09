setwd("~/Documents/GitHub/Media_volatility")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)
require(lfe)

clean_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/"

# Import the panel data
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
clean_data <- read.csv(import_filename, stringsAsFactors = FALSE)
clean_data <- pdata.frame(clean_data, index = c("Code", "Date"))


clean_data$intra_day <- 100*((clean_data$Close - clean_data$Open)/clean_data$Open)
clean_data$return <- 100*((clean_data$Close - plm::lag(clean_data$Close))/plm::lag(clean_data$Close))

clean_data$abs_return <- abs(clean_data$return)
clean_data$abs_intra_day <- abs(clean_data$intra_day)
clean_data$highlow_1lag <- plm::lag(clean_data$highlow,1)
clean_data$highlow_2lag <- plm::lag(clean_data$highlow,2)
clean_data$highlow_3lag <- plm::lag(clean_data$highlow,3)
clean_data$highlow_4lag <- plm::lag(clean_data$highlow,4)
clean_data$highlow_5lag <- plm::lag(clean_data$highlow,5)

model1 = lm(highlow ~ mention, data = clean_data)
summary(model1)
model2 <- felm(formula = highlow ~ mention | Code, data = clean_data)
summary(model2)
model3 <- felm(formula = highlow ~ mention | Code + Date, data = clean_data)
summary(model3)
model4 <- felm(formula = highlow ~ mention +  plm::lag(highlow, 1:5) | Code + Date, data = clean_data)
summary(model4)
model5 <- felm(formula = highlow ~ mention + plm::lag(highlow, 1:5) + abs_intra_day | Code + Date, data = clean_data)
summary(model5)
model6 <- felm(formula = highlow ~ mention + plm::lag(highlow, 1:5) + abs_intra_day +
                 VI_put + VI_call| Code + Date, data = clean_data)
summary(model6)
model7 <- felm(formula = highlow ~ mention + plm::lag(highlow, 1:5) + abs_intra_day +
                 VI_put + VI_call + IndexHighLow | Code , data = clean_data)
summary(model7)

model8 <- felm(formula = highlow ~ mention + highlow_1lag + highlow_2lag + highlow_3lag + 
                 highlow_4lag + highlow_5lag + abs_intra_day +
                 VI_put + VI_call + IndexHighLow | Code , data = clean_data)
summary(model8)



### Clean up the text 
clean_data$text <- paste(paste(clean_data$headline, clean_data$main_text, sep = ". "))

text_corpus <- Corpus(VectorSource(unlist(clean_data$text)))
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removePunctuation)
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english"))
text_corpus <- tm_map(text_corpus, stemDocument)
text_corpus <- tm_map(text_corpus, stripWhitespace)
clean_data$text_clean <- sapply(text_corpus, as.character)
clean_data$text_clean <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", clean_data$text_clean)
clean_data$text_clean <- str_squish(clean_data$text_clean)


## Export 
clean_data <- clean_data[,c("Date", "Code", "Close", "Open", "Low", "High", "Volume", 
                            "Turnover", "VI_put", "VI_call", "abs_intra_day", 
                            "return", "abs_return", "highlow", "highlow_1lag", 
                            "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag", 
                            "Index_Price", "Index_Change", "Index_High", "Index_Low", 
                            "Index_Open", "Index_abs_Change", "IndexHighLow",
                            "weekday", "Monday", "Tuesday", "Wednesday", "Thursday",
                            "mention", "head_mention", "ner_mention", "text", "text_clean")]
write.csv(booking_subsample,"/Users/julianashwin/Documents/GitHub/BTR.jl/data/booking_semisynth_sample.csv",
          row.names = FALSE)



