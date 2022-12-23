setwd("~/Documents/GitHub/Media_volatility")
rm(list=ls())

library(plm)
library(stringr)
library(stargazer)
library(ggplot2)
library(reshape2)
library(urca)
library(lfe)
library(tm)
library(readxl)
library(SentimentAnalysis)
library(vader)
library(tictoc)


standardise <- function(x){
  x <- (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(x)
}


clean_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/"

# Import the panel data
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
equity_data <- read.csv(import_filename, stringsAsFactors = FALSE)

# Create factors for day and period year and month
equity_data <- equity_data[order(equity_data$Date),]
equity_data$period <- as.numeric(
  factor(equity_data$Date, labels=unique(equity_data$Date), ordered=TRUE))
equity_data$year <- str_sub(equity_data$Date, 1, 4)
equity_data$year_period <- as.numeric(
  factor(equity_data$year, labels=unique(equity_data$year), ordered=TRUE))
cor.test(equity_data$year_period, equity_data$Turnover)
equity_data$month <- as.numeric(str_sub(equity_data$Date, 6, 7))


# Import the sector data
equity_data$NACE_code <- as.numeric(str_replace_all(as.character(equity_data$NACE_code), "/", ""))
equity_data$NACE_industry <- NA

# Get NACE IO industries for the equity data
bridging <- read.csv("/Users/julianashwin/Documents/DPhil/Raw_Data/UK_macro/input-output/Translating_IO_to_NACE.csv", 
                     stringsAsFactors = FALSE)
for (i in 1:nrow(bridging)){
  ind <- bridging[i, "OI_code"]
  upper <- bridging[i, "NAIC_upperbound"]
  lower <- bridging[i, "NAIC_lowerbound"]
  equity_data[which(equity_data$NACE_code >= lower & equity_data$NACE_code <= upper), "NACE_industry"] <- ind
}
equity_data$NACE_industry <- str_replace_all(equity_data$NACE_industry, " ", "_")
equity_data$NACE_industry <- str_replace_all(equity_data$NACE_industry, "\\&", "and")
equity_data$NACE_industry <- str_replace_all(equity_data$NACE_industry, "\\.", "_")
equity_data$NACE_industry <- str_replace_all(equity_data$NACE_industry, "\\,", "_")
equity_data$NACE_industry <- str_replace_all(equity_data$NACE_industry, "-", "to")
equity_data$Sector <- as.character(equity_data$NACE_industry)


"
Import and merge the LSPM data
"
# Bridging table for company codes
company_codes = read.csv(paste0(clean_dir, "FT/company_codes.csv"), stringsAsFactors = F)
company_codes$g1 <- company_codes$LSPM_number
# LSPM data
lspm_df <- read.csv(paste0(clean_dir, "UK_equities/LSPM_data.csv"), stringsAsFactors = F)
names(lspm_df) <- tolower(names(lspm_df))
# Combine to get codes consistent with the equity price data
lspm_short_df <- merge(lspm_df, company_codes[,c("g1", "Code")], by = "g1")
lspm_short_df$Date <- as.Date(as.character(lspm_short_df$p1), format = "%Y%m%d")
lspm_short_df <- lspm_short_df[which(lspm_short_df$Date < "2018-01-01"),]
lspm_short_df <- lspm_short_df[order(lspm_short_df$Code, lspm_short_df$Date),]
# Keep relevant variables
lspm_short_df$name <- lspm_short_df$g33
lspm_short_df$birth_date <- as.Date(as.character(lspm_short_df$g28), format = "%Y%m%d")
lspm_short_df$market_value <- lspm_short_df$a4
lspm_short_df$market_value[which(lspm_short_df$market_value == 0)] <- NA
lspm_short_df$lmarket_value <- log(lspm_short_df$market_value)
lspm_short_df$beta <- lspm_short_df$a5
lspm_short_df$variability <- lspm_short_df$a6
lspm_short_df$spec_risk <- lspm_short_df$a7
lspm_short_df$dividend <- lspm_short_df$a9
# Some lags of lspm variables
lspm_short_df <- pdata.frame(lspm_short_df, index = c("Code", "Date"))
lspm_short_df$market_value_1lag <- plm::lag(lspm_short_df$market_value,1)
lspm_short_df$lmarket_value_1lag <- plm::lag(lspm_short_df$lmarket_value,1)
lspm_short_df$beta_1lag <- plm::lag(lspm_short_df$beta,1)
lspm_short_df$variability_1lag <- plm::lag(lspm_short_df$variability,1)
lspm_short_df$spec_risk_1lag <- plm::lag(lspm_short_df$spec_risk,1)
lspm_short_df$dividend_1lag <- plm::lag(lspm_short_df$dividend,1)
lspm_short_df <- data.frame(lspm_short_df)
# Order Companies into quantiles by size
lspm_short_df$size_quantile <- NA
for (dd in unique(lspm_short_df$Date)){
  qs <- quantile(lspm_short_df$market_value[which(lspm_short_df$Date==dd)], probs = seq(0,1,1/3), na.rm = T)
  lspm_short_df$size_quantile[which(lspm_short_df$Date==dd & lspm_short_df$market_value <= qs[2])] <- "Small"
  lspm_short_df$size_quantile[which(lspm_short_df$Date==dd & 
          lspm_short_df$market_value > qs[2] & lspm_short_df$market_value <= qs[3])] <- "Medium"
  lspm_short_df$size_quantile[which(lspm_short_df$Date==dd & 
          lspm_short_df$market_value > qs[3] & lspm_short_df$market_value <= qs[4])] <- "Large"
}


clean_data <- merge(equity_data, lspm_short_df[,c("Code", "year", "month", "name", "birth_date",
                                "lmarket_value", "market_value", "beta", "variability", "dividend",
                                "spec_risk", "lmarket_value_1lag", "market_value_1lag", "beta_1lag", 
                                "variability_1lag", "dividend_1lag", "spec_risk_1lag", "size_quantile")], 
                    by = c("year", "month", "Code"), all.x = T)
ggplot(as.data.frame(clean_data)) + geom_density(aes(x = dividend))
clean_data$Date <- as.Date(clean_data$Date)
clean_data <- clean_data[order(clean_data$Code, clean_data$Date),]
ggplot(clean_data[which(clean_data$Code == "VOD.L"),]) + theme_bw() +
  geom_line(aes(x = Date, y = dividend/Close))

table(clean_data$size_quantile[which(clean_data$Code == "AAL.L")])

# Use panel structure to get lags and returns
clean_data <- pdata.frame(clean_data, index = c("Code", "period"))

# returns
clean_data$intra_day <- 100*((clean_data$Close - clean_data$Open)/clean_data$Open)
clean_data$return <- 100*((clean_data$Close - plm::lag(clean_data$Close))/plm::lag(clean_data$Close))
clean_data$abs_return <- abs(clean_data$return)
clean_data$abs_intra_day <- abs(clean_data$intra_day)
# highlow lags
clean_data$highlow_1lag <- plm::lag(clean_data$highlow,1)
clean_data$highlow_2lag <- plm::lag(clean_data$highlow,2)
clean_data$highlow_3lag <- plm::lag(clean_data$highlow,3)
clean_data$highlow_4lag <- plm::lag(clean_data$highlow,4)
clean_data$highlow_5lag <- plm::lag(clean_data$highlow,5)
clean_data$highlow_6lag <- plm::lag(clean_data$highlow,6)
clean_data$highlow_7lag <- plm::lag(clean_data$highlow,7)
clean_data$highlow_8lag <- plm::lag(clean_data$highlow,8)
clean_data$highlow_9lag <- plm::lag(clean_data$highlow,9)
clean_data$highlow_10lag <- plm::lag(clean_data$highlow,10)
# volume lags
clean_data$lVolume <- log(1+clean_data$Volume)
clean_data$lVolume_1lag <- plm::lag(clean_data$lVolume,1)
clean_data$lVolume_2lag <- plm::lag(clean_data$lVolume,2)
clean_data$lVolume_3lag <- plm::lag(clean_data$lVolume,3)
clean_data$lVolume_4lag <- plm::lag(clean_data$lVolume,4)
clean_data$lVolume_5lag <- plm::lag(clean_data$lVolume,5)
clean_data$lVolume_6lag <- plm::lag(clean_data$lVolume,6)
clean_data$lVolume_7lag <- plm::lag(clean_data$lVolume,7)
clean_data$lVolume_8lag <- plm::lag(clean_data$lVolume,8)
clean_data$lVolume_9lag <- plm::lag(clean_data$lVolume,9)
clean_data$lVolume_10lag <- plm::lag(clean_data$lVolume,10)
# implied vol lag (put)
clean_data$VI_put_1lag <- plm::lag(clean_data$VI_put,1)
clean_data$VI_put_2lag <- plm::lag(clean_data$VI_put,2)
clean_data$VI_put_3lag <- plm::lag(clean_data$VI_put,3)
clean_data$VI_put_4lag <- plm::lag(clean_data$VI_put,4)
clean_data$VI_put_5lag <- plm::lag(clean_data$VI_put,5)
# implied vol lag (call)
clean_data$VI_call_1lag <- plm::lag(clean_data$VI_call,1)
clean_data$VI_call_2lag <- plm::lag(clean_data$VI_call,2)
clean_data$VI_call_3lag <- plm::lag(clean_data$VI_call,3)
clean_data$VI_call_4lag <- plm::lag(clean_data$VI_call,4)
clean_data$VI_call_5lag <- plm::lag(clean_data$VI_call,5)
# abs_intra lags
clean_data$abs_intra_1lag <- plm::lag(clean_data$abs_intra_day,1)
clean_data$abs_intra_2lag <- plm::lag(clean_data$abs_intra_day,2)
clean_data$abs_intra_3lag <- plm::lag(clean_data$abs_intra_day,3)
clean_data$abs_intra_4lag <- plm::lag(clean_data$abs_intra_day,4)
clean_data$abs_intra_5lag <- plm::lag(clean_data$abs_intra_day,5)
clean_data$abs_intra_6lag <- plm::lag(clean_data$abs_intra_day,6)
clean_data$abs_intra_7lag <- plm::lag(clean_data$abs_intra_day,7)
clean_data$abs_intra_8lag <- plm::lag(clean_data$abs_intra_day,8)
clean_data$abs_intra_9lag <- plm::lag(clean_data$abs_intra_day,9)
clean_data$abs_intra_10lag <- plm::lag(clean_data$abs_intra_day,10)
# abs return lags
clean_data$abs_return_1lag <- plm::lag(clean_data$abs_return,1)
clean_data$abs_return_2lag <- plm::lag(clean_data$abs_return,2)
clean_data$abs_return_3lag <- plm::lag(clean_data$abs_return,3)
clean_data$abs_return_4lag <- plm::lag(clean_data$abs_return,4)
clean_data$abs_return_5lag <- plm::lag(clean_data$abs_return,5)
clean_data$abs_return_6lag <- plm::lag(clean_data$abs_return,6)
clean_data$abs_return_7lag <- plm::lag(clean_data$abs_return,7)
clean_data$abs_return_8lag <- plm::lag(clean_data$abs_return,8)
clean_data$abs_return_9lag <- plm::lag(clean_data$abs_return,9)
clean_data$abs_return_10lag <- plm::lag(clean_data$abs_return,10)
# return lags
clean_data$return_1lag <- plm::lag(clean_data$return,1)
clean_data$return_2lag <- plm::lag(clean_data$return,2)
clean_data$return_3lag <- plm::lag(clean_data$return,3)
clean_data$return_4lag <- plm::lag(clean_data$return,4)
clean_data$return_5lag <- plm::lag(clean_data$return,5)
clean_data$return_6lag <- plm::lag(clean_data$return,6)
clean_data$return_7lag <- plm::lag(clean_data$return,7)
clean_data$return_8lag <- plm::lag(clean_data$return,8)
clean_data$return_9lag <- plm::lag(clean_data$return,9)
clean_data$return_10lag <- plm::lag(clean_data$return,10)
# Overnight return 
clean_data$Close_1lag <- plm::lag(clean_data$Close,1)
clean_data$overnight <- 100*(clean_data$Open-clean_data$Close_1lag)/clean_data$Close_1lag
clean_data$abs_overnight <- abs(clean_data$overnight)

# Sentiment
clean_data <- data.frame(clean_data)
clean_data$text <- paste(paste(clean_data$headline, clean_data$main_text, sep = ". "))
clean_data$text[which(clean_data$text ==  "NA. NA")] <- NA
clean_data$LM_sentiment <- NA
pb = txtProgressBar(min = 1, max = nrow(clean_data), initial = 1) 
for (ii in which(!is.na(clean_data$text))){
  para <- clean_data$text[ii]
  lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
  clean_data$LM_sentiment[ii] <- lm_sentiment[1,1]
  setTxtProgressBar(pb,ii)
}
clean_data$LM_sentiment <- standardise(clean_data$LM_sentiment)
clean_data$LM_sentiment[which(is.na(clean_data$LM_sentiment))] <- 0
tic()
#clean_data$vader_sentiment <- NA
#vader_results <- vader_df(clean_data$text[which(!is.na(clean_data$text))[sample(1:1000,1)]])
#toc()
clean_data <- clean_data[complete.cases(clean_data[,c("highlow", "mention", "lVolume")]),]
clean_data$firm_highlow <- ave(clean_data$highlow, clean_data$Code)
clean_data$day_highlow <- ave(clean_data$highlow, clean_data$Date)
clean_data$firm_lVolume <- ave(clean_data$lVolume, clean_data$Code, FUN = mean)
clean_data$day_lVolume <- ave(clean_data$lVolume, clean_data$Date, FUN = mean)



model1 = lm(highlow ~ mention, data = clean_data)
summary(model1)
direct_model <- felm(formula = highlow ~ mention + 
                       abs_overnight + 
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag + 
                       firm_highlow + day_highlow, data = clean_data)
summary(direct_model)
mediator_model <- felm(formula = lVolume ~ mention + 
                         abs_overnight + 
                         highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
                         lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                         abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag + 
                         VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag + 
                         firm_highlow + day_highlow, data = clean_data)
summary(mediator_model)
both_model <- felm(formula = highlow ~ mention + lVolume + 
                     abs_overnight + 
                     highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
                     lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                     abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag + 
                     VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag + 
                     firm_highlow + day_highlow, data = clean_data)
summary(both_model)
mediator_model$coefficients[2]*both_model$coefficients[2]



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
clean_data$text_clean <- gsub("£", " ", clean_data$text_clean)
clean_data$text_clean <- str_squish(clean_data$text_clean)

clean_data$text[which(clean_data$text== "NA. NA")] <- NA


# Remove Schroeder articles that contain a reference to the chancellor
false_obs = which((clean_data$Code == "SDR.L") & str_detect(clean_data$text_clean, "chancellor"))
clean_data$text[false_obs] <- NA
clean_data$text_clean[false_obs] <- ""
clean_data$mention[false_obs] <- 0  
  
  
names(clean_data)
## Export 
if (FALSE){
  export_cols <- c("Date", "Code", "Company", 
                   "Close", "Open", "Low", "High", "Volume", "lVolume", 
                   "Turnover", "VI_put", "VI_call", "intra_day", "abs_intra_day", 
                   "return", "abs_return", "highlow", "overnight", "abs_overnight",
                   "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag", 
                   "highlow_6lag", "highlow_7lag", "highlow_8lag", "highlow_9lag", "highlow_10lag", 
                   "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag", 
                   "lVolume_6lag", "lVolume_7lag", "lVolume_8lag", "lVolume_9lag", "lVolume_10lag", 
                   "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag", 
                   "VI_call_1lag", "VI_call_2lag", "VI_call_3lag", "VI_call_4lag", "VI_call_5lag", 
                   "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag",
                   "abs_return_6lag", "abs_return_7lag", "abs_return_8lag", "abs_return_9lag", "abs_return_10lag",
                   "abs_intra_1lag", "abs_intra_2lag", "abs_intra_3lag", "abs_intra_4lag", "abs_intra_5lag",
                   "abs_intra_6lag", "abs_intra_7lag", "abs_intra_8lag", "abs_intra_9lag", "abs_intra_10lag",
                   "return_1lag", "return_2lag", "return_3lag", "return_4lag", "return_5lag", 
                   "return_6lag", "return_7lag", "return_8lag", "return_9lag", "return_10lag", 
                   "Index_Price", "Index_Change", "Index_High", "Index_Low", 
                   "Index_Open", "Index_abs_Change", "IndexHighLow",
                   "market_size", "lmarket_size", "beta", "variability", "spec_risk", "dividend",
                   "market_size_1lag", "lmarket_size_1lag", "beta_1lag", "variability_1lag", "spec_risk_1lag", "dividend_1lag",
                   "weekday", "Monday", "Tuesday", "Wednesday", "Thursday",
                   "mention", "head_mention", "ner_mention", "LM_sentiment", "text", "text_clean")
} else {
  export_cols <- names(clean_data)
}

export_cols[which(!(export_cols %in% names(clean_data)))]
export_data <- clean_data[,export_cols]
write.csv(export_data,paste0(clean_dir, "FT/matched/BTR_FT_data.csv"), row.names = FALSE,
          na = "")




