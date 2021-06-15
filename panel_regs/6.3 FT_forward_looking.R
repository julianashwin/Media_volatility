setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())

#dyn.load('/Library/Java/JavaVirtualMachines/jdk-9.0.4.jdk/Contents/Home/jre/lib/server/libjvm.dylib')


require(stringr)
require(tm)
require(tidytext)
require(wordcloud)
require(lfe)
require(plm)
#require(RDPOS)
require(glmmML)
require(pglm)
require(ggplot2)
require(stargazer)

clean_dir <- "~/Documents/DPhil/Clean_Data"
export_dir <- "~/Desktop/"


import_filename = paste(clean_dir, "FT/clean_byyear/FTarticles_2012.csv", sep = "/")
year_data <- read.csv(import_filename, stringsAsFactors = FALSE)


### Load the clean equity and article data 
import_filename = paste(clean_dir, "FT/matched/clean_data_export.csv", sep = "/")
full_data <- read.csv(import_filename, stringsAsFactors = FALSE)
full_data$period <- as.integer(as.factor(full_data$Date))
full_data <- pdata.frame(full_data, index = c("Code", "period"))
full_data$Date <- as.Date(full_data$Date)


import_filename = paste(clean_dir, "FT/matched/LIWC_2015_Results_short_articles.csv", sep = "/")
short_data <- read.csv(import_filename, stringsAsFactors = FALSE)
#write.csv(short_data, file = import_filename, row.names = FALSE)
import_filename = paste(clean_dir, "FT/matched/LIWC2015_Results_clean_equities_articles.csv", sep = "/")
test_data <- read.csv(import_filename, stringsAsFactors = FALSE)


test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)

summary(as.numeric(short_data$focuspast))
summary(as.numeric(short_data$focuspresent))
summary(as.numeric(short_data$focusfuture))
short_data$backward_looking <- short_data$focuspast/(short_data$focuspast + short_data$focuspresent + short_data$focusfuture)
short_data$forward_looking <- short_data$focusfuture/(short_data$focuspast + short_data$focuspresent + short_data$focusfuture)
full_data$backward_looking <- full_data$focuspast/(full_data$focuspast + full_data$focuspresent + full_data$focusfuture)
full_data$forward_looking <- full_data$focusfuture/(full_data$focuspast + full_data$focuspresent + full_data$focusfuture)

full_data[which(is.na(full_data$forward_looking)),"forward_looking"] <- 0
full_data$forward_mention <- as.numeric(full_data$forward_looking > 0)

ggplot(short_data, aes(forward_looking)) + geom_density()


test_data <- merge(full_data, subset(short_data,
                                     select=-c(highlow,article_id, headline)), 
                   by = c("Code", "Date"), all.x = TRUE)
test_data <- pdata.frame(test_data, index = c("Code", "period"))
test_data$focusfuture[which(is.na(test_data$focusfuture))] <- 0
test_data$focuspast[which(is.na(test_data$focuspast))] <- 0

WC_data <- test_data[,c("Code", "Date", "WC")]
full_data <- merge(full_data, WC_data, by = c("Code", "Date"), all.x = TRUE)
full_data <- pdata.frame(full_data, index = c("Code", "period"))
full_data$Date <- as.Date(full_data$Date)


test <- felm(highlow ~ mention + focusfuture + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + forward_mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)


full_data$will_mention <- str_count(full_data$short_text, "will")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "expect")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "set to")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "future")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "might")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "could")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "should")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "uncertain")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "tomorrow")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "today")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "later")
full_data$will_mention <- full_data$will_mention + str_count(full_data$short_text, "soon")
full_data$will_percent <- full_data$will_mention /full_data$WC
full_data$will_mention[which(is.na(full_data$will_mention))] <- 0
full_data$will_percent[which(is.na(full_data$will_percent))] <- 0


full_data$did_mention <- str_count(full_data$short_text, "fell")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "yesterday")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "previous")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "past")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "had")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "explained")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "was")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "were")
#full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "went")
full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "fell")
full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "rose")
full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "increased")
full_data$did_mention <- full_data$did_mention + str_count(full_data$short_text, "decreased")
full_data$did_percent <- full_data$did_mention /full_data$WC
full_data$did_mention[which(is.na(full_data$did_mention))] <- 0
full_data$did_percent[which(is.na(full_data$did_percent))] <- 0

test <- felm(highlow ~ mention + will_percent + did_percent + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)




test <- felm(highlow ~ plm::lag(mention, -1) + plm::lag(focusfuture, -1)  + plm::lag(focuspast, -1) 
             + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + focusfuture  + will_mention + focuspast + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)

res = test$residuals 
n = length(res) 
mod2 = lm(res[-n] ~ res[-1]) 
summary(mod2)



test_data$Mondaymention <- test_data$Monday*test_data$mention
test_data$Mondaywillpercent <- test_data$Monday*test_data$will_percent
test_data$Mondaywill <- test_data$Mondaymention*test_data$will_mention
test_data$Mondayfuture <- test_data$Mondaymention*test_data$focusfuture


test <- felm(highlow ~ mention + Mondaymention + will_percent + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)
test <- felm(highlow ~ mention + Mondaymention + will_mention + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)
test <- felm(highlow ~ mention + Mondaymention + Mondaywill + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)
test <- felm(highlow ~ plm::lag(mention, -1) + plm::lag(Mondaymention, -1) + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)
test <- felm(highlow ~ plm::lag(mention, -1) + plm::lag(Mondaymention, -1)+ plm::lag(Mondaywill, -1)
             + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)

test <- felm(highlow ~ plm::lag(Mondaymention, -1) + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)

test <- felm(highlow ~ Mondaymention + abs_intra_day + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)


test <- felm(highlow ~ Mondaymention + Mondaywill + Mondayfuture + plm::lag(highlow, 1:10) | Code + Date, data = test_data)
summary(test)


test <- felm(pChange ~ mention + plm::lag(posemo, -1) + plm::lag(pChange, 1:10) | Code + Date, data = test_data)
summary(test)




test <- felm(highlow ~ mention| Code + Date, data = short_data)


variables <- c("Code", "Date", "Close", "Open", "Low", "High", "highlow","intra_day","Volume", "Turnover",
              "Industry", "NACE_code", "article_id", "headline", "short_text", "mention", "mention_num", 
              "Monday", "weekday", "Index_Change", "IndexHighLow","VI_put", "VI_call", "positive", 
              "negative", "percent_positive", "focuspast", "focuspresent", "focusfuture", 
              "will_mention", "will_percent", "Mondaymention")
export_data <- test_data[,variables]









 
export_filename <- paste(clean_dir, "/FT/matched/clean_data_export.csv", sep = "")
write.csv(export_data,export_filename, row.names = FALSE)
# full_data <- read.csv(export_filename, stringsAsFactors = FALSE)


clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/full_both.csv", sep = "/")
slda_data <- read.csv(import_filename, stringsAsFactors = FALSE)


slda_data <- slda_data[which(slda_data$Code != "PO_p.L^C06"),]
slda_data$Date <- str_sub(slda_data$obs_id, (str_locate(slda_data$obs_id, "_")[,"end"]+1))



slda_data$period <- as.integer(as.factor(slda_data$Date))
slda_data <- pdata.frame(data.frame(slda_data), index = c("Code", "period"))
slda_data$Date <- as.Date(slda_data$Date)

slda_data$highlow_check <- slda_data$highlow
short_data <- slda_data[,c("Code", "Date", "pred_effect", "highlow_check", 
                           "highlow_1lag" , "highlow_2lag", "highlow_3lag", "highlow_4lag", "sample",
                          "highlow_mean")]

merge_data <- merge(short_data, full_data, by = c("Code", "Date"), all.x = TRUE)
merge_data$period <- as.integer(as.factor(merge_data$Date))
merge_data <- pdata.frame(data.frame(merge_data), index = c("Code", "period"))

merge_data$abs_intra_day <- abs(merge_data$intra_day)

### Verify that any discrepancies are due to rounding errors
View(merge_data[which(merge_data$highlow_check != merge_data$highlow),c("Code", "Date", "highlow_check", "highlow")])

summary(felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = merge_data))


test_data <- merge_data[which(merge_data$sample == "test"),]
train_data <- merge_data[which(merge_data$sample == "train"),]

summary(felm(highlow ~ mention  | Code + Date, data = train_data))
summary(felm(highlow ~ mention + abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag | Code + Date, data = train_data))
summary(felm(highlow ~ mention + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag | Code + Date, data = test_data))
summary(felm(highlow ~ pred_effect + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag | Code + Date, data = test_data))
summary(felm(highlow ~ percent_positive + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag | Code + Date, data = test_data))


summary(felm(highlow ~ mention + plm::lag(highlow, 1:10)| Code + Date , data = test_data))

lm_model <- felm(highlow ~ mention, data = full_data)
summary(lm_model)


plot_data <- data.frame(full_data[which(full_data$mention == 1),])
ggplot(plot_data) + geom_density(aes(focusfuture))
ggplot(plot_data) + geom_density(aes(will_percent))

full_data$focusfuture <- full_data$focusfuture/100
full_data$focuspast <- full_data$focuspast/100



#### Various regressions for RES presentation

test <- felm(highlow ~ focuspast + focuspast + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(test)


model1 <- felm(highlow ~ mention , data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention | Code + Date , data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention + plm::lag(highlow,1) | Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + mention_num | Code + Date , data = full_data)
summary(model4)

stargazer(model1, model2, model3, df = FALSE)


full_data$highlow_sq <- full_data$highlow^2
full_data$highlow_cb <- full_data$highlow^3
full_data$highlow_sqrt <- full_data$highlow^(1/2)
full_data$abs_intra_day <- abs(full_data$intra_day)
full_data$abs_intra_sq <- full_data$abs_intra_day^2
full_data$abs_intra_cb <- full_data$abs_intra_day^3
full_data$abs_intra_sqrt <- full_data$abs_intra_day^(1/2)



### Previous news 

model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(highlow_sq, 1:10)  +
                 plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) | Code + Date, data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(highlow_sq, 1:10)  +
                 plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                 plm::lag(abs_intra_day, 1:10) + plm::lag(abs_intra_sq, 1:10)  +
                 plm::lag(abs_intra_cb, 1:10)  + plm::lag(abs_intra_sqrt, 1:10)| Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + Mondaymention + plm::lag(highlow, 1:10) + plm::lag(highlow_sq, 1:10)  +
                 plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                 plm::lag(abs_intra_day, 1:10) + plm::lag(abs_intra_sq, 1:10)  +
                 plm::lag(abs_intra_cb, 1:10)  + plm::lag(abs_intra_sqrt, 1:10)| Code + Date, data = full_data)
summary(model4)
model5 <- felm(highlow ~ mention + will_percent + + plm::lag(highlow_sq, 1:10)  +
                 plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                 plm::lag(abs_intra_day, 1:10) + plm::lag(abs_intra_sq, 1:10)  +
                 plm::lag(abs_intra_cb, 1:10)  + plm::lag(abs_intra_sqrt, 1:10) | Code + Date, data = full_data)
summary(model5)

model6 <- felm(highlow ~ plm::lag(mention, -1) + 
                 plm::lag(Mondaymention, -1) + plm::lag(will_percent, -1)
                 + plm::lag(highlow_sq, 1:10)  +
                 plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                 plm::lag(abs_intra_day, 1:10) + plm::lag(abs_intra_sq, 1:10)  +
                 plm::lag(abs_intra_cb, 1:10)  + plm::lag(abs_intra_sqrt, 1:10) | Code + Date, data = full_data)
summary(model6)
model7 <- felm(VI_put ~ plm::lag(mention, 0) + 
                 plm::lag(highlow, 1:10) + plm::lag(VI_put, 1:10) + 
                 plm::lag(abs_intra_day, 1:10) | Code + Date, data = full_data)
summary(model7)

stargazer(model1, model2, model3, model4, model5, model6, df = FALSE)



### Forward looking

model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention + focusfuture + focuspast + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention + will_percent + focuspast + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model3)
model4 <- felm(highlow ~ mention + Mondaymention + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model4)
model5 <- felm(highlow ~ mention + plm::lag(mention, -1) + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(model5)

stargazer(model1, model2, model3, model4, model5, df = FALSE, title = "Forward looking", 
          table.placement = "H")










### Breaking news

full_data$abs_intra_day <- abs(full_data$intra_day)
full_data$close_close <- 100*(full_data$Close - plm::lag(full_data$Close))/plm::lag(full_data$Close)
full_data$abs_close_close <- abs(full_data$close_close)
full_data$close_open <- 100*(full_data$Open - plm::lag(full_data$Close))/plm::lag(full_data$Close)
full_data$abs_close_open <- abs(full_data$close_open)

full_data$percent_positive <- full_data$percent_positive/100
full_data$percent_positive <- full_data$percent_positive + 0.3547

# Sentiment
model1 <- felm(intra_day ~ mention + plm::lag(percent_positive, 0) + plm::lag(intra_day, 1:10) + 
                 plm::lag(close_open, 0:10) | Code + Date, data = full_data)
summary(model1)
model2 <- felm(intra_day ~ mention + plm::lag(percent_positive, -1) + plm::lag(intra_day, 1:10) + 
                 plm::lag(close_open, 0:10) | Code + Date, data = full_data)
summary(model2)
model3 <- felm(close_open ~ mention + plm::lag(percent_positive, 0) + plm::lag(intra_day, 0:10) + 
                 plm::lag(close_open, 1:10) | Code + Date, data = full_data)
summary(model3)
model4 <- felm(close_open ~ mention + plm::lag(percent_positive, -1) + plm::lag(intra_day, 0:10) + 
                 plm::lag(close_open, 1:10) | Code + Date, data = full_data)
summary(model4)
model5 <- felm(highlow ~ mention + plm::lag(percent_positive, 0) + plm::lag(highlow, 1:10)  | Code + Date, data = full_data)
summary(model5)
model6 <- felm(highlow ~ mention + plm::lag(percent_positive, -1) + plm::lag(highlow, 1:10)
               | Code + Date, data = full_data)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6, df = FALSE, title = "Sentiment", 
          table.placement = "H")


ggplot(data.frame(full_data[which(full_data$mention == 1),]), aes(percent_positive)) + geom_density() +
  labs(y = "Kerne density", x = expression(sentiment[" i,t"]))
export_filename = paste(export_dir, "sentiment_density.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)


# Control for c-o and o-c returns
model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = full_data)
summary(model1)
model2 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + 
                 plm::lag(abs_intra_day, 0:10) | Code + Date, data = full_data)
summary(model2)
model3 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + 
                 plm::lag(abs_intra_day, 0:10) + 
                 plm::lag(abs_close_open, 0:10) | Code + Date, data = full_data)
summary(model3)

# Control for implied volatility
full_data$VI_put_sqrt <- full_data$VI_put^(1/2)
full_data$VI_put_sq <- full_data$VI_put^2
full_data$VI_put_cb <- full_data$VI_put^3
full_data$VI_put_qrt <- full_data$VI_put^4
full_data$VI_call_sqrt <- full_data$VI_call^(1/2)
full_data$VI_call_sq <- full_data$VI_call^2
full_data$VI_call_cb <- full_data$VI_call^3
full_data$VI_call_qrt <- full_data$VI_call^4


model4 <- felm(highlow ~ mention +
                 plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10) +
                 plm::lag(highlow, 1:10) + 
                 plm::lag(abs_intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(model4)
model5 <- felm(highlow ~ mention +
                 plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10) +
                 plm::lag(VI_put_sqrt, 1:10) + plm::lag(VI_call_sqrt, 1:10) +
                 plm::lag(VI_put_sq, 1:10) + plm::lag(VI_call_sq, 1:10) +
                 plm::lag(VI_put_cb, 1:10) + plm::lag(VI_call_cb, 1:10) +
                 plm::lag(VI_put_qrt, 1:10) + plm::lag(VI_call_qrt, 1:10) +
                 plm::lag(highlow, 1:10) + 
                 plm::lag(abs_intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(model5)
model6 <- felm(highlow ~ mention +
                 plm::lag(VI_put, 0:10) + plm::lag(VI_call, 0:10) +
                 plm::lag(highlow, 1:10) + 
                 plm::lag(abs_intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(model6)


stargazer(model1, model2, model3, model4, model5, model6, df = FALSE,
          title = "Neither realised nor expected new information explain the media coverage effect", 
          table.placement = "H")


### Out of sample fit

noeffect_model <- felm(highlow ~ abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag +
                       highlow_mean + IndexHighLow, data = test_data)
mention_model <- felm(highlow ~ mention + abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag +
                        highlow_mean + IndexHighLow, data = test_data)
sentiment_model <- felm(highlow ~ percent_positive + abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag+
                          highlow_mean + IndexHighLow, data = test_data)
future_model <- felm(highlow ~ focusfuture + abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag +
                       highlow_mean + IndexHighLow, data = test_data)
predeffect_model <- felm(highlow ~ pred_effect + abs_intra_day + VI_put + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag +
                           highlow_mean + IndexHighLow, data = test_data)


summary(noeffect_model)$r.squared
summary(mention_model)$r.squared
summary(sentiment_model)$r.squared
summary(future_model)$r.squared
summary(predeffect_model)$r.squared


stargazer(noeffect_model, mention_model, sentiment_model, future_model, predeffect_model, df = TRUE,
          title = "sLDA model performs better out of sample than mention dummy, sentiment or future focus", 
          table.placement = "H")






import_filename = paste(clean_dir, "FT/clean_byyear/FTarticles_2012.csv", sep = "/")
year_data <- read.csv(import_filename, stringsAsFactors = FALSE)


import_filename = paste(clean_dir, "FT/14topic_model/output/beta_estimates.csv", sep = "/")
beta_estimates <- read.csv(import_filename, stringsAsFactors = FALSE)
import_filename = paste(clean_dir, "FT/14topic_model/output/text_vocab.csv", sep = "/")
text.vocab <- read.csv(import_filename, stringsAsFactors = FALSE)
K <- 14
beta_estimates <- as.matrix(beta_estimates)
text.vocab <- as.matrix(text.vocab)


export_dir <- "~/Documents/DPhil/Firm_level_news/second_draft/figures/14_topic_slda/"

for (i in 1:K){
  # Filename for exported graphic
  file_name <- paste0(export_dir, "topic", i, "_cloud.png")
  print(paste0("Writing ", file_name))
  
  # Create a term distribution df
  topic.df <- data.frame(term = text.vocab, p = beta_estimates[i,])
  topic.df$term <- text.vocab
  topic.df <- topic.df[order(-topic.df$p),]
  
  # Cut off only the top 50 words
  if (nrow(topic.df) > 50){
    topic.df <- topic.df[1:50,]
  }
  
  # Plot the wordclouds
  par(mar = rep(0.5, 4))
  png(file_name)
  wordcloud(words = topic.df$term,
            freq = topic.df$p,
            max.words = 50,
            random.order = FALSE,
            rot.per = 0.35, colors = "blue",vfont=c("serif","plain"),
            #colors= "blue", # brewer.pal(3, "Dark2"),
            scale=c(4,.2))
  dev.off()
}










RBS_data <- full_data[full_data$Code == "RBS.L",]
RBS_data$Date <- as.Date(RBS_data$Date)
RBS_data <- RBS_data[which(RBS_data$Date >= "2008-10-01" & RBS_data$Date < "2009-02-01"),]
ggplot(RBS_data, aes(x = Date)) + geom_line(aes(y = VI_put)) + geom_line(aes(y = highlow/100))




### Volume
model5 <- felm(Volume ~ mention +
                 plm::lag(VI_put, 0:10) + plm::lag(VI_call, 0:10) + plm::lag(Volume, 1:10) +
                 plm::lag(highlow, 0:10) + 
                 plm::lag(abs_intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(model5)
model6 <- felm(Turnover ~ mention +
                 plm::lag(VI_put, 0:10) + plm::lag(VI_call, 0:10) + plm::lag(Turnover, 1:10) +
                 plm::lag(highlow, 0:10) + 
                 plm::lag(abs_intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(model6)

stargazer(model5, model6, df = FALSE)


### Anticipating news

model1 <- felm(highlow ~ plm::lag(mention, 0) + plm::lag(highlow, 1:10) + plm::lag(intra_day, 0:10)
                 plm::lag(abs_close_open, 0:10) + plm::lag(abs_intra_day, 0:10) | Code + Date, data = full_data)
summary(model1)

model2 <- felm(highlow ~ plm::lag(will_percent, 0) + plm::lag(highlow, 1:10) + 
                 plm::lag(abs_close_close, 0:10) | Code + Date, data = full_data)
summary(model2)

model3 <- felm(highlow ~ plm::lag(Mondaymention, 0) + plm::lag(highlow, 1:10) + 
                 plm::lag(abs_close_close, 0:10) + VI_put + VI_call| Code + Date, data = full_data)
summary(model3)





### Volume and turnover



length(unique(full_data[which(!is.na(full_data$VI_call)),"Code"]))
length(unique(full_data[which(is.na(full_data$VI_call)),"Code"]))

model1 <- felm(highlow ~ mention + plm::lag(VI_put, 0:10) + plm::lag(VI_call, 0:10) +
                 plm::lag(highlow, 1:10) + plm::lag(intra_day, 0:10) +
                 plm::lag(abs_close_open, 0:10) + plm::lag(abs_intra_day, 0:10) | Code + Date, data = full_data)
summary(model1)






deathmodel <- felm(highlow ~ mention +  plm::lag(highlow, 1:10) + 
                               plm::lag(abs_close_close, 0:10) + 
                               plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(deathmodel)
deathmodel <- felm(highlow ~ mention_num +  plm::lag(highlow, 1:10) + 
                     plm::lag(abs_close_close, 0:10) + 
                     plm::lag(abs_close_open, 0:10)| Code + Date, data = full_data)
summary(deathmodel)



model <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(highlow_sq, 1:10)  +
                     plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                     plm::lag(abs_intra_day, 0:10) + plm::lag(abs_intra_sq, 0:10)  +
                     plm::lag(abs_intra_cb, 0:10)  + plm::lag(abs_intra_sqrt, 0:10) + 
                     plm::lag(abs_close_open, 0:10) + plm::lag(VI_put, 0:10) + 
                     plm::lag(VI_call, 0:10)| Code + Date, data = full_data)
summary(model)

finalmodel <- felm(highlow ~ Mondaymention + plm::lag(highlow, 1:10) + plm::lag(highlow_sq, 1:10)  +
                     plm::lag(highlow_cb, 1:10)  + plm::lag(highlow_sqrt, 1:10) + 
                     plm::lag(abs_intra_day, 0:10) + plm::lag(abs_intra_sq, 0:10)  +
                     plm::lag(abs_intra_cb, 0:10)  + plm::lag(abs_intra_sqrt, 0:10) + 
                     plm::lag(abs_close_open, 0:10) + plm::lag(VI_put, 1:10) + 
                     plm::lag(VI_call, 1:10)| Code + Date, data = full_data)
summary(finalmodel)








library(pglm)



### Get an "expected coverage" variable
install.packages("glmmML")


mentionprobit <- glm(mention ~ plm::lag(highlow, 1), 
                  family = binomial(link = "probit"), 
                  data = full_data)
summary(mentionprobit)

full_data$mention_resid <- mentionprobit$residuals
mentionlogit <- glm(mention ~ plm::lag(highlow, 1) + plm::lag(highlow, 2) + plm::lag(pChange), 
                 family = binomial(link = "logit"), 
                 data = full_data)
summary(mentionlogit)


