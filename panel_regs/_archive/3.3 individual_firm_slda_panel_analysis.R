setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)
require(lfe)

# Import the panel data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_topics_shocks.csv", sep = "/")
all_data <- read.csv(import_filename, stringsAsFactors = FALSE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))


### Eventually insert all of the mention dummy etc regressions here to make sure they are consistent.
test <- felm(abs_intra_day ~ T1_abs + plm::lag(abs_open_open,0:10)| Code + Date , data = all_data)
summary(test)

###########################


### Past values of VIX
vix_data <- read.csv("~/Documents/DPhil/Raw_Data/UK_macro/FTSE_100_VIX_Historical_Data.csv", stringsAsFactors = FALSE)
day <- str_sub(vix_data$Date, 5,6)
month <- str_sub(vix_data$Date, 1,3)
year <- str_sub(vix_data$Date, 9,12)

date <- paste

vix_archdata <- read.csv("~/Documents/DPhil/Raw_Data/UK_macro/vixarchive.csv", stringsAsFactors = FALSE)
colnames(vix_archdata) <- vix_archdata[1,]
colnames(vix_archdata) <- str_replace(colnames(vix_archdata), " ", "_")
vix_archdata <- vix_archdata[2:nrow(vix_archdata),]

vix_archdata <- vix_archdata[which(vix_archdata$Date != ""),]
date <- strptime(as.character(vix_archdata$Date), "%m/%d/%y")
date <- format(date, "%Y-%m-%d")
vix_archdata$Date <- date
vix_archdata[which(vix_archdata$VIX_Open == "n/a"), "VIX_Open"] <- NA
vix_archdata[which(vix_archdata$VIX_High == "n/a"), "VIX_High"] <- NA
vix_archdata[which(vix_archdata$VIX_Low == "n/a"), "VIX_Low"] <- NA
vix_archdata$VIX_Close <- as.numeric(vix_archdata$VIX_Close)
vix_archdata$VIX_High <- as.numeric(vix_archdata$VIX_High)
vix_archdata$VIX_Open <- as.numeric(vix_archdata$VIX_Open)
vix_archdata$VIX_Low <- as.numeric(vix_archdata$VIX_Low)



vix_data <- read.csv("~/Documents/DPhil/Raw_Data/UK_macro/vixcurrent.csv", stringsAsFactors = FALSE)
colnames(vix_data) <- vix_data[1,]
colnames(vix_data) <- str_replace(colnames(vix_data), " ", "_")
vix_data <- vix_data[2:nrow(vix_data),]

date <- strptime(as.character(vix_data$Date), "%m/%d/%Y")
date <- format(date, "%Y-%m-%d")
                               
vix_data$Date <- date
vix_data$VIX_Close <- as.numeric(vix_data$VIX_Close)
vix_data$VIX_High <- as.numeric(vix_data$VIX_High)
vix_data$VIX_Open <- as.numeric(vix_data$VIX_Open)
vix_data$VIX_Low <- as.numeric(vix_data$VIX_Low)

vix_data <- rbind(vix_archdata, vix_data)






vix_data <- read.csv("~/Documents/DPhil/Raw_Data/UK_macro/FTSE100_VOLATINDX_Price_Data_Historical_2019-01-27_20-06.csv", 
                     stringsAsFactors = FALSE, skip = 3)

date <- strptime(as.character(vix_data$Date), "%d/%m/%Y")
date <- format(date, "%Y-%m-%d")

vix_data$Date <- date
vix_data$VIX_Close <- as.numeric(vix_data$Close)
vix_data$VIX_High <- as.numeric(vix_data$High)
vix_data$VIX_Open <- as.numeric(vix_data$Open)
vix_data$VIX_Low <- as.numeric(vix_data$Low)

vix_data <- vix_data[,c("Date", "VIX_Close", "VIX_High", "VIX_Open", "VIX_Low" )]

vix_data$Date <- as.character(vix_data$Date)
all_data$Date <- as.character(all_data$Date)
all_data$Code <- as.character(all_data$Code)

all_data <- merge(all_data, vix_data, by = "Date", all.x = TRUE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))

model <- felm(highlow ~ (VIX_Open^2), data = all_data)
model1 <- felm(highlow ~ mention | Code, data = all_data)
model2 <- felm(highlow ~ mention | Code + Date, data = all_data)
model3 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
model4 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(VIX_Open, 0:10) | Code, data = all_data)
model5 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
model5b <- felm(highlow ~ mention + plm::lag(highlow, 1:10) 
                + plm::lag(highlow_sq, 1:10) | Code + Date, data = all_data)
model6 <- felm(highlow ~ plm::lag(mention, -1) + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) | Code + Date, data = all_data)


stargazer(model, model1, model2, model3, model4, model5b, model6, 
          table.placement = "H", df = FALSE)

model <- felm(highlow ~ highlow_control_shock_phi + plm::lag(highlow, 1:10) | Code + Date, data = all_data)

stargazer(model5, model, table.placement = "H", df = FALSE)

modelner <- lm(highlow ~ ner_mention, data = all_data)
modelhead <- lm(highlow ~ head_mention, data = all_data)
modelboth <- lm(highlow ~ mention, data = all_data)

stargazer(modelner, modelhead, modelboth, 
          table.placement = "H", df = FALSE)


model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model2 <- felm(highlow ~ highlow_control_shock_phi + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
stargazer(model1, model2, 
          table.placement = "H", df = FALSE)

model1 <- felm(abs_intra_day ~ mention + plm::lag(abs_intra_day, 1:10)
                 + plm::lag(highlow, 0:10)| Code + Date, data = all_data)
model2 <- felm(abs_intra_day ~ mention #+ plm::lag(abs_intra_day, 1:10)
               + plm::lag(highlow, 0:10)| Code + Date, data = all_data)





firm_mentions <- aggregate(clean_data[,c("mention")], by = list(clean_data$Code, clean_data$Company), sum)
firm_mentions <- firm_mentions[order(-firm_mentions$x),]


ggplot(firm_mentions, aes(x=x)) + geom_histogram()

model1 <- felm(highlow ~ highlow_control_shock_phi + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model2 <- felm(highlow ~ plm::lag(abs_intra_day, 0) + highlow_control_shock_phi 
               + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model3 <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
               + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
model4 <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
               + plm::lag(highlow, 1:10) + plm::lag(VIX_Open, 0:10)| Code, data = all_data)
summary(model4)

stargazer(model1, model2, model3, model4, table.placement = "H", df = FALSE, 
          title = "Media volatility effect controlling for absolute return")


all_data$abs_intra_day_sq <- all_data$abs_intra_day^2
all_data$highlow_sq <- all_data$highlow^2

model1 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10)| Code + Date, data = all_data)


model1 <- felm(highlow ~ highlow_control_shock_phi + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model2 <- felm(highlow ~ plm::lag(abs_intra_day, 0) + highlow_control_shock_phi 
               + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model3 <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
               + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
model4a <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
                + plm::lag(highlow, 1:10) 
                + plm::lag(abs_intra_day_sq, 0:10)| Code + Date, data = all_data)
model4b <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
                + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day_sq, 0:10) 
                + plm::lag(VIX_Open, 0:10)| Code, data = all_data)

stargazer(model1, model2, model3, model4a, model4b,
          table.placement = "H", df = FALSE, 
          title = "Media volatility effect controlling for absolute return")

model4b <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + mention 
                + plm::lag(highlow, 1:10) + plm::lag(abs_intra_day_sq, 0:10) 
                + plm::lag(VIX_Open, 0:10)| Code, data = all_data)


model4c <- felm(highlow ~ plm::lag(abs_intra_day, 0:10) + highlow_control_shock_phi 
                + plm::lag(highlow, 1:10) + VIX_Open
                + plm::lag(abs_intra_day_sq, 0:10)| Code , data = all_data)
stargazer(model1, model2, model3, model4a, model4b, model4c, table.placement = "H", df = FALSE, 
          title = "Media volatility effect controlling for VIX index")