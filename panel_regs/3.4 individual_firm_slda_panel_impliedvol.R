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
clean_dir <- "~/Documents/DPhil/Clean_Data/"
import_filename = paste0(clean_dir, "FT/matched/clean_equities_articles_slda_topics_shocks.csv")
price_data <- read.csv(import_filename, stringsAsFactors = FALSE)
price_data$Date <- as.Date(price_data$Date)

# Import the implied volatility data
clean_filename = paste(clean_dir, "UK_equities/VI_series_full.csv", sep = "/")
VI_data <- read.csv(clean_filename, fileEncoding = "utf-8", stringsAsFactors = FALSE)
VI_data$Date <- as.Date(VI_data$Date)

### Need to merge by Date and Code 
price_codes <- price_data[,c("Code", "Company")]
price_codes$Code_str <- price_codes$Code
price_codes <- unique(price_codes[,c("Code", "Code_str")])
VI_codes <- unique(VI_data$Code)

# Remove everything after "^" in price_data$Code_str
for (i in 1:nrow(price_codes)){
  temp.loc <- as.data.frame(str_locate_all(price_codes$Code_str[i], "\\^"))
  if (nrow(temp.loc) > 0 ){
    code <- price_codes$Code_str[i]
    print(code)
    code <- str_sub(code, 1, (temp.loc[1,"start"] - 1))
    price_codes$Code_str[i] <- code
  }
}

# Need to deal with duplicates on a case-by-case basis
table(price_codes[which(duplicated(price_codes$Code_str)),])

# For CCH.L, the correct is CCH.L not CCH.L^H04 to match the VI (coca-cola not celltech)
price_codes[which(price_codes$Code == "CCH.L^H04"),"Code_str"] <- "CCH.LH04"
# EXL.L doesn't appear in the VI data
price_codes[which(price_codes$Code == "EXL.L^L05"),"Code_str"] <- "EXL.LL05"
# TW.L and TW.LB00 both have options data 
price_codes[which(price_codes$Code == "TW.L^B00"),"Code_str"] <- "TW.LB00"

table(price_codes[which(duplicated(price_codes$Code_str)),])

price_data <- merge(price_data, price_codes, by = "Code", all.x = TRUE)
price_data$Date <- as.Date(price_data$Date)


### Now merge by the "Code_str" variable rather than Code
VI_data$Code_str <- VI_data$Code
VI_data <- subset(VI_data, select=-c(Code))

table(duplicated(price_data[,c("Date", "Code")]))
table(duplicated(price_data[,c("Date", "Code_str")]))
table(duplicated(VI_data[,c("Date", "Code_str")]))


all_data <- merge(price_data, VI_data, by =c ("Date", "Code_str"), all.x = TRUE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))
all_data <- all_data[-which(all_data$weekday == "Saturday" | all_data$weekday == "Sunday"),]
all_data <- all_data[,c(1,3:46, 119, 120)]

# export_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
# write.csv(all_data, file = export_filename, row.names = FALSE)


### Also include the VFTSE index for the entire FTSE index
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



all_data$highlow_sq <- (all_data$highlow)^2
all_data$VI_call_sq <- (all_data$VI_call)^2
all_data$VI_put_sq <- (all_data$VI_put)^2
all_data$VIX_Open_sq <- (all_data$VIX_Open)^2

### Now add this in as a control
short_data <- all_data[which(!is.na(all_data$VI_put)),]

model <- felm(highlow ~ mention, data = all_data)
model <- felm(highlow ~ VI_put + VI_call, data = all_data)
summary(model)
model1 <- felm(highlow ~ mention + VI_put + VI_call| Code, data = all_data)
model2 <- felm(highlow ~ mention + VI_put + VI_call + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model3 <- felm(highlow ~ mention + VI_put + VI_call + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) + VI_put_sq + VI_call_sq | Code + Date, data = all_data)
model4 <- felm(highlow ~ mention + VI_put + VI_call + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) + plm::lag(VI_put_sq, 0:10) + plm::lag(VI_call_sq, 0:10) 
               + plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10)| 
                 Code + Date, data = all_data)
model5 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) 
                | Code + Date, data = all_data)
model6 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) 
                + plm::lag(highlow_sq, 1:10) + plm::lag(VI_put_sq, 0:10) + plm::lag(VI_call_sq, 0:10) 
                + plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10) + plm::lag(abs_intra_day, 1:10) | 
                  Code + Date, data = all_data)

stargazer(model1, model2, model3, model4, model5, model6, 
          table.placement = "H", df = FALSE, title = "Mention effect and implied volatility controls")




### Now with the sLDA shock

model <- felm(highlow ~ VI_put + VI_call, data = all_data)
summary(model)
model1 <- felm(highlow ~ highlow_control_shock_phi + VI_put + VI_call| Code, data = all_data)
model2 <- felm(highlow ~ highlow_control_shock_phi + VI_put + VI_call + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
model3 <- felm(highlow ~ highlow_control_shock_phi + VI_put + VI_call + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) + VI_put_sq + VI_call_sq | Code + Date, data = all_data)
model4 <- felm(highlow ~ highlow_control_shock_phi + VI_put + VI_call + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) + plm::lag(VI_put_sq, 0:10) + plm::lag(VI_call_sq, 0:10) 
               + plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10)| 
                 Code + Date, data = all_data)
model5 <- felm(highlow ~ highlow_control_shock_phi + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) 
               | Code + Date, data = all_data)
model6 <- felm(highlow ~ highlow_control_shock_phi + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) 
               + plm::lag(highlow_sq, 1:10) + plm::lag(VI_put_sq, 0:10) + plm::lag(VI_call_sq, 0:10) 
               + plm::lag(VI_put, 1:10) + plm::lag(VI_call, 1:10) + plm::lag(abs_intra_day, 1:10) | 
                 Code + Date, data = all_data)

stargazer(model1, model2, model3, model4, model5, model6, 
          table.placement = "H", df = FALSE, title = "Article effect and implied volatility controls")


############################# End ############################# 