setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())

require(stringr)
require(tm)
require(tidytext)
require(wordcloud)
require(lfe)
require(plm)
require(RDPOS)
require(glmmML)
require(pglm)

# Directories
clean_dir <- "~/Documents/DPhil/Clean_Data"
export_dir <- "~/Desktop/"

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_data_export.csv", sep = "/")
full_data <- read.csv(import_filename, stringsAsFactors = FALSE)
full_data$Date <- as.Date(full_data$Date)
full_data$Code <- str_replace_all(full_data$Code, "\\^", "_")

full_data <- pdata.frame(full_data, index = c("Code", "Date"))

full_data$abs_intra_day <- abs((full_data$Close - full_data$Open)/full_data$Open)


mention_data <- full_data[,c("Code", "Date", "highlow", "mention", "VI_put")]
mention_data <- mention_data[!is.na(mention_data$VI_put),]
mention_data$Date <- as.Date(mention_data$Date)
#mention_data <- mention_data[which(mention_data$Date <= "2003-01-01"),]




test <- felm(highlow ~ mention + plm::lag(abs_intra_day, 0:10) 
             + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(test)
test <- felm(highlow ~ mention + plm::lag(abs_intra_day, 0:10) + 
             + plm::lag(highlow, 1:10)| Code + Date, data = full_data)
summary(test)

test <- felm(plm::lag(VI_put,0) ~ Mondaymention + plm::lag(VI_put, 1:10)| Code + Date, data = full_data)
summary(test)


test <- felm(Volume ~ mention + VI_put + plm::lag(Volume, 1:10)| Code + Date, data = full_data)
summary(test)
test <- felm(Volume ~ mention + plm::lag(VI_put, 0:30) + plm::lag(Volume, 1:30)| Code + Date, data = full_data)
summary(test)


full_data$highlow_sq <- full_data$highlow^2
full_data$highlow_sqrt <- full_data$highlow^(1/2)
full_data$highlow_cb <- full_data$highlow^3
full_data$highlow_ln <- log(full_data$highlow)

test <- felm(highlow ~ mention + 
               plm::lag(highlow, 1:10) + plm::lag(highlow_sqrt, 1:10) + plm::lag(highlow_sq, 1:10) + 
               plm::lag(highlow_cb, 1:10) + plm::lag(highlow_ln, 1:10) + plm::lag(Turnover, 1:10) + 
               plm::lag(VI_put, 1:10) + plm::lag(Volume, 1:10) +    
               plm::lag(abs_intra_day, 1:10)| Code + Date, data = full_data)
summary(test)

test <- felm(highlow ~ mention + 
               plm::lag(highlow, 1:10) + plm::lag(Turnover, 1:10) + 
               plm::lag(VI_put, 1:10) + plm::lag(Volume, 1:10) +    
               plm::lag(abs_intra_day, 1:10)| Code + Date, data = full_data)
summary(test)


test <- felm(Volume ~ mention + plm::lag(VI_put, 0:30) + plm::lag(Volume, 1:30) + 
               plm::lag(highlow, 0:10)| Code + Date, data = full_data)
summary(test)


test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(VI_put, 0:10) + plm::lag(Volume, 1:10) + 
               plm::lag(abs_intra_day, 0:10)| Code + Date, data = full_data)
summary(test)

test <- felm(highlow ~ mention + plm::lag(highlow, 1:10)+ plm::lag(VI_put, 1:10) + 
               plm::lag(Volume, 1:10) + plm::lag(abs_intra_day, 1:10)| Code + Date, data = full_data)
summary(test)

test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(VI_put, 1:10) + 
               plm::lag(Volume, 1:10) + plm::lag(abs_intra_day, 0:10)| Code + Date, data = full_data)
summary(test)

test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + plm::lag(VI_put, 1:10) + 
               plm::lag(Volume, 0:10) + plm::lag(abs_intra_day, 0:10)| Code + Date, data = full_data)
summary(test)

test <- felm(highlow ~ mention + plm::lag(highlow, 1:10)  + plm::lag(VI_put, 1:10) + 
               plm::lag(Volume, 0:10) + plm::lag(Turnover, 0:10) +
               plm::lag(abs_intra_day, 0:10)| Code + Date, data = full_data)
summary(test)




### Spread back to get dispersion, fed and news measures for each series
sector.panel <- spread(mention_data, Code, mention)
sector.panel <- select(sector.panel, -VI_put)

mention.panel <- sector.panel %>%
  group_by(Date) %>% 
  summarise_all(funs(mean), na.rm = TRUE)

mention.panel$AAL.L

mention.panel[is.na(mention.panel)] <- 0


merge.df <- merge(full_data, mention.panel, by = "Date", all.x = TRUE)
merge.df <- pdata.frame(merge.df, index = c("Code", "Date"))

test <- felm(highlow ~ mention + plm::lag(highlow, 1:10)| Code + Date, data = merge.df)
summary(test)

test <- felm(highlow ~ ABF.L | Code + Date, data = merge.df)
summary(test)


command <- paste0("test <- felm(highlow ~", paste(colnames(merge.df)[33:185], collapse = " + "), ", data = merge.df)")
eval(parse(text=command))




