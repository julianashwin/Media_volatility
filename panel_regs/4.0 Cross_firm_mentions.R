setwd("~/Documents/GitHub/Media_volatility")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)
library(dplyr)



# Import the NACE bridging table
bridging <- read.csv("/Users/julianashwin/Documents/DPhil/Raw_Data/UK_macro/input-output/Translating_IO_to_NACE.csv", 
                     stringsAsFactors = FALSE)

# Import the equity data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/BTR_FT_data.csv", sep = "/")
all_data <- read.csv(import_filename, stringsAsFactors = FALSE)


# Import the slda topics csv as this includes the NACE codes we need
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_topics_shocks.csv", sep = "/")
nace_codes <- read.csv(import_filename, stringsAsFactors = FALSE)

nace_codes <- nace_codes[,c("Code", "NACE_code", "NACE_description")]
nace_codes <- unique(nace_codes)

all_data <- merge(all_data, nace_codes, by = "Code", all.x = TRUE)


all_data <- pdata.frame(all_data, index = c("Code", "Date"))
# Classify sectors as they are in the ONS IO tables
all_data$NACE_code <- as.numeric(str_replace_all(as.character(all_data$NACE_code), "/", ""))
all_data$NACE_industry <- NA

# Get NACE IO industries for the equity data
for (i in 1:nrow(bridging)){
  ind <- bridging[i, "OI_code"]
  upper <- bridging[i, "NAIC_upperbound"]
  lower <- bridging[i, "NAIC_lowerbound"]
  all_data[which(all_data$NACE_code >= lower & all_data$NACE_code <= upper), "NACE_industry"] <- ind
  
}

all_data$NACE_industry <- str_replace_all(all_data$NACE_industry, " ", "_")
all_data$NACE_industry <- str_replace_all(all_data$NACE_industry, "\\&", "and")
all_data$NACE_industry <- str_replace_all(all_data$NACE_industry, "\\.", "_")
all_data$NACE_industry <- str_replace_all(all_data$NACE_industry, "\\,", "_")
all_data$NACE_industry <- str_replace_all(all_data$NACE_industry, "-", "to")



# Choose the granularity of the Sector variable
all_data$Sector <- as.character(all_data$NACE_industry)
sectors <- as.data.frame(unique(all_data[,c("Sector", "Code")]))

all_sectors <- unique(all_data$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]




test <- felm(abs_intra_day ~ mention |Code, data = all_data)
summary(test)
summary(felm(highlow ~ mention + plm::lag(highlow,1) |Code + Date, data = all_data))
test <- felm(highlow ~ abs_intra_day + mention + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
summary(test)



####### Get some cross firm, within sector effects, by cycling through each date

# Cut down the size of the data to speed up the super inefficient loop
temp_data <- all_data[,c("Date", "Code", "Sector", "abs_intra_day", "highlow", "VI_put",
                       "VI_call", "Index_Change","IndexHighLow", "mention")]
temp_data$Date <- as.character(temp_data$Date)
temp_variables <- colnames(temp_data)


# Separate data frame for the own_sec mentions
cross_firm_data <- temp_data[1,]
cross_firm_data$own_sec_mention <- 0 
cross_firm_data$own_sec_mention_notme <- 0 
cross_firm_data <- cross_firm_data[0,]
colnames(cross_firm_data)


for (s in all_sectors){
  print(s)
  sector_data <- temp_data[which(temp_data$Sector == s),temp_variables]
  
  # Aggregate mention dummies per sector per day
  sector_mention_avg <- aggregate(sector_data$mention, by=list(sector_data$Date), mean)
  sector_mention <- aggregate(sector_data$mention, by=list(sector_data$Date), max)
  
  # Rename for merging
  colnames(sector_mention) <- (c("Date", paste0("Sector", s,"_mention")))
  colnames(sector_mention_avg) <- (c("Date", paste0("Sector", s,"_mention_avg")))
  sector_mention$Date <- as.character(sector_mention$Date)
  sector_mention_avg$Date <- as.character(sector_mention_avg$Date)
  
  temp_data <- merge(temp_data, sector_mention, by = "Date", all.x = TRUE)
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_mention)),]$Sector",s 
                   ,"_mention <- 0", sep = "")
  eval(parse(text=command))
    
  temp_data <- merge(temp_data, sector_mention_avg, by = "Date", all.x = TRUE)
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_mention_avg)),]$Sector",s 
                     ,"_mention_avg <- 0", sep = "")
  eval(parse(text=command))
    
  # And store the "own sector" effect in the sector_data df
  colnames(sector_mention) <- (c("Date", paste0("own_sec_mention")))
  colnames(sector_mention_avg) <- (c("Date", paste0("own_sec_mention_avg")))
  sector_data <- merge(sector_data, sector_mention, by = "Date", all.x = TRUE)  
  sector_data <- merge(sector_data, sector_mention_avg, by = "Date", all.x = TRUE)  
  
  # Add this to the cross_firm_data df
  cross_firm_data <- rbind(cross_firm_data, sector_data)
}
cross_firm_data$own_sec_mention_notme <- as.numeric(cross_firm_data$mention==0 & cross_firm_data$own_sec_mention ==1)
cross_firm_data$own_sec_mention_avg_notme <- cross_firm_data$own_sec_mention_avg
cross_firm_data[which(cross_firm_data$mention==1), "own_sec_mention_avg_notme"] <- 0

# Merge in the sector_by_sector mentions from temp_data
colnames(temp_data)
varstart <- which(colnames(temp_data)== temp_variables[length(temp_variables)]) +1 # Thursday was the last variable in temp_data before
each_sec_vars <- colnames(temp_data[,varstart:(ncol(temp_data))]) 
temp_data <- temp_data[,c("Date", "Code", "Sector", each_sec_vars)]

temp_data$Date <- as.character(temp_data$Date)
temp_data$Code <- as.character(temp_data$Code)
temp_data$Sector <- as.character(temp_data$Sector)
cross_firm_data$Date <- as.character(cross_firm_data$Date)
cross_firm_data$Code <- as.character(cross_firm_data$Code)
cross_firm_data$Sector <- as.character(cross_firm_data$Sector)

cross_firm_data <- merge(cross_firm_data, temp_data, by = c("Date", "Code", "Sector"), all.x = TRUE)

cross_firm_data <- data.frame(cross_firm_data)
cross_firm_data$Date <- as.Date(cross_firm_data$Date)
cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))

# Own sector mention effects
model1 = felm(abs_intra_day ~ mention + own_sec_mention_notme | Code, data = cross_firm_data)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention_notme | Code + Date, data = cross_firm_data)
summary(model2)
model3 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10)| Code + Date
             , data = cross_firm_data)
summary(model3)
model4 = felm(highlow ~ mention + own_sec_mention_notme | Code, 
             data = cross_firm_data)
summary(model4)
model5 = felm(highlow ~ mention + own_sec_mention_notme | Code + Date, 
              data = cross_firm_data)
summary(model5)
model6 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10) | Code + Date, 
              data = cross_firm_data)
summary(model6)
model6 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10) + 
                plm::lag(abs_intra_day, 0:10)| Code + Date, 
              data = cross_firm_data)
summary(model6)


Cross_firm_mention_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                           table.placement = "H", df = FALSE, column.sep.width = "2pt",
                                           title = "Cross-firm effects with mention dummies")

# Average own sector mention effects
model1 = felm(abs_intra_day ~ mention + own_sec_mention_avg_notme | Code, data = cross_firm_data)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention_avg_notme | Code + Date, data = cross_firm_data)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_avg_notme + plm::lag(abs_open_open, 0:10)| Code + Date
              , data = cross_firm_data)
summary(model3)
model4 = felm(highlow ~ mention + own_sec_mention_avg_notme | Code, 
              data = cross_firm_data)
summary(model4)
model5 = felm(highlow ~ mention + own_sec_mention_avg_notme | Code + Date, 
              data = cross_firm_data)
summary(model5)
model6 = felm(highlow ~ mention + own_sec_mention_avg_notme + plm::lag(highlow, 1:10) | Code + Date, 
              data = cross_firm_data)
summary(model6)


Cross_firm_mention_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                      table.placement = "H", df = FALSE, column.sep.width = "2pt",
                                      title = "Cross-firm effects with mention dummies")






### Caculate average within sector volatility for each day


# Get N_sector for each day 
sector_N <-  cross_firm_data %>%
  count(Date, Sector) 
colnames(sector_N) <- c("Date", "Sector", "N_sector")
cross_firm_data <- merge(cross_firm_data, sector_N, by = c("Date", "Sector"), all.x = TRUE)



# abs_intra
own_sector_shocks_df <- aggregate(cross_firm_data$abs_intra_day, 
                                  by=list(Date=cross_firm_data$Date, Sector=cross_firm_data$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_abs_intra")
cross_firm_data <- merge(cross_firm_data, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# highlow
own_sector_shocks_df <- aggregate(cross_firm_data$highlow, 
                                  by=list(Date=cross_firm_data$Date, Sector=cross_firm_data$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_highlow")
cross_firm_data <- merge(cross_firm_data, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# not_me effects
cross_firm_data$own_sec_abs_intra_notme <- 
  cross_firm_data$own_sec_abs_intra - cross_firm_data$abs_intra_day/cross_firm_data$N_sector
cross_firm_data$own_sec_highlow_notme <- 
  cross_firm_data$own_sec_highlow - cross_firm_data$highlow/cross_firm_data$N_sector


cross_firm_data <- as.data.frame(cross_firm_data)
cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))


clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_mentions_data_NACE.csv", sep = "/")
write.csv(cross_firm_data, file = clean_filename, row.names = FALSE)
# cross_firm_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
# cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))



############################  Compare the within sector effect for different options ############################ 

# Dummies
model1 = felm(abs_intra_day ~ mention| Code, data = cross_firm_data)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_data)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code, data = cross_firm_data)
summary(model3)
model4 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_data)
summary(model4)
model5 = felm(abs_intra_day ~ mention + own_sec_mention_notme + plm::lag(abs_open_open, 0:10)| Code +Date, data = cross_firm_data)
summary(model5)

model6 = felm(highlow ~ mention| Code, data = cross_firm_data)
summary(model6)
model7 = felm(highlow ~ mention + own_sec_mention| Code, data = cross_firm_data)
summary(model7)
model8 = felm(highlow ~ mention + own_sec_mention_notme| Code, data = cross_firm_data)
summary(model8)
model9 = felm(highlow ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_data)
summary(model9)
model10 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_data)
summary(model10)

mention_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                                  model6, model7, model8, model9, model10, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "Mention own sector effects")


# Include reduced form sector level volatility
model1 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_data)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention + own_sec_abs_intra_notme| Code, data = cross_firm_data)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code, data = cross_firm_data)
summary(model3)
model4 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code + Date, data = cross_firm_data)
summary(model4)
model5 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme + plm::lag(abs_intra_day, 1:10)| Code, data = cross_firm_data)
summary(model5)

model6 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_data)
summary(model6)
model7 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_data)
summary(model7)
model8 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code, data = cross_firm_data)
summary(model8)
model9 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code + Date, data = cross_firm_data)
summary(model9)
model10 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_data)
summary(model10)

mention_control_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                                       model6, model7, model8, model9, model10, column.sep.width = "1pt",
                                       table.placement = "H", df = FALSE, title = "Mention own sector effects, controlling for sector movements (NACE).")



### Each sector effect on the whole index
aggregate_news <- cross_firm_data[,c("Date", "Index_Change", "Index_abs_Change", "IndexHighLow", 
                                     mention_avg_vars, mention_vars)]
aggregate_news <- unique(aggregate_news)
# Differences seem minor, so just remove duplicates for now
aggregate_news = aggregate_news[!duplicated(aggregate_news$Date),]

aggregate_news <- aggregate_news[order(aggregate_news$Date),]

command <- paste("model1 <- felm(IndexHighLow ~ ", 
                 paste(mention_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model1)
command <- paste("model2 <- felm(Index_Change ~ ", 
                 paste(mention_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model2)
command <- paste("model3 <- felm(Index_abs_Change ~ ", 
                 paste(mention_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model3)

stargazer(model1, model2, model3, column.sep.width = "1pt", table.placement = "H", font.size = "tiny",
          df = FALSE, title = "Aggregate mention effect by sector")


command <- paste("model1 <- felm(IndexHighLow ~ ", 
                 paste(mention_avg_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model1)
command <- paste("model2 <- felm(Index_Change ~ ", 
                 paste(mention_avg_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model2)
command <- paste("model3 <- felm(Index_abs_Change ~ ", 
                 paste(mention_avg_vars, collapse = " + "), ", data = aggregate_news)", sep = "")
eval(parse(text=command))
summary(model3)

stargazer(model1, model2, model3, column.sep.width = "1pt", table.placement = "H", font.size = "tiny",
          df = FALSE, title = "Aggregate average mention effect byb sector")

