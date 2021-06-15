setwd("~/Documents/GitHub/Firm_level_news_analysis")
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
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_topics_shocks.csv", sep = "/")
all_data <- read.csv(import_filename, stringsAsFactors = FALSE)
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
sectors <- as.data.frame(unique(all_data[,c("Sector", "Company")]))

all_sectors <- unique(all_data$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]




test <- felm(abs_intra_day ~ mention |Code, data = all_data)
summary(test)
summary(felm(highlow ~ mention + plm::lag(highlow,1) |Code + Date, data = all_data))
test <- felm(abs_intra_day ~ mention + highlow + plm::lag(abs_open_open,0:10)|Code + Date, data = all_data)
summary(test)
test <- felm(highlow ~ abs_intra_day + mention + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
summary(test)
test <- felm(highlow ~ abs_intra_day + highlow_control_shock + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
summary(test)
test <- felm(highlow ~ abs_intra_day + T10_highlow + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
summary(test)



####### Get some cross firm, within sector effects, by cycling through each date

# Cut down the size of the data to speed up the super inefficient loop
temp_data <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day", "highlow", 
                       "abspChange", "pChange", "Index_abs_Change", 
                       "Index_Change","IndexHighLow", "mention", "abs_open_open",
                       "Monday", "Tuesday", "Wednesday", "Thursday")]
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
varstart <- which(colnames(temp_data)== "Thursday") +1 # Thursday was the last variable in temp_data before
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

clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_mentions_data_NACE.csv", sep = "/")
write.csv(cross_firm_data, file = clean_filename, row.names = FALSE)
# cross_firm_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
# cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))


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

# Look at effect of each sector in turn
even_indexes<-seq(2,length(each_sec_vars),2)
odd_indexes<-seq(1,(length(each_sec_vars)-1),2)
mention_vars <- each_sec_vars[odd_indexes]
mention_avg_vars <- each_sec_vars[even_indexes]

command <- paste("abs_sector_dummies <- felm(abs_intra_day ~ mention + own_sec_mention_notme + ", 
                 paste(mention_vars, collapse = " + "), 
                 "+ plm::lag(abs_open_open, 0:4) | Code, data = cross_firm_data)", sep = "")
eval(parse(text=command))
summary(abs_sector_dummies)

command <- paste("abs_sector_avg <- felm(abs_intra_day ~ mention + own_sec_mention_notme + ", 
                 paste(mention_avg_vars, collapse = " + "), 
                 "+ plm::lag(abs_open_open, 0:4) | Code, data = cross_firm_data)", sep = "")
eval(parse(text=command))
summary(abs_sector_avg)

command <- paste("highlow_sector_dummies <- felm(highlow ~ mention + own_sec_mention_notme + ", 
                 paste(mention_vars, collapse = " + "), 
                 "+ plm::lag(highlow, 1:4) | Code, data = cross_firm_data)", sep = "")
eval(parse(text=command))
summary(highlow_sector_dummies)

command <- paste("highlow_sector_avg <- felm(highlow ~ mention + own_sec_mention_notme + ", 
                 paste(mention_avg_vars, collapse = " + "), 
                 "+ plm::lag(highlow, 1:4) | Code, data = cross_firm_data)", sep = "")
eval(parse(text=command))
summary(highlow_sector_avg)


stargazer(abs_sector_dummies,highlow_sector_dummies, font.size = "tiny",
          table.placement = "H", df = FALSE, column.sep.width = "2pt",
          title = "All sector effects")
stargazer(abs_sector_avg,highlow_sector_avg, font.size = "tiny",
          table.placement = "H", df = FALSE,
          title = "All sector effects")



### Test whether there are different effects if you exclude firms also mentioned tomorrow
all_data$mention_lead <- plm::lag(all_data$mention, -1)
all_data$mention_headlead <- plm::lag(all_data$head_mention, -1)
all_data$mention_nerlead <- plm::lag(all_data$ner_mention, -1)
all_data$today_and_tomorrow <- as.numeric(all_data$mention == 1 & all_data$mention_lead == 1)
all_data$today_and_tomorrow_head <- as.numeric(all_data$mention == 1 & all_data$mention_headlead == 1)
all_data$today_and_tomorrow_ner <- as.numeric(all_data$mention == 1 & all_data$mention_nerlead == 1)

all_data$mention_not_tomorrow <- all_data$mention - all_data$today_and_tomorrow
all_data$mention_not_headtomorrow <- all_data$mention - all_data$today_and_tomorrow_head
all_data$mention_not_nertomorrow <- all_data$mention - all_data$today_and_tomorrow_ner

all_data$abs_control_shock_not_tomorrow <- all_data$abs_control_shock*(all_data$mention - all_data$today_and_tomorrow)
all_data$highlow_control_shock_not_tomorrow <- all_data$highlow_control_shock*(all_data$mention - all_data$today_and_tomorrow)
all_data$abs_control_shock_todayandtomorrow <- all_data$abs_control_shock*(all_data$mention*all_data$today_and_tomorrow)
all_data$highlow_control_shock_todayandtomorrow <- all_data$highlow_control_shock*(all_data$mention*all_data$today_and_tomorrow)


### What is the effect of removing the mention tomorrow?
model1 <- felm(highlow ~ mention| Code, data = all_data)
summary(model1)
model2 <- felm(highlow ~ mention_not_tomorrow| Code, data = all_data)
summary(model2)
model3 <- felm(highlow ~ today_and_tomorrow| Code, data = all_data)
summary(model3)
model4 <- felm(highlow ~ mention_not_tomorrow + plm::lag(highlow, 1:10)| Code + Date, 
              data = all_data)
summary(model4)
model5 <- felm(highlow ~ today_and_tomorrow + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
summary(model5)
model6 <- felm(highlow ~ highlow_control_shock_not_tomorrow + plm::lag(highlow, 1:10)| Code + Date, 
              data = all_data)
summary(model6)
model7 <- felm(highlow ~ highlow_control_shock_todayandtomorrow + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
summary(model7)

stargazer(model1, model2, model3, model4, model5, model6, model7, column.sep.width = "1pt", table.placement = "H",
          df = FALSE, title = "Results taking into account whether also mentioned tomorrow")


model1 <- felm(abs_intra_day ~ mention| Code, data = all_data)
summary(model1)
model2 <- felm(abs_intra_day ~ mention_not_tomorrow| Code, data = all_data)
summary(model2)
model3 <- felm(abs_intra_day ~ today_and_tomorrow| Code, data = all_data)
summary(model3)
model4 <- felm(abs_intra_day ~ mention_not_tomorrow + plm::lag(abs_open_open, 0:10)| Code + Date, 
              data = all_data)
summary(model4)
model5 <- felm(abs_intra_day ~ today_and_tomorrow + plm::lag(abs_open_open, 0:10)| Code + Date, data = all_data)
summary(model5)
model6 <- felm(abs_intra_day ~ abs_control_shock_not_tomorrow + plm::lag(abs_open_open, 0:10)| Code + Date, 
              data = all_data)
summary(model6)
model7 <- felm(abs_intra_day ~ abs_control_shock_todayandtomorrow + plm::lag(abs_open_open, 0:10)| Code + Date, data = all_data)
summary(model7)

stargazer(model1, model2, model3, model4, model5, model6, model7, column.sep.width = "1pt", table.placement = "H",
          df = FALSE, title = "Results taking into account whether also mentioned tomorrow")




### What is the effect of removing the mention, and controlling for absolute change 
model1 <- felm(highlow ~ abs_intra_day + mention| Code, data = all_data)
summary(model1)
model2 <- felm(highlow ~ abs_intra_day + mention_not_tomorrow| Code, data = all_data)
summary(model2)
model3 <- felm(highlow ~ abs_intra_day + today_and_tomorrow| Code, data = all_data)
summary(model3)
model4 <- felm(highlow ~ abs_intra_day + mention_not_tomorrow + plm::lag(highlow, 1:10)| Code + Date, 
              data = all_data)
summary(model4)
model5 <- felm(highlow ~ abs_intra_day + today_and_tomorrow + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
summary(model5)
model6 <- felm(highlow ~ abs_intra_day + highlow_control_shock_not_tomorrow + plm::lag(highlow, 1:10)| Code + Date, 
              data = all_data)
summary(model6)
model7 <- felm(highlow ~ abs_intra_day + highlow_control_shock_todayandtomorrow + plm::lag(highlow, 1:10)| Code + Date, data = all_data)
summary(model7)

stargazer(model1, model2, model3, model4, model5, model6, model7, column.sep.width = "1pt", table.placement = "H",
          df = FALSE, title = "Results looking at consecutive mentions and controlling for intra-day")






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



#################### Bring in the Topicsssss ##########################

# First add the own_sec_mention dummies to the all_data data.frame
sec_mentions <- cross_firm_data[,c("Date", "Code", "own_sec_mention", "own_sec_mention_avg", 
                                   "own_sec_mention_notme", "own_sec_mention_avg_notme"  )]
all_data <- merge(all_data, sec_mentions, by = c("Date", "Code"), all.x = TRUE)


# Bear in mind that the Topics are already included in all_data


test <- felm(highlow ~ mention + own_sec_mention_notme |Code , data = all_data)
summary(test)



# Get the own sector effects for each shock and measure
cross_firm_news <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day", "highlow",
                               "abspChange", "pChange", "Index_abs_Change",
                               "Index_Change", "IndexHighLow", "mention", "abs_open_open", 
                               "Monday", "Tuesday", "Wednesday", "Thursday",
                               "own_sec_mention", "own_sec_mention_avg", 
                               "own_sec_mention_notme", "own_sec_mention_avg_notme",  
                               "abs_shock","abs_control_shock", "abs_control_shock_phi",
                               "highlow_shock", "highlow_control_shock", "highlow_control_shock_phi" )]
cross_firm_news$Date <- as.character(cross_firm_news$Date)

# Get N_sector for each day 
sector_N <-  cross_firm_news %>%
  count(Date, Sector) 
colnames(sector_N) <- c("Date", "Sector", "N_sector")
cross_firm_news <- merge(cross_firm_news, sector_N, by = c("Date", "Sector"), all.x = TRUE)



######################################### abs intra day ######################################### 
# abs_intra shock
own_sector_shocks_df <- aggregate(cross_firm_news$abs_shock, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_abs")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)
  
# abs_intra_control shock
own_sector_shocks_df <- aggregate(cross_firm_news$abs_control_shock, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_abs_c")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# abs_intra_control_phi shock
own_sector_shocks_df <- aggregate(cross_firm_news$abs_control_shock_phi, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_abs_c_phi")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# abs_intra
own_sector_shocks_df <- aggregate(cross_firm_news$abs_intra_day, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_abs_intra")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)



######################################### highlow ######################################### 
# highlow shock
own_sector_shocks_df <- aggregate(cross_firm_news$highlow_control_shock, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_highlow")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# highlow_control shock
own_sector_shocks_df <- aggregate(cross_firm_news$highlow_control_shock, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_highlow_c")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# highlow_control_phi shock
own_sector_shocks_df <- aggregate(cross_firm_news$highlow_control_shock_phi, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_news_highlow_c_phi")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)

# highlow
own_sector_shocks_df <- aggregate(cross_firm_news$highlow, 
                                  by=list(Date=cross_firm_news$Date, Sector=cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_highlow")
cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)


#### not_me effects
cross_firm_news$own_sec_news_abs_notme <- 
  cross_firm_news$own_sec_news_abs - cross_firm_news$abs_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_abs_c_notme <- 
  cross_firm_news$own_sec_news_abs_c - cross_firm_news$abs_control_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_abs_c_phi_notme <- 
  cross_firm_news$own_sec_news_abs_c_phi - cross_firm_news$abs_control_shock_phi/cross_firm_news$N_sector
cross_firm_news$own_sec_news_highlow_notme <- 
  cross_firm_news$own_sec_news_highlow - cross_firm_news$highlow_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_highlow_c_notme <- 
  cross_firm_news$own_sec_news_highlow_c - cross_firm_news$highlow_control_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_highlow_c_phi_notme <- 
  cross_firm_news$own_sec_news_highlow_c_phi - cross_firm_news$highlow_control_shock_phi/cross_firm_news$N_sector


cross_firm_news$own_sec_abs_intra_notme <- 
  cross_firm_news$own_sec_abs_intra - cross_firm_news$abs_intra_day/cross_firm_news$N_sector
cross_firm_news$own_sec_highlow_notme <- 
  cross_firm_news$own_sec_highlow - cross_firm_news$highlow/cross_firm_news$N_sector


cross_firm_news <- as.data.frame(cross_firm_news)
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))





############################  Compare the within sector effect for different options ############################ 

# Dummies
model1 = felm(abs_intra_day ~ mention| Code, data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code, data = cross_firm_news)
summary(model3)
model4 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_news)
summary(model4)
model5 = felm(abs_intra_day ~ mention + own_sec_mention_notme + plm::lag(abs_open_open, 0:10)| Code +Date, data = cross_firm_news)
summary(model5)

model6 = felm(highlow ~ mention| Code, data = cross_firm_news)
summary(model6)
model7 = felm(highlow ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(model7)
model8 = felm(highlow ~ mention + own_sec_mention_notme| Code, data = cross_firm_news)
summary(model8)
model9 = felm(highlow ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_news)
summary(model9)
model10 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(model10)

mention_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                                  model6, model7, model8, model9, model10, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "Mention own sector effects")



# Absolute sLDA shocks
model1 = felm(abs_intra_day ~ abs_control_shock| Code, data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c| Code, data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(model3)
model4 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code + Date, data = cross_firm_news)
summary(model4)
model5 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(model5)

model6 = felm(highlow ~ highlow_control_shock| Code, data = cross_firm_news)
summary(model6)
model7 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c| Code, data = cross_firm_news)
summary(model7)
model8 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme| Code, data = cross_firm_news)
summary(model8)
model9 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme| Code + Date, data = cross_firm_news)
summary(model9)
model10 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(model10)

slda_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                               model6, model7, model8, model9, model10, column.sep.width = "1pt",
                               table.placement = "H", df = FALSE, title = "sLDA own sector effects")



# Include reduced form sector level volatility
model1 = felm(abs_intra_day ~ abs_control_shock + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(model3)
model4 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme| Code + Date, data = cross_firm_news)
summary(model4)
model5 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(model5)

model6 = felm(highlow ~ highlow_control_shock + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model6)
model7 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model7)
model8 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model8)
model9 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + own_sec_highlow_notme| Code + Date, data = cross_firm_news)
summary(model9)
model10 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + own_sec_highlow_notme + plm::lag(highlow, 1:10)| Code +Date, data = cross_firm_news)
summary(model10)

slda_control_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                                       model6, model7, model8, model9, model10, column.sep.width = "1pt",
                                       table.placement = "H", df = FALSE, title = "sLDA own sector effects, controlling for sector movements (NACE).")


# Include reduced form sector level volatility
model1 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(model3)
model4 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code + Date, data = cross_firm_news)
summary(model4)
abs_intra5 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(model5)

model6 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model6)
model7 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model7)
model8 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(model8)
model9 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code + Date, data = cross_firm_news)
summary(model9)
model10 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(model10)

slda_control_ownsec_table <- stargazer(model1, model2, model3, model4, model5,
                                       model6, model7, model8, model9, model10, column.sep.width = "1pt",
                                       table.placement = "H", df = FALSE, title = "Mention own sector effects, controlling for sector movements (NACE).")















############################ When not mentioned tomorrow ############################ 


### Get sector effects when notmentioned tomorrow
cross_firm_news$Date <- as.character(cross_firm_news$Date)
cross_firm_news$Sector <- as.character(cross_firm_news$Sector)
short_merger <- all_data[,c("Code", "Date", "mention_not_tomorrow")]
cross_firm_news <- merge(cross_firm_news, short_merger, by = c("Code", "Date"), all.x = TRUE)

# Now create a notomorrow variable for each of the shocks
cross_firm_news$abs_shock_notomorrow <- cross_firm_news$abs_shock*cross_firm_news$mention_not_tomorrow
cross_firm_news$abs_control_shock_notomorrow <- cross_firm_news$abs_control_shock*cross_firm_news$mention_not_tomorrow
cross_firm_news$abs_control_shock_phi_notomorrow <- cross_firm_news$abs_control_shock_phi*cross_firm_news$mention_not_tomorrow
cross_firm_news$highlow_shock_notomorrow <- cross_firm_news$highlow_shock*cross_firm_news$mention_not_tomorrow
cross_firm_news$highlow_control_shock_notomorrow <- cross_firm_news$highlow_control_shock*cross_firm_news$mention_not_tomorrow
cross_firm_news$highlow_control_shock_phi_notomorrow <- cross_firm_news$highlow_control_shock_phi*cross_firm_news$mention_not_tomorrow





# Create an own sector variable and merge it in to own_sector_shocks_df each time

# abs shock
own_sector_shocks_df <- aggregate(cross_firm_news[,c("abs_shock_notomorrow")], 
                                        by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(own_sector_shocks_df) <- c("Date", "Sector", "own_sec_abs_shock_notomorrow")

# abs control shock
temp_data <- aggregate(cross_firm_news[,c("abs_control_shock_notomorrow")], 
                                  by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(temp_data) <- c("Date", "Sector", "own_sec_abs_c_shock_notomorrow")
own_sector_shocks_df <- merge(own_sector_shocks_df, temp_data, by = c("Date", "Sector"), all.x = TRUE)

# abs control phi shock
temp_data <- aggregate(cross_firm_news[,c("abs_control_phi_shock_notomorrow")], 
                                  by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(temp_data) <- c("Date", "Sector", "own_sec_abs_c_phi_shock_notomorrow")
own_sector_shocks_df <- merge(own_sector_shocks_df, temp_data, by = c("Date", "Sector"), all.x = TRUE)

# highlow
temp_data <- aggregate(cross_firm_news[,c("highlow_shock_notomorrow")], 
                                  by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(temp_data) <- c("Date", "Sector", "own_sec_highlow_shock_notomorrow")
own_sector_shocks_df <- merge(own_sector_shocks_df, temp_data, by = c("Date", "Sector"), all.x = TRUE)

# highlow
temp_data <- aggregate(cross_firm_news[,c("highlow_control_shock_notomorrow")], 
                                  by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(temp_data) <- c("Date", "Sector", "own_sec_highlow_c_notomorrow")
own_sector_shocks_df <- merge(own_sector_shocks_df, temp_data, by = c("Date", "Sector"), all.x = TRUE)

# highlow
temp_data <- aggregate(cross_firm_news[,c("highlow_control_shock_phi_notomorrow")], 
                                  by = list(cross_firm_news$Date, cross_firm_news$Sector), mean)
colnames(temp_data) <- c("Date", "Sector", "own_sec_highlow_c_phi_notomorrow")
own_sector_shocks_df <- merge(own_sector_shocks_df, temp_data, by = c("Date", "Sector"), all.x = TRUE)



cross_firm_news <- merge(cross_firm_news, own_sector_shocks_df, by = c("Date", "Sector"), all.x = TRUE)
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))

test = felm(highlow ~ highlow_control_shock_notomorrow + own_sec_highlow_shock_notomorrow | Code + Date, 
            data = cross_firm_news)
summary(test)


clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_NACE.csv", sep = "/")
write.csv(cross_firm_news, file = clean_filename, row.names = FALSE)
# cross_firm_news <- read.csv(clean_filename, stringsAsFactors = FALSE)
# cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))




######################################### abs intra day ######################################### 
model1 = felm(abs_intra_day ~ abs_shock_notomorrow + own_sec_abs_shock_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ abs_shock_notomorrow + own_sec_abs_shock_notomorrow +
                plm::lag(abs_open_open, 0:10)| Code + Date, 
              data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ abs_control_shock_notomorrow + own_sec_abs_c_shock_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model3)
model4 = felm(abs_intra_day ~ abs_control_shock_notomorrow + own_sec_abs_c_shock_notomorrow +
                plm::lag(abs_open_open, 0:10)| Code + Date, 
              data = cross_firm_news)
summary(model4)
model5 = felm(abs_intra_day ~ abs_control_shock_phi_notomorrow + own_sec_abs_c_phi_shock_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model5)
model6 = felm(abs_intra_day ~ abs_control_shock_phi_notomorrow + own_sec_abs_c_phi_shock_notomorrow +
                plm::lag(abs_open_open, 0:10)| Code + Date, 
              data = cross_firm_news)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6, column.sep.width = "1pt", table.placement = "H",
          df = FALSE, title = "Abs own sector shocks with no mention tomorrow")


######################################### highlow ######################################### 
model1 = felm(highlow ~ highlow_shock_notomorrow + own_sec_highlow_shock_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model1)
model2 = felm(highlow ~ highlow_shock_notomorrow + own_sec_highlow_shock_notomorrow +
                plm::lag(highlow, 1:10)| Code + Date, 
              data = cross_firm_news)
summary(model2)
model3 = felm(highlow ~ highlow_control_shock_notomorrow + own_sec_highlow_c_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model3)
model4 = felm(highlow ~ highlow_control_shock_notomorrow + own_sec_highlow_c_notomorrow +
                plm::lag(highlow, 1:10)| Code + Date, 
              data = cross_firm_news)
summary(model4)
model5 = felm(highlow ~ highlow_control_shock_phi_notomorrow + own_sec_highlow_c_phi_notomorrow | Code + Date, 
              data = cross_firm_news)
summary(model5)
model6 = felm(highlow ~ highlow_control_shock_phi_notomorrow + own_sec_highlow_c_phi_notomorrow +
                plm::lag(highlow, 1:10)| Code + Date, 
              data = cross_firm_news)
summary(model6)

stargazer(model1, model2, model3, model4, model5, model6, column.sep.width = "1pt", table.placement = "H",
          df = FALSE, title = "High-low own sector shocks with no mention tomorrow")









# Compare the persistence of different effects
model1 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(model1)
model2 = felm(abs_intra_day ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3)| Code, data = cross_firm_news)
summary(model2)
model3 = felm(abs_intra_day ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3) + 
                    plm::lag(own_sec_abs_intra_notme, 0:3) + 
                    plm::lag(abs_open_open, 0:10)| Code + Date, data = cross_firm_news)
summary(model3)

model4 = felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme| Code, data = cross_firm_news)
summary(model4)
model5 = felm(highlow ~ plm::lag(highlow_control_shock, 0:3) + plm::lag(own_sec_news_highlow_c_notme, 0:3)| Code, data = cross_firm_news)
summary(model5)
model6 = felm(highlow ~ plm::lag(highlow_control_shock, 0:3) + plm::lag(own_sec_news_highlow_c_notme, 0:3) + 
                  plm::lag(own_sec_highlow_notme, 0:3) + 
                  plm::lag(highlow, 1:10)| Code + Date, data = cross_firm_news)
summary(model6)

slda_persistence_ownsec_table <- stargazer(model1, model2, model3, model4, model5, model6, 
                                           column.sep.width = "1pt",table.placement = "H",
                                           df = FALSE, title = "Persistence sLDA own sector effects, controlling for sector movements (NACE).")





daily_mean <- aggregate(all_data[,c("highlow_control_shock_phi", "IndexHighLow", "Index_abs_Change")], 
                        by = list(all_data$Date), FUN = mean)

summary(lm(IndexHighLow ~ Index_abs_Change + highlow_control_shock_phi
           + lag(IndexHighLow, 1) + lag(IndexHighLow, 2) + lag(IndexHighLow, 3)
           + lag(IndexHighLow, 4), data = daily_mean))
