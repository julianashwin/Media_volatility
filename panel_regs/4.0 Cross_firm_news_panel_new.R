setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_absreltopics_shocks.csv", sep = "/")
all_data <- read.csv(import_filename, stringsAsFactors = FALSE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))


# Choose the granularity of the Sector variable
all_data$Sector <- as.character(all_data$Industry_Group)
all_data$Sector[which(all_data$Industry_Group == "cut-off")] <- NA
sectors <- as.data.frame(unique(all_data[,c("Sector", "Company")]))

all_sectors <- unique(all_data$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]




test <- plm(abs_intra_day ~ mention , data = all_data,
                    index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(abs_intra_day ~ abs_shock , data = all_data,
                    index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(abs_intra_day ~ mention + lag(abs_open_open,0:4) 
                    + lag(Index_abs_Change, 0:4)
                    + Monday + Tuesday + Wednesday + Thursday, data = all_data,
                    index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(intra_day ~ rel_control_shock + lag(open_open,0:4) 
            + lag(Index_Change, 0:4)
            + Monday + Tuesday + Wednesday + Thursday, data = all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(highlow ~ mention , data = all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(highlow ~ abs_shock , data = all_data,
            index = c("Code", "Date"), model = "within")
summary(test)




####### Get some cross firm, within sector effects, by cycling through each date

# Cut down the size of the data to speed up the super inefficient loop
temp_data <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day", "highlow", 
                       "abspChange", "pChange", "Index_abs_Change", 
                       "Index_Change","IndexHighLow", "mention", "abs_open_open",
                       "Monday", "Tuesday", "Wednesday", "Thursday")]
temp_data$Date <- as.Date(temp_data$Date)
temp_data$own_sec_mention <- 0 
temp_data$own_sec_mention_notme <- 0 
for (s in all_sectors){
  command <- paste("temp_data$Sec", s, 
                   "_mention <- 0", sep = "")
  eval(parse(text=command))
}
cross_variables <- colnames(temp_data)
print(cross_variables)
cross_firm_data <- temp_data[0,]

all_dates <- unique(temp_data$Date)
for(i in 1:length(all_dates)){
  print(all_dates[i])
  d <- all_dates[i]
  today_data <- temp_data[which(temp_data$Date == d),]
  for (s in all_sectors){
    sector_day_data <- today_data[which(today_data$Sector == s),]
    sector_mention <- as.numeric(any(sector_day_data$mention ==1))
    
    command1 <- paste("today_data$Sec", s, 
                      "_mention <- sector_mention", sep = "")
    eval(parse(text=command1))
    
    command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                      "\"),\"own_sec_mention\"] <- sector_mention", sep = "")
    eval(parse(text=command2))
    
  }
  
  cross_firm_data <- rbind(cross_firm_data, today_data)
}

cross_firm_data$own_sec_mention_notme <- as.numeric(cross_firm_data$mention==0 & cross_firm_data$own_sec_mention ==1)


cross_firm_data <- data.frame(cross_firm_data)
cross_firm_data$Date <- as.Date(cross_firm_data$Date)
cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))

clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_mentions_data.csv", sep = "/")
write.csv(cross_firm_data, file = clean_filename, row.names = FALSE)
# cross_firm_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "Date"))

# Merge back in the DoW dummies that were annoyingly excluded (shouldn't need this anymore)
#dow_dummies <- all_data[, c("Date", "Code", "Monday", "Tuesday", "Wednesday", "Thursday")]
#cross_firm_data <- merge(cross_firm_data, dow_dummies, by = c("Date", "Code"))


model1 = felm(abs_intra_day ~ mention + own_sec_mention_notme | Code, data = cross_firm_data)
summary(model1)
model2 = felm(abs_intra_day ~ mention + own_sec_mention_notme | Code + Date, data = cross_firm_data)
summary(model2)
model3 = felm(abs_intra_day ~ mention + own_sec_mention_notme + plm::lag(abs_open_open, 0:10)| Code + Date
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


Cross_firm_mention_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                           table.placement = "H", df = FALSE, column.sep.width = "2pt",
                                           title = "Cross-firm effects with mention dummies")


#stargazer(own_sec_fixed, own_sec_controls_fixed, own_sec_fixed_lead, 
#          own_sec_controls_fixed_lead)



all_sector_dummies <- plm(abs_intra_day ~ mention 
                          + own_sec_mention_notme 
                          + Sec5120_mention + Sec5430_mention + Sec5410_mention + Sec5530_mention + Sec5510_mention
                          + Sec5220_mention + Sec5010_mention + Sec5710_mention + SecNA_mention + Sec5340_mention
                          + Sec5550_mention+Sec5620_mention+Sec5720_mention+ Sec5130_mention+Sec5240_mention
                          + Sec5210_mention + Sec5910_mention + Sec5540_mention + Sec5110_mention + Sec5330_mention 
                          + Sec5810_mention + Sec5310_mention + Sec5610_mention + Sec554_mention +Sec5560_mention
                          + plm::lag(abspChange, 1:4)
                          + plm::lag(Index_abs_Change, 0:4)
                          , data = cross_firm_data, 
                          index = c("Code", "Date"), model = "within")
summary(all_sector_dummies)

stargazer(all_sector_dummies,
          table.placement = "H", df = FALSE,
          title = "All sector effects")





#################### Bring in the Topicsssss ##########################

# First add the own_sec_mention dummies to the all_data data.frame
sec_mentions <- cross_firm_data[,c("Date", "Code", "own_sec_mention", "own_sec_mention_notme")]
all_data <- merge(all_data, sec_mentions, by = c("Date", "Code"), all.x = TRUE)


# Bear in mind that the Topics are already included in all_data


test <- plm(abs_intra_day ~ own_sec_mention_notme , data = all_data,
              index = c("Code", "Date"), model = "within")
summary(test)


# Get some cross sector effects, based on the sLDA decomposition
temp_data <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day", "highlow",
                       "abspChange", "pChange", "Index_abs_Change",
                       "Index_Change", "IndexHighLow", "mention", "abs_open_open", 
                       "Monday", "Tuesday", "Wednesday", "Thursday",
                       "own_sec_mention", "own_sec_mention_notme", 
                       "abs_shock","abs_control_shock", "rel_shock", "rel_control_shock" )]

temp_data$own_sec_news_abs <- 0 
temp_data$own_sec_news_abs_c <- 0 
temp_data$own_sec_news_rel <- 0 
temp_data$own_sec_news_rel_c <- 0 
temp_data$own_sec_news_abs_notme <- 0 
temp_data$own_sec_news_abs_c_notme <- 0 
temp_data$own_sec_news_rel_notme <- 0 
temp_data$own_sec_news_rel_c_notme <- 0 

temp_data$own_sec_abs_intra <- 0 
temp_data$own_sec_highlow <- 0 
temp_data$own_sec_abs_intra_notme <- 0 
temp_data$own_sec_highlow_notme <- 0 

temp_data$N_sector <- 0 


cross_variables <- colnames(temp_data)
print(cross_variables)
cross_firm_news <- temp_data[0,]

temp_data$Date <- as.Date(temp_data$Date)
all_dates <- unique(temp_data$Date)
for(i in 1:length(all_dates)){
  if (i %% 100 == 0){
    print(all_dates[i])
  }
  d <- all_dates[i]
  today_data <- temp_data[which(temp_data$Date == d),]
  for (s in all_sectors){
    sector_day_data <- today_data[which(today_data$Sector == s),]
    
    N_sector <- nrow(sector_day_data)
    
    sector_news_abs_intra <- sum(as.numeric(sector_day_data$abs_intra_day), na.rm = TRUE)/N_sector
    sector_news_highlow <- sum(as.numeric(sector_day_data$highlow), na.rm = TRUE)/N_sector
    
    # abs_intra
    command1 <- paste("today_data[which(today_data$Sector == \"", s, 
                      "\"),\"own_sec_abs_intra\"] <- sector_news_abs_intra", sep = "")
    eval(parse(text=command1))
    
    # highlow
    command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                      "\"),\"own_sec_highlow\"] <- sector_news_highlow", sep = "")
    eval(parse(text=command2))
    
    # N_sector
    command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                      "\"),\"N_sector\"] <- N_sector", sep = "")
    eval(parse(text=command2))
    
    
    if (any(sector_day_data$mention == 1)){
      #sector_news_abs <- sum(as.numeric(sector_day_data[which(sector_day_data$both_mention ==1),
      #                                              ]$abs_shock), na.rm = TRUE)
      
      sector_news_abs <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                    ]$abs_shock), na.rm = TRUE)/N_sector
      sector_news_abs_c <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                          ]$abs_control_shock), na.rm = TRUE)/N_sector
      sector_news_rel <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                        ]$rel_shock), na.rm = TRUE)/N_sector
      sector_news_rel_c <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                          ]$rel_control_shock), na.rm = TRUE)/N_sector
      
      # abs_shock
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_abs\"] <- sector_news_abs", sep = "")
      eval(parse(text=command2))
      
      # abs_shock_c
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_abs_c\"] <- sector_news_abs_c", sep = "")
      eval(parse(text=command2))
      
      # rel_shock
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_rel\"] <- sector_news_rel", sep = "")
      eval(parse(text=command2))
      
      # rel_shock_c
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_rel_c\"] <- sector_news_rel_c", sep = "")
      eval(parse(text=command2))
      
    }
    
  }
  
  cross_firm_news <- rbind(cross_firm_news, today_data)
}

#### REMEMBER TO PUT BACK IN THE NA SECTOR ONES


cross_firm_news$own_sec_news_abs_notme <- 
  cross_firm_news$own_sec_news_abs - cross_firm_news$abs_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_abs_c_notme <- 
  cross_firm_news$own_sec_news_abs_c - cross_firm_news$abs_control_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_rel_notme <- 
  cross_firm_news$own_sec_news_rel - cross_firm_news$rel_shock/cross_firm_news$N_sector
cross_firm_news$own_sec_news_rel_c_notme <- 
  cross_firm_news$own_sec_news_rel_c - cross_firm_news$rel_control_shock/cross_firm_news$N_sector

cross_firm_news$own_sec_abs_intra_notme <- 
  cross_firm_news$own_sec_abs_intra - cross_firm_news$abs_intra_day/cross_firm_news$N_sector
cross_firm_news$own_sec_highlow_notme <- 
  cross_firm_news$own_sec_highlow - cross_firm_news$highlow/cross_firm_news$N_sector


cross_firm_news <- as.data.frame(cross_firm_news)
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))


























# Merge back in the DoW dummies that were annoyingly excluded (shouldn't need this anymore)
#dow_dummies <- all_data[, c("Date", "Code", "Monday", "Tuesday", "Wednesday", "Thursday")]
#cross_firm_news <- merge(cross_firm_news, dow_dummies, by = c("Date", "Code"))
cross_firm_news <- cross_firm_news[which(!is.na(cross_firm_news$Sector)),]

cross_firm_news <- cross_firm_news[which(!is.na(cross_firm_news$Sector)),]
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_new3.csv", sep = "/")
write.csv(cross_firm_news, file = clean_filename, row.names = FALSE)
# cross_firm_news <- read.csv(clean_filename, stringsAsFactors = FALSE)
# cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))
cross_firm_news <- cross_firm_news[which(!is.na(cross_firm_news$Sector)),]


test = plm(abs_intra_day ~ abs_shock
                  , data = cross_firm_news, 
                  index = c("Code", "Date"), model = "within")
summary(test)



# Compare the within sector effect for different options


# Dummies
abs_intra1 = felm(abs_intra_day ~ mention| Code, data = cross_firm_news)
summary(abs_intra1)
abs_intra2 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(abs_intra2)
abs_intra3 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code, data = cross_firm_news)
summary(abs_intra3)
abs_intra4 = felm(abs_intra_day ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_news)
summary(abs_intra4)
abs_intra5 = felm(abs_intra_day ~ mention + own_sec_mention_notme + plm::lag(abs_open_open, 0:10)| Code +Date, data = cross_firm_news)
summary(abs_intra5)

highlow1 = felm(highlow ~ mention| Code, data = cross_firm_news)
summary(highlow1)
highlow2 = felm(highlow ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(highlow2)
highlow3 = felm(highlow ~ mention + own_sec_mention_notme| Code, data = cross_firm_news)
summary(highlow3)
highlow4 = felm(highlow ~ mention + own_sec_mention_notme| Code + Date, data = cross_firm_news)
summary(highlow4)
highlow5 = felm(highlow ~ mention + own_sec_mention_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(highlow5)

mention_ownsec_table <- stargazer(abs_intra1, abs_intra2, abs_intra3, abs_intra4, abs_intra5,
                                  highlow1, highlow2, highlow3, highlow4, highlow5, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "Mention own sector effects")



# Absolute sLDA shocks
abs_intra1 = felm(abs_intra_day ~ abs_control_shock| Code, data = cross_firm_news)
summary(abs_intra1)
abs_intra2 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c| Code, data = cross_firm_news)
summary(abs_intra2)
abs_intra3 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(abs_intra3)
abs_intra4 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code + Date, data = cross_firm_news)
summary(abs_intra4)
abs_intra5 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(abs_intra5)

highlow1 = felm(highlow ~ abs_control_shock| Code, data = cross_firm_news)
summary(highlow1)
highlow2 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c| Code, data = cross_firm_news)
summary(highlow2)
highlow3 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(highlow3)
highlow4 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme| Code + Date, data = cross_firm_news)
summary(highlow4)
highlow5 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(highlow5)

slda_ownsec_table <- stargazer(abs_intra1, abs_intra2, abs_intra3, abs_intra4, abs_intra5,
                                  highlow1, highlow2, highlow3, highlow4, highlow5, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "sLDA own sector effects")



# Include reduced form sector level volatility
abs_intra1 = felm(abs_intra_day ~ abs_control_shock + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(abs_intra1)
abs_intra2 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(abs_intra2)
abs_intra3 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(abs_intra3)
abs_intra4 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme| Code + Date, data = cross_firm_news)
summary(abs_intra4)
abs_intra5 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme+ own_sec_abs_intra_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(abs_intra5)

highlow1 = felm(highlow ~ abs_control_shock + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow1)
highlow2 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow2)
highlow3 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow3)
highlow4 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme + own_sec_highlow_notme| Code + Date, data = cross_firm_news)
summary(highlow4)
highlow5 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme + own_sec_highlow_notme + plm::lag(highlow, 1:10)| Code +Date, data = cross_firm_news)
summary(highlow5)

slda_control_ownsec_table <- stargazer(abs_intra1, abs_intra2, abs_intra3, abs_intra4, abs_intra5,
                                  highlow1, highlow2, highlow3, highlow4, highlow5, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "sLDA own sector effects, controlling for sector movements.")


# Include reduced form sector level volatility
abs_intra1 = felm(abs_intra_day ~ mention + own_sec_mention| Code, data = cross_firm_news)
summary(abs_intra1)
abs_intra2 = felm(abs_intra_day ~ mention + own_sec_mention + own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(abs_intra2)
abs_intra3 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code, data = cross_firm_news)
summary(abs_intra3)
abs_intra4 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme| Code + Date, data = cross_firm_news)
summary(abs_intra4)
abs_intra5 = felm(abs_intra_day ~ mention + own_sec_mention_notme+ own_sec_abs_intra_notme + plm::lag(abs_open_open, 0:10)| Code, data = cross_firm_news)
summary(abs_intra5)

highlow1 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow1)
highlow2 = felm(highlow ~ mention + own_sec_mention + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow2)
highlow3 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code, data = cross_firm_news)
summary(highlow3)
highlow4 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme| Code + Date, data = cross_firm_news)
summary(highlow4)
highlow5 = felm(highlow ~ mention + own_sec_mention_notme + own_sec_highlow_notme + plm::lag(highlow, 1:10)| Code, data = cross_firm_news)
summary(highlow5)

slda_control_ownsec_table <- stargazer(abs_intra1, abs_intra2, abs_intra3, abs_intra4, abs_intra5,
                                       highlow1, highlow2, highlow3, highlow4, highlow5, column.sep.width = "1pt",
                                       table.placement = "H", df = FALSE, title = "Mention own sector effects, controlling for sector movements.")






# Compare the persistence of different effects
abs_intra1 = felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(abs_intra1)
abs_intra2 = felm(abs_intra_day ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3)| Code, data = cross_firm_news)
summary(abs_intra2)
abs_intra3 = felm(abs_intra_day ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3) + 
                    plm::lag(own_sec_abs_intra_notme, 0:3) + 
                    plm::lag(abs_open_open, 0:10)| Code + Date, data = cross_firm_news)
summary(abs_intra3)

highlow1 = felm(highlow ~ abs_control_shock + own_sec_news_abs_c_notme| Code, data = cross_firm_news)
summary(highlow1)
highlow2 = felm(highlow ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3)| Code, data = cross_firm_news)
summary(highlow2)
highlow3 = felm(highlow ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3) + 
                  plm::lag(own_sec_highlow_notme, 0:3) + 
                  plm::lag(highlow, 1:10)| Code + Date, data = cross_firm_news)
summary(highlow3)

slda_persistence_ownsec_table <- stargazer(abs_intra1, abs_intra2, abs_intra3, highlow1, highlow2, highlow3, 
                                           column.sep.width = "1pt",table.placement = "H",
                                           df = FALSE, title = "sLDA own sector effects, controlling for sector movements.")









