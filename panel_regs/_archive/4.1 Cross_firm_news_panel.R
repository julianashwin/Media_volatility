setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_absreltopics_shocks.csv", sep = "/")
all_data <- read.csv(import_filename, stringsAsFactors = FALSE)

all_data <- pdata.frame(all_data, index = c("Code", "Date"))


# Choose the granularity of the Sector variable
all_data$Sector <- as.character(all_data$Business_Sector)
all_data$Sector[which(all_data$Business_Sector == "cut-off")] <- NA
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



####### Get some cross firm, within sector effects, by cycling through each date

# Cut down the size of the data to speed up the super inefficient loop
temp_data <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day",
                       "abspChange", "pChange", "Index_abs_Change", 
                       "Index_Change", "mention", "open_open", "abs_open_open",
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

# Merge back in the DoW dummies that were annoyingly excluded (shouldn't need this anymore)
#dow_dummies <- all_data[, c("Date", "Code", "Monday", "Tuesday", "Wednesday", "Thursday")]
#cross_firm_data <- merge(cross_firm_data, dow_dummies, by = c("Date", "Code"))


model1 = plm(abs_intra_day ~ mention +
                      own_sec_mention_notme
                    , data = cross_firm_data, 
                    index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(abs_intra_day ~ mention + own_sec_mention_notme +
               plm::lag(abs_open_open, 0:4) + plm::lag(Index_abs_Change, 0:4) +
               Monday + Tuesday + Wednesday + Thursday , data = cross_firm_data, 
             index = c("Code", "Date"), model = "within")
summary(model2)
model3 = plm(abspChange ~ mention +
               own_sec_mention_notme
             , data = cross_firm_data, 
             index = c("Code", "Date"), model = "within")
summary(model3)
model4 = plm(abspChange ~ mention +
               own_sec_mention_notme  +
               plm::lag(abspChange, 1:4) + 
               plm::lag(Index_abs_Change, 0:4) +
               Monday + Tuesday + Wednesday + Thursday
             , data = cross_firm_data, 
             index = c("Code", "Date"), model = "within")
summary(model4)


Cross_firm_mention_table <- stargazer(model1, model2, model3, model4,
                                           table.placement = "H", df = FALSE,
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
temp_data <- all_data[,c("Date", "Code", "Company", "Sector", "abs_intra_day",
                       "abspChange", "pChange", "Index_abs_Change",
                       "Index_Change", "mention", "open_open", "Monday", "Tuesday", "Wednesday", "Thursday",
                       "own_sec_mention", "own_sec_mention_notme",
                       "abs_open_open", "abs_shock","abs_control_shock", "rel_shock", "rel_control_shock" )]

temp_data$own_sec_news_abs <- 0 
temp_data$own_sec_news_abs_c <- 0 
temp_data$own_sec_news_rel <- 0 
temp_data$own_sec_news_rel_c <- 0 
temp_data$own_sec_news_abs_notme <- 0 
temp_data$own_sec_news_abs_c_notme <- 0 
temp_data$own_sec_news_rel_notme <- 0 
temp_data$own_sec_news_rel_c_notme <- 0 


for (s in all_sectors){
  command <- paste("temp_data$Sec", s, 
                   "_news_abs <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data$Sec", s, 
                   "_news_abs_c <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data$Sec", s, 
                   "_news_rel <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data$Sec", s, 
                   "_news_rel_c <- 0", sep = "")
  eval(parse(text=command))
}
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
    
    if (any(sector_day_data$mention == 1)){
      #sector_news_abs <- sum(as.numeric(sector_day_data[which(sector_day_data$both_mention ==1),
      #                                              ]$abs_shock), na.rm = TRUE)
      
      sector_news_abs <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                    ]$abs_shock), na.rm = TRUE)
      sector_news_abs_c <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                          ]$abs_control_shock), na.rm = TRUE)
      sector_news_rel <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                        ]$rel_shock), na.rm = TRUE)
      sector_news_rel_c <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                          ]$rel_control_shock), na.rm = TRUE)
      
      # abs_shock
      command1 <- paste("today_data$Sec", s, 
                        "_news_abs <- sector_news_abs", sep = "")
      eval(parse(text=command1))
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_abs\"] <- sector_news_abs", sep = "")
      eval(parse(text=command2))
      
      # abs_shock_c
      command1 <- paste("today_data$Sec", s, 
                        "_news_abs_c <- sector_news_abs_c", sep = "")
      eval(parse(text=command1))
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_abs_c\"] <- sector_news_abs_c", sep = "")
      eval(parse(text=command2))
      
      # rel_shock
      command1 <- paste("today_data$Sec", s, 
                        "_news_rel <- sector_news_rel", sep = "")
      eval(parse(text=command1))
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_rel\"] <- sector_news_rel", sep = "")
      eval(parse(text=command2))
      
      # rel_shock_c
      command1 <- paste("today_data$Sec", s, 
                        "_news_rel_c <- sector_news_rel_c", sep = "")
      eval(parse(text=command1))
      command2 <- paste("today_data[which(today_data$Sector == \"", s, 
                        "\"),\"own_sec_news_rel_c\"] <- sector_news_rel_c", sep = "")
      eval(parse(text=command2))
      
    }
    
  }
  
  cross_firm_news <- rbind(cross_firm_news, today_data)
}

#### REMEMBER TO PUT BACK IN THE NA SECTOR ONES


cross_firm_news$own_sec_news_abs_notme <- 
  cross_firm_news$own_sec_news_abs - cross_firm_news$abs_shock
cross_firm_news$own_sec_news_abs_c_notme <- 
  cross_firm_news$own_sec_news_abs_c - cross_firm_news$abs_control_shock
cross_firm_news$own_sec_news_rel_notme <- 
  cross_firm_news$own_sec_news_rel - cross_firm_news$rel_shock
cross_firm_news$own_sec_news_rel_c_notme <- 
  cross_firm_news$own_sec_news_rel_c - cross_firm_news$rel_control_shock

cross_firm_news <- as.data.frame(cross_firm_news)
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))



# Merge back in the DoW dummies that were annoyingly excluded (shouldn't need this anymore)
#dow_dummies <- all_data[, c("Date", "Code", "Monday", "Tuesday", "Wednesday", "Thursday")]
#cross_firm_news <- merge(cross_firm_news, dow_dummies, by = c("Date", "Code"))


clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_new.csv", sep = "/")
write.csv(cross_firm_news, file = clean_filename, row.names = FALSE)
# cross_firm_news <- read.csv(clean_filename, stringsAsFactors = FALSE)
# cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))


test = plm(abs_intra_day ~ abs_shock
                  , data = cross_firm_news, 
                  index = c("Code", "Date"), model = "within")
summary(test)


own_sec_abs = plm(abs_intra_day ~ abs_shock + own_sec_news_abs_notme
                    , data = cross_firm_news, 
                    index = c("Code", "Date"), model = "within")
summary(own_sec_abs)
own_sec_abs_c = plm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme + 
                    plm::lag(abs_open_open, 0:4) + 
                    plm::lag(Index_abs_Change, 0:4) +
                    Monday + Tuesday + Wednesday + Thursday, data = cross_firm_news, 
                    index = c("Code", "Date"), model = "within")
summary(own_sec_abs_c)
own_sec_rel = plm(pChange ~ rel_shock + own_sec_news_rel_notme
                  , data = cross_firm_news, 
                  index = c("Code", "Date"), model = "within")
summary(own_sec_rel)
own_sec_rel_c = plm(pChange ~ rel_control_shock + own_sec_news_rel_c_notme+
                    plm::lag(pChange, 1:4) + 
                    plm::lag(Index_Change, 0:4)+
                      Monday + Tuesday + Wednesday + Thursday
                    , data = cross_firm_news, 
                    index = c("Code", "Date"), model = "within")
summary(own_sec_rel_c)



stargazer(own_sec_abs, own_sec_abs_c, own_sec_rel, own_sec_rel_c,
          table.placement = "H", df = FALSE,
          title = "sLDA own sector effects")

colnames(cross_firm_news)
all_sector_dummies <- plm(abspChange ~ abs_shock + own_sec_news_abs_notme
                          + FinancialServices_news_abs + Banks_news_abs
                          + ElectronicElectricalEquipment_news_abs
                          + Tobacco_news_abs
                          , data = cross_firm_news, 
                          index = c("Code", "Date"), model = "within")
summary(all_sector_dummies)

stargazer(all_sector_dummies)





# How many companies in each sector?
companies <- unique(cross_firm_news[,c("Code", "Sector")])
table(companies$Sector)
mean(table(companies$Sector))




# Look at the effect of each firm
all_data$Code_chr <- as.character(all_data$Code)
all_data$Code_chr <- str_replace(all_data$Code_chr, "\\.", "d")
all_data$Code_chr <- str_replace(all_data$Code_chr, "\\^", "u")
for (c in unique(all_data$Code_chr)){
  print(c)
}
all_codes <- unique(all_data$Code_chr)

temp_data <- all_data[,c("Date", "Code", "Code_chr", "Company", "Sector", "abs_intra_day",
                         "abspChange", "pChange", "Index_abs_Change",
                         "Index_Change", "mention", "open_open", "Monday", "Tuesday", "Wednesday", "Thursday",
                         "abs_open_open", "abs_shock","abs_control_shock", "rel_shock", "rel_control_shock" )]



for (c in all_codes){
  command <- paste("temp_data$", c, 
                   "_news_abs_c <- 0", sep = "")
  eval(parse(text=command))
}
cross_variables <- colnames(temp_data)
print(cross_variables)
cross_all_firm_news <- temp_data[0,]

temp_data$Date <- as.Date(temp_data$Date)
all_dates <- unique(temp_data$Date)
for(i in 1:length(all_dates)){
  if (i %% 100 == 0){
    print(all_dates[i])
  }
  d <- all_dates[i]
  today_data <- temp_data[which(temp_data$Date == d),]
  for (c in all_codes){
    sector_day_data <- today_data[which(today_data$Code_chr == c),]
    
    if (any(sector_day_data$mention == 1)){
      #sector_news_abs <- sum(as.numeric(sector_day_data[which(sector_day_data$both_mention ==1),
      #                                              ]$abs_shock), na.rm = TRUE)
      
      firm_news_abs_c <- sum(as.numeric(sector_day_data[which(sector_day_data$mention ==1),
                                                          ]$abs_control_shock), na.rm = TRUE)
      
      # abs_shock_c
      command1 <- paste("today_data$", c, 
                        "_news_abs_c <- firm_news_abs_c", sep = "")
      eval(parse(text=command1))
      
    }
    
  }
  
  cross_all_firm_news <- rbind(cross_all_firm_news, today_data)
}




