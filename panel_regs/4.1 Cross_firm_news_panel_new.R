setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)
require(reshape2)




clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_new3.csv", sep = "/")
cross_firm_news <- read.csv(clean_filename, stringsAsFactors = FALSE)
cross_firm_news$Date <- as.Date(cross_firm_news$Date)
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "Date"))
cross_firm_news <- cross_firm_news[which(!is.na(cross_firm_news$Sector)),]



all_sectors <- unique(cross_firm_news$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]


test <- plm(abs_intra_day ~ mention , data = cross_firm_news,
                    index = c("Code", "Date"), model = "within")
summary(test)
test = felm(highlow ~ plm::lag(abs_control_shock, 0:3) + plm::lag(own_sec_news_abs_c_notme, 0:3) + 
                  plm::lag(own_sec_highlow_notme, 0:3) + 
                  plm::lag(highlow, 1:10)| Code + Date, data = cross_firm_news)
summary(test)



temp_data <- cross_firm_news
temp_data_mention <- cross_firm_news
temp_data$Date <- as.character(temp_data$Date)
temp_data_mention$Date <- as.character(temp_data_mention$Date)


# Get separate variables for sector-by-sector shocks
for (s in all_sectors){
  print(s)
  sector_data <- cross_firm_news[which(cross_firm_news$Sector == s),]
  dates <- sector_data$Date
  
  sector_shocks <- aggregate(sector_data$abs_control_shock, by=list(sector_data$Date), mean)
  sector_mention <- aggregate(sector_data$mention, by=list(sector_data$Date), mean)
  
  colnames(sector_shocks) <- (c("Date", paste0("Sector", s,"_shock")))
  colnames(sector_mention) <- (c("Date", paste0("Sector", s,"_mention")))
  
  sector_shocks$Date <- as.character(sector_shocks$Date)
  sector_mention$Date <- as.character(sector_mention$Date)
  
  
  temp_data <- merge(temp_data, sector_shocks, by = "Date", all.x = TRUE)
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_shock)),]$Sector",s ,"_shock <- 0", sep = "")
  eval(parse(text=command))
  
  temp_data_mention <- merge(temp_data_mention, sector_mention, by = "Date", all.x = TRUE)
  command <- paste("temp_data_mention[which(is.na(temp_data_mention$Sector", s, "_mention)),]$Sector",s 
                   ,"_mention <- 0", sep = "")
  eval(parse(text=command))
  
}


clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_news_data.csv", sep = "/")
write.csv(temp_data, file = clean_filename, row.names = FALSE)
# temp_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_mention_data.csv", sep = "/")
write.csv(temp_data_mention, file = clean_filename, row.names = FALSE)
# temp_data_mention <- read.csv(clean_filename, stringsAsFactors = FALSE)


### Remove time and firm fixed effects and store residuals in dataframe

# First for the highlow volatility measure
temp_data <- pdata.frame(temp_data, index = c("Code", "Date"))
highlow_model <- felm(highlow ~ plm::lag(highlow, 1:10) |Code + Date, data = temp_data)
highlow_resids <- highlow_model$residuals
highlow_resid_data <- cbind(as.character(highlow_model$fe$Code), 
                             as.character(highlow_model$fe$Date), as.numeric(highlow_resids))
highlow_resid_data <- as.data.frame(highlow_resid_data, stringsAsFactors = FALSE)
colnames(highlow_resid_data)  <- c("Code", "Date", "highlow_resids")
highlow_resid_data$highlow_resids <- as.numeric(highlow_resid_data$highlow_resids)
temp_data$Date <- as.character(temp_data$Date); temp_data$Code <- as.character(temp_data$Code);
temp_data <- merge(temp_data, highlow_resid_data, by = c("Code", "Date"), all.x = TRUE)

# Then for the absolute change measure
temp_data <- pdata.frame(temp_data, index = c("Code", "Date"))
abs_model <- felm(abs_intra_day ~ plm::lag(abs_open_open, 1:10) |Code + Date, data = temp_data)
abs_resids <- abs_model$residuals
abs_resid_data <- cbind(as.character(abs_model$fe$Code), 
                            as.character(abs_model$fe$Date), as.numeric(abs_resids))
abs_resid_data <- as.data.frame(abs_resid_data, stringsAsFactors = FALSE)
colnames(abs_resid_data)  <- c("Code", "Date", "abs_resids")
abs_resid_data$abs_resids <- as.numeric(abs_resid_data$abs_resids)
temp_data$Date <- as.character(temp_data$Date); temp_data$Code <- as.character(temp_data$Code);
temp_data <- merge(temp_data, abs_resid_data, by = c("Code", "Date"), all.x = TRUE)





test <- plm(highlow_resids ~ abs_control_shock, index = c("Code", "Date"), data = temp_data)
summary(test)
test <- plm(abs_resids ~ abs_control_shock, index = c("Code", "Date"), data = temp_data)
summary(test)


highlow_coefficient_data <- as.data.frame(paste0("Sector_", all_sectors))
colnames(highlow_coefficient_data) <- "Responding_Sector"
abs_coefficient_data <- highlow_coefficient_data

for (s in all_sectors){
  print(s)
  sector_data <- temp_data[which(temp_data$Sector == s),]
  
  
  command <- paste("sector_model <- lm(fixed_effect_resids ~ 0 + ", paste(colnames(temp_data)[37:83], collapse = " + "), 
                   ", data = sector_data)", sep = "")
  eval(parse(text=command))
  
  coeffs <- summary(sector_model)$coefficients[,4]
  pvalues <- summary(sector_model)$coefficients[,4]
  command <- paste0("coefficient_data$Sector_", s, "news <- sector_model$coefficients")
  eval(parse(text=command))
  
  print(mean(sector_model$coefficients, na.rm = TRUE))
  
}
rownames(coefficient_data) <- coefficient_data$Responding_Sector
coefficient_data <- coefficient_data[,2:48]

# Remove any rows that are all NAs, by checking the diagonal
diagonals <- diag(as.matrix(coefficient_data))
coefficient_data <- coefficient_data[which(!is.na(diagonals)), which(!is.na(diagonals))]
coefficient_data <- as.matrix(coefficient_data)


### Plot a matrix of these cross-sector coefficients

# Turn the matrix into long form
longData<-melt(coefficient_data)

# Plot the nonzero values of the matrix
ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient(low="grey90", high="red") +
  labs(x="shocked sector", y="Responding sector", title="Matrix") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=90, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))









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


model1 = plm(highlow ~ mention + own_sec_mention_notme, data = cross_firm_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(highlow ~ mention + own_sec_mention_notme + abs_intra_day + 
               plm::lag(abs_open_open, 0:10), data = cross_firm_data, 
             index = c("Code", "Date"), model = "within")
summary(model2)


Cross_firm_mention_table <- stargazer(model1, model2, table.placement = "H", df = FALSE,
                                      title = "Cross-firm effects with mention dummies")

# How many companies in each sector?
companies <- unique(cross_firm_news[,c("Code", "Sector")])
table(companies$Sector)
mean(table(companies$Sector))




# Look at the effect of each firmall_data$Code_chr <- as.character(all_data$Code)
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




