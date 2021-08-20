setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)
require(reshape2)




clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_mentions_data.csv", sep = "/")
cross_firm_news <- read.csv(clean_filename, stringsAsFactors = FALSE)
cross_firm_news$period <- as.integer(as.factor(cross_firm_news$Date))
cross_firm_news <- pdata.frame(cross_firm_news, index = c("Code", "period"))
cross_firm_news$Date <- as.Date(cross_firm_news$Date)
cross_firm_news <- cross_firm_news[which(!is.na(cross_firm_news$Sector)),]


# Import the input-output tables 
import_filename <- paste0(clean_dir, "/UK_macro/input-output/ONS_mat_labels.csv")
matrix_labels <-read.csv(import_filename, stringsAsFactors = FALSE)

matrix_labels$x <- str_replace_all(matrix_labels$x, " ", "_")
matrix_labels$x <- str_replace_all(matrix_labels$x, "\\&", "and")
matrix_labels$x <- str_replace_all(matrix_labels$x, "\\.", "_")
matrix_labels$x <- str_replace_all(matrix_labels$x, "\\,", "_")
matrix_labels$x <- str_replace_all(matrix_labels$x, "-", "to")

# Adjust this category as it doesn't currently match
matrix_labels[which(matrix_labels$x == "41__42__and_43"),] <- "41__42_and_43"


all_sectors <- unique(cross_firm_news$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]
all_sectors <- as.character(all_sectors)

all(all_sectors %in% matrix_labels$x)

test <- felm(highlow ~ mention | Code + Date, data = all_data)
summary(test)
test <- felm(highlow ~ mention + abs_intra_day + VI_put + plm::lag(highlow, 1:10) | Code + Date, data = all_data)
summary(test)


temp_data <- all_data
temp_data_mention_avg <- all_data
temp_data$Date <- as.character(temp_data$Date)
temp_data_mention_avg$Date <- as.character(temp_data_mention_avg$Date)

# Get separate variables for sector-by-sector shocks
for (s in matrix_labels$x){
  print(s)
  sector_data <- all_data[which(all_data$Sector == s),]
  
  if (nrow(sector_data) > 0){
    sector_shocks <- aggregate(sector_data[,c("highlow_control_shock_phi")],
                               by=list(sector_data$Date), mean)
    sector_mention_avg <- aggregate(sector_data$mention, by=list(sector_data$Date), mean)
  } else {
    
    ## If there are no observations, then this provides zeros (as the shock value is zero)
    sector_shocks <- data.frame(cbind(temp_data$Date[1:100], matrix(0, nrow = 100, ncol = 1)))
    sector_mention_avg <- data.frame(cbind(temp_data$Date[1:100], rep(0, 100)))
  }
  
  
  colnames(sector_shocks) <- (c("Date", paste0("Sector", s,"_shock")))
  colnames(sector_mention_avg) <- (c("Date", paste0("Sector", s,"_mention_avg")))
  
  
  sector_shocks$Date <- as.character(sector_shocks$Date)
  sector_mention_avg$Date <- as.character(sector_mention_avg$Date)
  
  temp_data <- merge(temp_data, sector_shocks, by = "Date", all.x = TRUE)
  command <- paste("if (any(is.na(temp_data$Sector", s, "_shock))){
                   temp_data[which(is.na(temp_data$Sector", s, "_shock)),]$Sector",s ,"_shock <- 0
                   }", sep = "")
  eval(parse(text=command))
  
  
  temp_data_mention_avg <- merge(temp_data_mention_avg, sector_mention_avg, by = "Date", all.x = TRUE)
  command <- paste("if (any(is.na(temp_data$Sector", s, "_mention_avg))){
                   temp_data_mention_avg[which(is.na(temp_data_mention_avg$Sector", s, "_mention_avg)),]$Sector",s 
                   ,"_mention_avg <- 0
                   }", sep = "")
  eval(parse(text=command))
  
}

test <- felm(highlow ~ mention + highlow_control_shock_phi + plm::lag(highlow, 1:10) | Code + Date, data = cross_firm_news)
summary(test)



temp_data <- pdata.frame(temp_data, index = c("Code", "period"))
temp_data_mention_avg <- pdata.frame(temp_data_mention_avg, index = c("Code", "period"))





clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_news_data_NACE_short.csv", sep = "/")
write.csv(temp_data, file = clean_filename, row.names = FALSE)
# temp_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_mention_avg_data_NACE_short.csv", sep = "/")
write.csv(temp_data_mention_avg, file = clean_filename, row.names = FALSE)
# temp_data_mention_avg <- read.csv(clean_filename, stringsAsFactors = FALSE)


start_col <- which(colnames(temp_data) == "Sector01_shock")
end_col <- which(colnames(temp_data) == "Sector97_shock")
cols <- seq(start_col, end_col)
temp_data_abs <- temp_data[,c(1:57,cols)]
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_absshock_data_NACE.csv", sep = "/")
write.csv(temp_data_abs, file = clean_filename, row.names = FALSE)
# temp_data_abs <- read.csv(clean_filename, stringsAsFactors = FALSE)

start_col <- which(colnames(temp_data) == "Sector01_highlow_c_shock")
end_col <- which(colnames(temp_data) == "Sector97_highlow_c_shock")
cols <- seq(start_col, end_col, 6)
temp_data_highlow <- temp_data[,c(1:57,cols)]
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_highlowshock_data_NACE.csv", sep = "/")
write.csv(temp_data_highlow, file = clean_filename, row.names = FALSE)
# temp_data_highlow <- read.csv(clean_filename, stringsAsFactors = FALSE)


# Seems to be a problem (with dof?) when a time fixed effect is included...
test <- felm(highlow ~ highlow_control_shock_phi + Sector66_shock| Code + Date , data = temp_data)
summary(test)
test <- felm(highlow ~  Sector66_shock| Code + Date, data = temp_data)
summary(test)

table(temp_data$Sector06_and_07_shock != 0)
table(temp_data$Sector06_and_07_shock != 0 & temp_data_mention_avg$Sector06_and_07_mention_avg != 0)
table(temp_data_mention_avg$Sector == "06_and_07")
table(temp_data$Sector06_and_07_shock != 0 & temp_data_mention_avg$Sector == "06_and_07")
table(temp_data$Sector06_and_07_shock != 0 & temp_data_mention_avg$Sector == "06_and_07" & 
        temp_data$own_sec_news_highlow_c_phi == temp_data$Sector06_and_07_shock)
View(temp_data[which(temp_data$Sector06_and_07_abs_c_shock != 0 & temp_data_mention$Sector == "06_and_07"),
               c("Date", "Code", "Sector", "abs_intra_day", "mention", "own_sec_news_abs_c",
                 "Sector06_and_07_abs_c_shock")])
# Differences due to sampling error


which(colnames(temp_data_mention_avg) == "Sector01_mention_avg")
which(colnames(temp_data) == "Sector01_shock")


import_filename <- paste0(clean_dir, "/UK_macro/input-output/average_supplier_matrix_intconsweighted.csv")
cons_matrix <- read.csv(import_filename)
colnames(cons_matrix) <- matrix_labels$x
rownames(cons_matrix) <- matrix_labels$x
import_filename <- paste0(clean_dir, "/UK_macro/input-output/average_consumer_matrix_intdemweighted.csv")
dem_matrix <- read.csv(import_filename)
dem_matrix[is.na(dem_matrix)] <- 0
colnames(dem_matrix) <- matrix_labels$x
rownames(dem_matrix) <- matrix_labels$x

nrow(dem_matrix)



# Some summary statistics for the input-output matrix
test_mat <- as.matrix(dem_matrix)
N <- nrow(dem_matrix)*nrow(test_mat)
N_offdiag <- N - nrow(test_mat)
N_diag <- nrow(test_mat)
total_diag <- sum(diag(test_mat))
total_offdiag <- sum(test_mat) - sum(diag(test_mat))

# How many zeros are there
zeros <- as.numeric(table(test_mat == 0)[2])

mean_offdiag_nonzero <- total_offdiag/(N_offdiag - zeros)
mean_diag <- total_diag/(N_diag)

# First populate with distribtion centered around the off diagonal nonzero mean
values <- runif(N, min = 0.000001, max = 2*mean_offdiag_nonzero)

# Then add the appropriate number of zeros
values[sample(1:N, (zeros), replace = FALSE)] <- 0
as.numeric(table(values == 0)[2])

placebo <- matrix(values, nrow = nrow(dem_matrix), ncol = ncol(dem_matrix))
diag(placebo) <- runif(N_diag, 0.000001, 2*mean_diag)


placebo <- test_mat[sample(nrow(test_mat)),]
#diag(placebo) <- diag(test_mat)

colnames(placebo) <- matrix_labels$x
rownames(placebo) <- matrix_labels$x




  

# Create a variable for the intermediate consumption weighted shock
temp_data$cons_weighted_shock <- 0
temp_data$dem_weighted_shock <- 0
temp_data$cons_weighted_mentions <- 0
temp_data$dem_weighted_mentions <- 0
temp_data$placebo_weighted_shock <- 0
temp_data$placebo_weighted_mentions <- 0

weighted_shocks <- temp_data[0,]

# First multiply by *columns* of the I-O matrix, to get the effect of shocks to suppliers
start_col <- which(colnames(temp_data) == paste0("Sector01_shock"))
end_col <- which(colnames(temp_data) == paste0("Sector97_shock"))
cols <- seq(start_col, end_col)

start_col_mention <- which(colnames(temp_data_mention_avg) == paste0("Sector01_mention_avg"))
end_col_mention <- which(colnames(temp_data_mention_avg) == paste0("Sector97_mention_avg"))
cols_mention <- seq(start_col, end_col)




for (i in 1:nrow(matrix_labels)){
  sector <- matrix_labels$x[i]
  var <- colnames(temp_data)[cols[i]]
  
  print(paste("Column", colnames(cons_matrix)[i], "and Row", rownames(cons_matrix)[i]))
  print(paste("Sector", sector, "in variable", var))
  
  sector_data <- temp_data[which(temp_data$Sector == sector),]
  sector_mention_data <- temp_data[which(temp_data$Sector == sector),]
  
  if (nrow(sector_data)> 0 ){
    
    # First the intermediate consumption 
    matrix_coeffs <- cons_matrix[,sector]
    
    shocks <- sector_data[,start_col:end_col]
    mentions <- sector_mention_data[,start_col_mention:end_col_mention]
    
    shocks <- sapply(shocks, function(x) {
      if(is.factor(x)) as.numeric(as.character(x)) else x
    })
    mentions <- sapply(mentions, function(x) {
      if(is.factor(x)) as.numeric(as.character(x)) else x
    })
    table(sapply(shocks, class))
    table(sapply(mentions, class))
    
    cons_shock <- shocks %*% matrix_coeffs
    sector_data$cons_weighted_shock <- cons_shock
    
    cons_mentions <- mentions %*% matrix_coeffs
    sector_data$cons_weighted_mentions <- cons_mentions
    
    # Now the intermediate demand 
    matrix_coeffs <- t(dem_matrix[sector,])
    
    dem_shock <- shocks %*% matrix_coeffs
    sector_data$dem_weighted_shock <- as.numeric(dem_shock)
    
    dem_mentions <- mentions %*% matrix_coeffs
    sector_data$dem_weighted_mentions <- as.numeric(dem_mentions)
  
    
    
    # Now the placebo matrix 
    matrix_coeffs <- (placebo[sector,])
    
    placebo_shock <- shocks %*% matrix_coeffs
    sector_data$placebo_weighted_shock <- as.numeric(placebo_shock)
    
    placebo_mentions <- mentions %*% matrix_coeffs
    sector_data$placebo_weighted_mentions <- as.numeric(placebo_mentions)
    
    weighted_shocks <- rbind(weighted_shocks, sector_data)
  }
  
}




weighted_shocks$dem_weighted_shock <- as.numeric(weighted_shocks$dem_weighted_shock)
weighted_shocks$dem_weighted_mentions <- as.numeric(weighted_shocks$dem_weighted_mentions)


# Some summary statistics for the input-output matrix
test_mat <- as.matrix(dem_matrix)
sum(diag(test_mat))
test_mat <- as.matrix(cons_matrix)
sum(diag(test_mat))

weighted_shocks <- pdata.frame(data.frame(weighted_shocks), index = c("Code", "period"))

weighted_shocks$pred_effect <- weighted_shocks$highlow_control_shock_phi/2
weighted_shocks$dem_weighted_shock <- weighted_shocks$dem_weighted_shock/2
weighted_shocks$cons_weighted_shock <- weighted_shocks$cons_weighted_shock/2
weighted_shocks$placebo_weighted_shock <- weighted_shocks$placebo_weighted_shock/2
weighted_shocks$own_sec_news_highlow_c_phi <- weighted_shocks$own_sec_news_highlow_c_phi/2

model1 <- felm(highlow ~ pred_effect  + plm::lag(highlow, 1:10) + own_sec_news_highlow_c_phi
               + dem_weighted_shock + cons_weighted_shock + placebo_weighted_shock| Code + Date,
               data = weighted_shocks)
summary(model1) 
model1 <- felm(highlow ~ pred_effect  + plm::lag(highlow, 1:10) 
               + dem_weighted_shock + cons_weighted_shock + placebo_weighted_shock| Code + Date,
               data = weighted_shocks)
summary(model1) 




# Run some test regressions
#regress_data <- weighted_shocks[,c("Code", "Date", "abs_intra_day", "abs_open_open", "dem_weighted_shock", "Sector",
#                                   "cons_weighted_shock", "abs_control_shock")]



model1 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(model1) 
model2 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put + cons_weighted_shock 
               + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(model2) 
model3 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put + dem_weighted_shock + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(model3) 
model4 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
               + dem_weighted_shock + cons_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(model4) 
model5 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
             + own_sec_news_highlow_c_phi
             + dem_weighted_shock + cons_weighted_shock
             + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(model5) 
model6 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
               + own_sec_news_highlow_c_phi + own_sec_highlow_notme
               + dem_weighted_shock + cons_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model6) 
model7 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
               + own_sec_news_highlow_c_phi + own_sec_highlow_notme
               + dem_weighted_shock + cons_weighted_shock + placebo_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model7) 
model8 <- felm(highlow ~ pred_effect 
               + own_sec_news_highlow_c_phi
               + dem_weighted_shock + cons_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model8) 


stargazer(model1, model4, model5, model6, model8, df = FALSE,
          title = "Spillover effect, with article variable", 
          table.placement = "H")





model1 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
               + placebo_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model1) 
model2 <- felm(highlow ~ pred_effect + 
               + placebo_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model2) 


stargazer(model1, model2, df = FALSE,
          title = "Spillover effect, with placebo matrices", 
          table.placement = "H")






model1 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model1) 
model2 <- felm(highlow ~ mention + abs_intra_day + VI_put + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model2) 

stargazer(model1, model2, df = FALSE,
          title = "The article effect and the mention dummy", 
          table.placement = "H")






daily_mean <- aggregate(weighted_shocks[,c("pred_effect", "mention", "IndexHighLow", "Index_abs_Change", 
                                           "VI_put")], 
                        by = list(weighted_shocks$Date), FUN = mean)
daily_mean$Date <- as.Date(daily_mean$Group.1)
daily_mean$Group.1 <- "FTSE"
daily_mean <- daily_mean[order(daily_mean$Date),]
daily_mean$period <- as.integer(as.factor(daily_mean$Date))
daily_mean <- pdata.frame(daily_mean, index = c("Group.1", "period"))

daily_mean$IndexHighLow_1lag <- plm::lag(daily_mean$IndexHighLow, 1)
daily_mean$IndexHighLow_2lag <- plm::lag(daily_mean$IndexHighLow, 2)
daily_mean$IndexHighLow_3lag <- plm::lag(daily_mean$IndexHighLow, 3)
daily_mean$IndexHighLow_4lag <- plm::lag(daily_mean$IndexHighLow, 4)
daily_mean$IndexHighLow_5lag <- plm::lag(daily_mean$IndexHighLow, 5)
daily_mean$IndexHighLow_6lag <- plm::lag(daily_mean$IndexHighLow, 6)
daily_mean$IndexHighLow_7lag <- plm::lag(daily_mean$IndexHighLow, 7)
daily_mean$IndexHighLow_8lag <- plm::lag(daily_mean$IndexHighLow, 8)
daily_mean$IndexHighLow_9lag <- plm::lag(daily_mean$IndexHighLow, 9)
daily_mean$IndexHighLow_10lag <- plm::lag(daily_mean$IndexHighLow, 10)


lm(IndexHighLow ~ lag(IndexHighLow, 1), data = daily_mean)

model1 <- lm(IndexHighLow ~ pred_effect, data = daily_mean)
summary(model1)
model2 <- lm(IndexHighLow ~ pred_effect
           + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
           , data = daily_mean)
summary(model2)
model3 <- lm(IndexHighLow ~ pred_effect
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             + IndexHighLow_9lag + IndexHighLow_10lag
             , data = daily_mean)
summary(model3)
model4 <- lm(IndexHighLow ~ pred_effect + Index_abs_Change 
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             + Monday + Tuesday + Wednesday + Thursday 
             , data = daily_mean)
summary(model4)
model5 <- lm(IndexHighLow ~ mention, data = daily_mean)
summary(model5)
model6 <- lm(IndexHighLow ~ mention
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             , data = daily_mean)
summary(model6)
model7 <- lm(IndexHighLow ~ mention
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             + IndexHighLow_9lag + IndexHighLow_10lag
             , data = daily_mean)
summary(model7)
model8 <- lm(IndexHighLow ~ mention + Index_abs_Change 
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             , data = daily_mean)
summary(model8)

stargazer(model5, model7, model8, df = FALSE,
          title = "The aggregate effects of firm-level media coverage", 
          table.placement = "H")







model1 <- felm(highlow ~ mention + abs_intra_day + VI_put + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model1) 
model2 <- felm(highlow ~ mention + abs_intra_day + VI_put + cons_weighted_mentions 
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model2) 
model3 <- felm(highlow ~ mention + abs_intra_day + VI_put + dem_weighted_mentions + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model3) 
model4 <- felm(highlow ~ mention + abs_intra_day + VI_put 
               + dem_weighted_mentions + cons_weighted_mentions
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model4) 
model5 <- felm(highlow ~ pred_effect + abs_intra_day + VI_put 
               + own_sec_mention_avg_notme
               + dem_weighted_mentions + cons_weighted_mentions
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model5) 
model6 <- felm(highlow ~ mention + abs_intra_day + VI_put 
               + own_sec_mention_avg_notme + own_sec_highlow_notme
               + dem_weighted_shock + cons_weighted_shock
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_shocks)
summary(model6) 

stargazer(model1, model2, model3, model4, model5, model6, df = FALSE,
          title = "Neither realised nor expected new information explain the media coverage effect", 
          table.placement = "H")












test <- felm(highlow ~ highlow_control_shock_phi + abs_intra_day + VI_put 
             + own_sec_news_highlow_c_phi
             + dem_weighted_shock + cons_weighted_shock
             + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(test) 


test <- felm(highlow ~ highlow_control_shock_phi + own_sec_news_highlow_c_phi_notme
             + dem_weighted_shock + cons_weighted_shock
             + plm::lag(highlow, 1:10) | Code + Date, 
             data = weighted_shocks)
summary(test) 



test <- felm(weighted_shocks$abs_intra_day ~ weighted_shocks$abs_control_shock + 
               weighted_shocks$cons_weighted_shock + plm::lag(weighted_shocks$abs_open_open, 0:5) | weighted_shocks$Code+ weighted_shocks$Date)
summary(test)
test <- felm(weighted_shocks$abs_intra_day ~ weighted_shocks$abs_control_shock +
               weighted_shocks_abs$dem_weighted_shock + plm::lag(weighted_shocks$abs_open_open, 0:5) | weighted_shocks$Code + weighted_shocks$Date)
summary(test)
test <- felm(weighted_shocks$abs_intra_day ~ weighted_shocks$abs_control_shock +weighted_shocks$cons_weighted_shock +
               weighted_shocks$dem_weighted_shock + plm::lag(weighted_shocks$abs_open_open, 0:5) | weighted_shocks$Code + weighted_shocks$Date)
summary(test)

weighted_shocks_abs$cons_weighted_shock_hl <- weighted_shocks_highlow$cons_weighted_shock
weighted_shocks_abs$dem_weighted_shock_hl <- weighted_shocks_highlow$dem_weighted_shock


test <- felm(highlow ~ highlow_control_shock + cons_weighted_shock_hl + plm::lag(highlow, 1:5) | Code + Date, 
             data = weighted_shocks_abs)
summary(test) 
test <- felm(highlow ~ highlow_control_shock_phi + cons_weighted_shock + plm::lag(highlow, 1:5) | Code + Date, 
             data = weighted_shocks_highlow)
summary(test) 



hl_shocks <- weighted_shocks_highlow[, c("Date", "Code", "cons_weighted_shock", "dem_weighted_shock")]
colnames(hl_shocks) <- c("Date", "Code", "cons_weighted_shock_hl", "dem_weighted_shock_hl")
hl_shocks$Date <- as.character(hl_shocks$Date)
hl_shocks$Code <- as.character(hl_shocks$Code)
weighted_shocks_abs$Date <- as.character(weighted_shocks_abs$Date)
weighted_shocks_abs$Code <- as.character(weighted_shocks_abs$Code)

weighted_shocks <- merge(weighted_shocks_abs, hl_shocks, by = c("Code", "Date"), all.x = TRUE)

weighted_shocks <- pdata.frame(weighted_shocks, index = c("Code", "Date"))




clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_I-O_NACE.csv", sep = "/")
write.csv(weighted_shocks, file = clean_filename, row.names = FALSE)
# weighted_shocks <- read.csv(clean_filename, stringsAsFactors = FALSE)
# weighted_shocks <- pdata.frame(weighted_shocks, index = c("Code", "Date"))














############################ abs intra day ############################ 


### Create a results table for weighted intermediate demand and consumption shocks on abs_intra_day
cons1 <- felm(abs_intra_day ~ abs_control_shock + cons_weighted_shock | Code + Date, data = weighted_shocks)
summary(cons1)
dem1 <- felm(abs_intra_day ~ abs_control_shock + dem_weighted_shock | Code + Date, data = weighted_shocks)
summary(dem1)
cons2 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_notme + 
               cons_weighted_shock | Code + Date, data = weighted_shocks)
summary(cons2)
dem2 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_notme + 
               dem_weighted_shock | Code + Date, data = weighted_shocks)
summary(dem2)
cons3 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme + 
                cons_weighted_shock + plm::lag(abs_open_open, 0:5)| Code + Date, data = weighted_shocks)
summary(cons3)
dem3 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_c_notme + 
               dem_weighted_shock + plm::lag(abs_open_open, 0:5) | Code + Date, data = weighted_shocks)
summary(dem3)


int_cons_table <- stargazer(cons1, cons2, cons3, column.sep.width = "1pt",
                                  table.placement = "H", df = FALSE, title = "Abs Intermediate consumption effects")

int_dem_table <- stargazer(dem1, dem2, dem3, column.sep.width = "1pt",
                            table.placement = "H", df = FALSE, title = "Abs Intermediate demand effects")



both1 <- felm(abs_intra_day ~ highlow_control_shock + cons_weighted_shock_hl + dem_weighted_shock_hl
              | Code + Date, data = weighted_shocks)
summary(both1)
both2 <- felm(abs_intra_day ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + dem_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(both2)
both3 <- felm(abs_intra_day ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + dem_weighted_shock_hl + plm::lag(abs_open_open, 0:10)| Code + Date, data = weighted_shocks)
summary(both3)


both_table <- stargazer(both1, both2, both3, column.sep.width = "1pt",
                           table.placement = "H", df = FALSE, title = "Up and downstream effects")





############################ highlow ############################ 


### Create a results table for weighted intermediate demand and consumption shocks on abs_intra_day
cons1 <- felm(highlow ~ highlow_control_shock + cons_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(cons1)
dem1 <- felm(highlow ~ highlow_control_shock + dem_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(dem1)
cons2 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(cons2)
dem2 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
               dem_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(dem2)
cons3 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + plm::lag(highlow, 1:5)| Code + Date, data = weighted_shocks)
summary(cons3)
dem3 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
               dem_weighted_shock_hl + plm::lag(highlow, 1:5) | Code + Date, data = weighted_shocks)
summary(dem3)
cons4 <- felm(highlow ~ abs_intra_day + highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + plm::lag(highlow, 1:5)| Code + Date, data = weighted_shocks)
summary(cons4)
dem4 <- felm(highlow ~ abs_intra_day + highlow_control_shock + own_sec_news_highlow_c_notme + 
               dem_weighted_shock_hl + plm::lag(highlow, 1:5) | Code + Date, data = weighted_shocks)
summary(dem4)


int_cons_table <- stargazer(cons1, cons2, cons3, cons4, column.sep.width = "1pt",
                            table.placement = "H", df = FALSE, title = "High-low Intermediate consumption effects")

int_dem_table <- stargazer(dem1, dem2, dem3, dem4, column.sep.width = "1pt",
                           table.placement = "H", df = FALSE, title = "High-low Intermediate demand effects")



both1 <- felm(highlow ~ highlow_control_shock + cons_weighted_shock_hl + dem_weighted_shock_hl
              | Code + Date, data = weighted_shocks)
summary(both1)
both2 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + dem_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(both2)
both3 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + dem_weighted_shock_hl + plm::lag(highlow, 1:10)| Code + Date, data = weighted_shocks)
summary(both3)
both4 <- felm(highlow ~ abs_intra_day + highlow_control_shock + own_sec_news_highlow_c_notme + 
                cons_weighted_shock_hl + dem_weighted_shock_hl + plm::lag(highlow, 1:10)| Code + Date, data = weighted_shocks)
summary(both4)

both_table <- stargazer(both1, both2, both3, both4, column.sep.width = "1pt",
                        table.placement = "H", df = FALSE, title = "High-low Up and downstream effects")


table<- stargazer(both1, cons3, dem3, both3, cons4, dem4, both4)




both4 <- felm(highlow ~  highlow_control_shock + own_sec_news_highlow_c_notme + 
                plm::lag(highlow, 1:10)| Code + Date, data = weighted_shocks)
summary(both4)
both4 <- felm(highlow ~ abs_intra_day + mention + own_sec_mention + own_sec_highlow_notme +
                plm::lag(highlow, 1:10)| Code + Date, data = weighted_shocks)
summary(both4)
both4 <- felm(highlow ~  highlow_control_shock + own_sec_news_highlow_c_notme + own_sec_highlow_notme +
                plm::lag(highlow, 1:10) + abs_intra_day| Code + Date, data = weighted_shocks)
summary(both4)




model1 <- felm(highlow ~  mention + own_sec_mention_avg_notme +
                 plm::lag(highlow, 1:10)| Code + Date, data = weighted_shocks)
model2 <- felm(highlow ~  mention + own_sec_mention_avg_notme + own_sec_highlow_notme +
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_shocks)
model3 <- felm(highlow ~  mention + own_sec_mention_avg_notme + own_sec_highlow_notme +
                 plm::lag(highlow, 1:10) + abs_intra_day| Code + Date, data = weighted_shocks)
model4 <- felm(highlow ~  highlow_control_shock_phi + own_sec_news_highlow_c_notme +
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_shocks)
model5 <- felm(highlow ~  highlow_control_shock_phi + own_sec_news_highlow_c_notme + own_sec_highlow_notme +
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_shocks)
model6 <- felm(highlow ~  highlow_control_shock_phi + own_sec_news_highlow_c_notme + own_sec_highlow_notme +
                 plm::lag(highlow, 1:10) + abs_intra_day| Code + Date, data = weighted_shocks)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = FALSE, title = "Within sector effects")



# Generate random I-O matrices as a sense check
temp_mat <- as.matrix(cons_matrix)
diagonals <- diag(temp_mat)
n_zeroes <- sum(temp_m)








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









# Get separate variables for sector-by-sector shocks
for (s in matrix_labels$x){
  print(s)
  sector_data <- cross_firm_news[which(cross_firm_news$Sector == s),]
  
  if (nrow(sector_data) > 0){
    sector_shocks <- aggregate(sector_data[,c("abs_shock", "abs_control_shock", "abs_control_shock_phi", "highlow_shock",
                                              "highlow_control_shock", "highlow_control_shock_phi")],
                               by=list(sector_data$Date), mean)
    sector_mention_avg <- aggregate(sector_data$mention, by=list(sector_data$Date), mean)
    sector_mention <- aggregate(sector_data$mention, by=list(sector_data$Date), max)
  } else {
    sector_shocks <- data.frame(cbind(temp_data$Date[1:100], matrix(0, nrow = 100, ncol = 6)))
    sector_mention_avg <- data.frame(cbind(temp_data$Date[1:100], rep(0, 100)))
    sector_mention <- data.frame(cbind(temp_data$Date[1:100], rep(0, 100)))
  }
  
  
  colnames(sector_shocks) <- (c("Date", paste0("Sector", s,"_abs_shock"), paste0("Sector", s,"_abs_c_shock"), 
                                paste0("Sector", s,"_abs_c_phi_shock"), paste0("Sector", s,"_highlow_shock"), 
                                paste0("Sector", s,"_highlow_c_shock"), paste0("Sector", s,"_highlow_c_phi_shock")))
  colnames(sector_mention) <- (c("Date", paste0("Sector", s,"_mention")))
  colnames(sector_mention_avg) <- (c("Date", paste0("Sector", s,"_mention_avg")))
  
  
  sector_shocks$Date <- as.character(sector_shocks$Date)
  sector_mention$Date <- as.character(sector_mention$Date)
  sector_mention_avg$Date <- as.character(sector_mention_avg$Date)
  
  temp_data <- merge(temp_data, sector_shocks, by = "Date", all.x = TRUE)
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_abs_shock)),]$Sector",s ,"_abs_shock <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_abs_c_shock)),]$Sector",s ,"_abs_c_shock <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_abs_c_phi_shock)),]$Sector",s ,"_abs_c_phi_shock <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_highlow_shock)),]$Sector",s ,"_highlow_shock <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_highlow_c_shock)),]$Sector",s ,"_highlow_c_shock <- 0", sep = "")
  eval(parse(text=command))
  command <- paste("temp_data[which(is.na(temp_data$Sector", s, "_highlow_c_phi_shock)),]$Sector",s ,"_highlow_c_phi_shock <- 0", sep = "")
  eval(parse(text=command))
  
  temp_data_mention <- merge(temp_data_mention, sector_mention, by = "Date", all.x = TRUE)
  command <- paste("temp_data_mention[which(is.na(temp_data_mention$Sector", s, "_mention)),]$Sector",s 
                   ,"_mention <- 0", sep = "")
  eval(parse(text=command))
  
  temp_data_mention_avg <- merge(temp_data_mention_avg, sector_mention_avg, by = "Date", all.x = TRUE)
  command <- paste("temp_data_mention_avg[which(is.na(temp_data_mention_avg$Sector", s, "_mention_avg)),]$Sector",s 
                   ,"_mention_avg <- 0", sep = "")
  eval(parse(text=command))
  
  
}

