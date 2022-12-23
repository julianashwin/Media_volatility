setwd("~/Documents/GitHub/Media_volatility")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)
require(reshape2)
require(stringr)



clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/cross_firm_mentions_data_NACE.csv", sep = "/")
cross_firm_data <- read.csv(clean_filename, stringsAsFactors = FALSE)
cross_firm_data$period <- as.integer(as.factor(cross_firm_data$Date))
cross_firm_data <- pdata.frame(cross_firm_data, index = c("Code", "period"))
cross_firm_data$Date <- as.Date(cross_firm_data$Date)
cross_firm_data <- cross_firm_data[which(!is.na(cross_firm_data$Sector)),]

all_data <-  cross_firm_data[,c("Date", "Sector", "Code", "abs_intra_day", "highlow", "VI_put", "VI_call", 
                                "Index_Change", "IndexHighLow", "mention", "N_sector", "own_sec_mention", 
                                "own_sec_mention_avg", "own_sec_mention_notme", "own_sec_mention_avg_notme",
                                "own_sec_abs_intra", "own_sec_highlow", "own_sec_abs_intra_notme", 
                                "own_sec_highlow_notme", "period")]


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


all_sectors <- unique(all_data$Sector)
all_sectors <- all_sectors[which(!is.na(all_sectors))]
all_sectors <- as.character(all_sectors)

all(all_sectors %in% matrix_labels$x)

test <- felm(highlow ~ mention | Code + Date, data = all_data)
summary(test)
test <- felm(highlow ~ mention + plm::lag(highlow, 1:10) + 
               plm::lag(VI_put,1:10) + plm::lag(VI_call,1:10) | Code + Date, data = all_data)
summary(test)


temp_data <- all_data
temp_data_mention_avg <- all_data
temp_data$Date <- as.character(temp_data$Date)
temp_data_mention_avg$Date <- as.character(temp_data_mention_avg$Date)

# Get separate variables for sector-by-sector shocks
s = matrix_labels$x[5]
for (s in matrix_labels$x){
  print(s)
  sector_data <- all_data[which(all_data$Sector == s),]
  
  if (nrow(sector_data) > 0){
    sector_mention <- aggregate(sector_data$mention, by=list(sector_data$Date), max)
    sector_mention_avg <- aggregate(sector_data$mention, by=list(sector_data$Date), mean)
  } else {
    # If there are no observations, then this provides zeros (as the shock value is zero)
    sector_mention <- data.frame(cbind(unique(temp_data$Date)[1:100], matrix(0, nrow = 100, ncol = 1)))
    sector_mention_avg <- data.frame(cbind(unique(temp_data$Date)[1:100], rep(0, 100)))
  }
  # Rename columns
  colnames(sector_mention) <- (c("Date", paste0("Sector", s,"_mention")))
  colnames(sector_mention_avg) <- (c("Date", paste0("Sector", s,"_mention_avg")))
  
  # Prep for merge
  sector_mention$Date <- as.character(sector_mention$Date)
  sector_mention_avg$Date <- as.character(sector_mention_avg$Date)
  temp_data$Date <- as.character(temp_data$Date)
  temp_data_mention_avg$Date <- as.character(temp_data_mention_avg$Date)
  
  # Merge in mentions
  temp_data <- merge(temp_data, sector_mention, by = "Date", all.x = TRUE)
  command <- paste("if (any(is.na(temp_data$Sector", s, "_mention))){
                   temp_data[which(is.na(temp_data$Sector", s, "_mention)),]$Sector",s ,"_mention <- 0
                   }", sep = "")
  eval(parse(text=command))
  
  
  temp_data_mention_avg <- merge(temp_data_mention_avg, sector_mention_avg, by = "Date", all.x = TRUE)
  command <- paste("if (any(is.na(temp_data_mention_avg$Sector", s, "_mention_avg))){
                   temp_data_mention_avg[which(is.na(temp_data_mention_avg$Sector", s, "_mention_avg)),]$Sector",s 
                   ,"_mention_avg <- 0
                   }", sep = "")
  eval(parse(text=command))
  
}




temp_data <- pdata.frame(temp_data, index = c("Code", "period"))
temp_data_mention_avg <- pdata.frame(temp_data_mention_avg, index = c("Code", "period"))


test <- felm(highlow ~ mention + own_sec_mention + plm::lag(highlow, 1:10) | Code + Date, 
             data = temp_data)
summary(test)


# Seems to be a problem (with dof?) when a time fixed effect is included...
test <- felm(highlow ~Sector66_mention| Code + Date , data = temp_data)
summary(test)
test <- felm(highlow ~  Sector66_mention_avg| Code + Date, data = temp_data_mention_avg)
summary(test)

table(temp_data$Sector06_and_07_mention != 0)
table(temp_data$Sector06_and_07_mention != 0 & temp_data_mention_avg$Sector06_and_07_mention_avg != 0)
table(temp_data_mention_avg$Sector == "06_and_07")
table(temp_data$Sector06_and_07_mention != 0 & temp_data_mention_avg$Sector == "06_and_07")
table(temp_data$Sector06_and_07_mention != 0 & temp_data_mention_avg$Sector == "06_and_07")

which(colnames(temp_data_mention_avg) == "Sector01_mention_avg")
which(colnames(temp_data) == "Sector01_mention")


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
temp_data$cons_weighted_mention <- 0
temp_data$dem_weighted_mention <- 0
temp_data_mention_avg$cons_weighted_mention_avg <- 0
temp_data_mention_avg$dem_weighted_mention_avg <- 0
temp_data$placebo_weighted_mention <- 0
temp_data_mention_avg$placebo_weighted_mention_avg <- 0

weighted_mention <- temp_data[0,]
weighted_mention_avg <- temp_data_mention_avg[0,][0,]

# First multiply by *columns* of the I-O matrix, to get the effect of shocks to suppliers
start_col_mention <- which(colnames(temp_data) == paste0("Sector01_mention"))
end_col_mention <- which(colnames(temp_data) == paste0("Sector97_mention"))
cols_mention <- seq(start_col_mention, end_col_mention)

start_col_mention_avg <- which(colnames(temp_data_mention_avg) == paste0("Sector01_mention_avg"))
end_col_mention_avg <- which(colnames(temp_data_mention_avg) == paste0("Sector97_mention_avg"))
cols_mention_avg <- seq(start_col_mention_avg, end_col_mention_avg)




for (i in 1:nrow(matrix_labels)){
  sector <- matrix_labels$x[i]
  var_mention <- colnames(temp_data)[cols_mention[i]]
  var_mention_avg <- colnames(temp_data_mention_avg)[cols_mention_avg[i]]
  
  stopifnot(str_split(var_mention, "_")[[1]][1] == str_split(var_mention_avg, "_")[[1]][1])
  print(paste("Column", colnames(cons_matrix)[i], "and Row", rownames(cons_matrix)[i]))
  print(paste("Sector", sector, "in variable", var_mention))
  
  sector_mention <- temp_data[which(temp_data$Sector == sector),]
  sector_mention_avg <- temp_data_mention_avg[which(temp_data_mention_avg$Sector == sector),]
  
  if (nrow(sector_mention)> 0 ){
    
    mentions <- sector_mention[,cols_mention]
    mention_avgs <- sector_mention_avg[,cols_mention_avg]
    
    mentions <- sapply(mentions, function(x) {
      if(is.character(x)) as.numeric(as.character(x)) else x
    })
    mention_avgs <- sapply(mention_avgs, function(x) {
      if(is.character(x)) as.numeric(as.character(x)) else x
    })
    #table(sapply(mentions, class))
    #table(sapply(mention_avgs, class))
    
    # First the intermediate consumption 
    matrix_coeffs <- cons_matrix[,sector]
    
    cons_mention <- mentions %*% matrix_coeffs
    sector_mention$cons_weighted_mention <- cons_mention
    
    cons_mention_avg <- mention_avgs %*% matrix_coeffs
    sector_mention_avg$cons_weighted_mention_avg <- cons_mention_avg
    
    # Now the intermediate demand 
    matrix_coeffs <- t(dem_matrix[sector,])
    
    dem_mention <- mentions %*% matrix_coeffs
    sector_mention$dem_weighted_mention <- dem_mention
    
    dem_mention_avg <- mention_avgs %*% matrix_coeffs
    sector_mention_avg$dem_weighted_mention_avg <- dem_mention_avg
  
    # Now the placebo matrix 
    matrix_coeffs <- (placebo[sector,])
    
    placebo_mention <- mentions %*% matrix_coeffs
    sector_mention$placebo_weighted_mention <- placebo_mention
    
    placebo_mention_avg <- mention_avgs %*% matrix_coeffs
    sector_mention_avg$placebo_weighted_mention_avg <- placebo_mention_avg
    
    weighted_mention <- rbind(weighted_mention, sector_mention)
    weighted_mention_avg <- rbind(weighted_mention_avg, sector_mention_avg)
  }
  
}

weighted_mention$dem_weighted_mention <- as.numeric(weighted_mention$dem_weighted_mention)
weighted_mention_avg$dem_weighted_mention_avg <- as.numeric(weighted_mention_avg$dem_weighted_mention_avg)

weighted_mention$cons_weighted_mention <- as.numeric(weighted_mention$cons_weighted_mention)
weighted_mention_avg$cons_weighted_mention_avg <- as.numeric(weighted_mention_avg$cons_weighted_mention_avg)



# Some summary statistics for the input-output matrix
test_mat <- as.matrix(dem_matrix)
sum(diag(test_mat))
test_mat <- as.matrix(cons_matrix)
sum(diag(test_mat))

weighted_mention_df <- pdata.frame(data.frame(weighted_mention), index = c("Code", "period"))
weighted_mention_avg_df <- pdata.frame(data.frame(weighted_mention_avg), index = c("Code", "period"))



# Baseline
model1 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call +  
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_mention_avg_df)
summary(model1) 
# Only cons
model2 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 cons_weighted_mention_avg | Code + Date, 
             data = weighted_mention_avg_df)
summary(model2) 
# Only dem
model3 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 dem_weighted_mention_avg | Code + Date, 
             data = weighted_mention_avg_df)
summary(model3) 
# Both
model4 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 dem_weighted_mention_avg + cons_weighted_mention_avg| Code + Date, 
             data = weighted_mention_avg_df)
summary(model4) 
# Both plus own sector
model5 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
             + own_sec_mention_avg_notme 
             + dem_weighted_mention_avg + cons_weighted_mention_avg | Code + Date, 
             data = weighted_mention_avg_df)
summary(model5) 
# Both, own sec mention and vol
model6 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
               + own_sec_mention_avg_notme + own_sec_highlow_notme +
               + dem_weighted_mention_avg + cons_weighted_mention_avg| Code + Date, 
               data = weighted_mention_avg_df)
summary(model6) 
# Chuck in a placebo
model7 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
               + own_sec_mention_avg_notme + own_sec_highlow_notme
               + dem_weighted_mention_avg + cons_weighted_mention_avg + placebo_weighted_mention_avg| Code + Date, 
               data = weighted_mention_avg_df)
summary(model7) 
# Withoout the abs_intra_day and VI
model8 <- felm(highlow ~ mention 
               + own_sec_mention_avg_notme + own_sec_highlow_notme
               + dem_weighted_mention_avg + cons_weighted_mention_avg
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = weighted_mention_avg_df)
summary(model8) 

ggplot() + geom_density(aes(x=weighted_mention_avg_df$placebo_weighted_mention_avg ))

stargazer(model1, model4, model5, model6, model8, df = FALSE,
          title = "Spillover effect of media coverage with article variable", 
          table.placement = "H")

write.csv(weighted_mention_avg_df, str_c(clean_dir, "/FT/matched/weighted_mention_avg_full.csv"),
          row.names = F)

weighted_mentions_short <- as_tibble(weighted_mention_avg_df) %>%
  select(Code, Date, Sector, N_sector, own_sec_mention, own_sec_mention_avg,
         own_sec_mention_notme, own_sec_mention_avg_notme,
         own_sec_highlow, own_sec_highlow_notme,cons_weighted_mention_avg,
         dem_weighted_mention_avg, placebo_weighted_mention_avg)

write.csv(weighted_mentions_short, str_c(clean_dir, "/FT/matched/weighted_mention_avg.csv"),
          row.names = F)


model1 <- felm(highlow ~ mention + placebo_weighted_mention_avg + plm::lag(highlow, 1:10) | 
                 Code + Date, data = weighted_mention_avg_df)
summary(model1) 
model2 <- felm(highlow ~ mention + placebo_weighted_mention_avg + own_sec_mention_avg_notme + 
                 own_sec_highlow_notme + abs_intra_day + VI_put  + VI_call + 
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_mention_avg_df)
summary(model2) 


stargazer(model1, model2, df = FALSE,
          title = "Spillover effects, with placebo matrices", 
          table.placement = "H")





# Baseline
model1 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call +  
                 plm::lag(highlow, 1:10) | Code + Date, data = weighted_mention_df)
summary(model1) 
# Only cons
model2 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 cons_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model2) 
# Only dem
model3 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 dem_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model3) 
# Both
model4 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 dem_weighted_mention + cons_weighted_mention| Code + Date, 
               data = weighted_mention_df)
summary(model4) 
# Both plus own sector
model5 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 + own_sec_mention_notme
               + dem_weighted_mention + cons_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model5) 
# Both, own sec mention and vol
model6 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 + own_sec_mention_notme + own_sec_highlow_notme
               + dem_weighted_mention + cons_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model6) 
# Chuck in a placebo
model7 <- felm(highlow ~ mention + abs_intra_day + VI_put + VI_call + plm::lag(highlow, 1:10) + 
                 + own_sec_mention_notme + own_sec_highlow_notme
               + dem_weighted_mention + cons_weighted_mention + placebo_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model7) 
# Withoout the abs_intra_day and VI
model8 <- felm(highlow ~ mention + plm::lag(highlow, 1:10) 
               + own_sec_mention_notme + own_sec_highlow_notme
               + dem_weighted_mention + cons_weighted_mention | Code + Date, 
               data = weighted_mention_df)
summary(model8) 


stargazer(model1, model4, model5, model6, model8, df = FALSE,
          title = "Spillover effect of media coverage with article variable (max, not average)", 
          table.placement = "H")






daily_mean <- aggregate(weighted_mention_avg_df[,c("mention", "IndexHighLow", "Index_Change", 
                                           "VI_put", "VI_call")], 
                        by = list(weighted_mention_avg_df$Date), FUN = mean, na.action = na.omit)
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
model8 <- lm(IndexHighLow ~ mention + abs(Index_Change) +
             + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             , data = daily_mean)
summary(model8)
model9 <- lm(IndexHighLow ~ mention + abs(Index_Change) + 
               + IndexHighLow_1lag + IndexHighLow_2lag + IndexHighLow_3lag + IndexHighLow_4lag
             + IndexHighLow_5lag + IndexHighLow_6lag + IndexHighLow_7lag + IndexHighLow_8lag
             , data = daily_mean)
summary(model9)



stargazer(model5, model7, model8, df = FALSE,
          title = "The aggregate effects of firm-level media coverage", 
          table.placement = "H")

