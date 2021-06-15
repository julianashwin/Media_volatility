setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list = ls())
require(plm)
require(ggplot2)
require(stringr)
require(stargazer)
require(lfe)
require(reshape2)
require(Matrix)



clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_absshock_data_NACE.csv", sep = "/")
temp_data_abs <- read.csv(clean_filename, stringsAsFactors = FALSE)
temp_data_abs <- pdata.frame(temp_data_abs, index = c("Code", "Date"))

clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/each_sector_highlowshock_data_NACE.csv", sep = "/")
temp_data_highlow <- read.csv(clean_filename, stringsAsFactors = FALSE)
temp_data_highlow <- pdata.frame(temp_data_highlow, index = c("Code", "Date"))

test <- felm(highlow ~  highlow_shock + Sector06_and_07_highlow_c_shock| Code + Date, data = temp_data_highlow)
summary(test)
test <- felm(highlow ~  Sector06_and_07_abs_c_shock | Code + Date, data = temp_data_abs)
summary(test)





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


import_filename <- paste0(clean_dir, "/UK_macro/input-output/average_supplier_matrix_intconsweighted.csv")
cons_matrix <- read.csv(import_filename)
colnames(cons_matrix) <- matrix_labels$x
rownames(cons_matrix) <- matrix_labels$x
import_filename <- paste0(clean_dir, "/UK_macro/input-output/average_consumer_matrix_intdemweighted.csv")
dem_matrix <- read.csv(import_filename)
dem_matrix[is.na(dem_matrix)] <- 0
colnames(dem_matrix) <- matrix_labels$x
rownames(dem_matrix) <- matrix_labels$x





# Some summary statistics for the input-output matrix
test_mat <- as.matrix(dem_matrix)
sum(diag(test_mat))
test_mat <- as.matrix(cons_matrix)
sum(diag(test_mat))


# Generate a random I-O matrix
temp_mat <- as.matrix(cons_matrix)
temp_mat <- as.vector(temp_mat)
temp_mat <- sample(temp_mat)
temp_mat <- data.frame(matrix(temp_mat, nrow = 105, ncol = 105))

rand_mat <- data.frame(scale(temp_mat, center = FALSE, 
               scale = colSums(temp_mat)))
colnames(rand_mat) <- matrix_labels$x
rownames(rand_mat) <- matrix_labels$x



for (shock in c("abs", "highlow")){
  
  if (shock == "abs"){
    temp_data <- temp_data_abs
  } else if (shock == "highlow"){
    temp_data <- temp_data_highlow
  }
  
  
  # Create a variable for the intermediate consumption weighted shock
  temp_data$cons_weighted_shock <- 0
  temp_data$dem_weighted_shock <- 0
  weighted_shocks <- temp_data[0,]
  
  # First multiply by *columns* of the I-O matrix, to get the effect of shocks to suppliers
  start_col <- which(colnames(temp_data) == paste0("Sector01_", shock, "_c_shock"))
  end_col <- which(colnames(temp_data) == paste0("Sector97_", shock, "_c_shock"))
  cols <- seq(start_col, end_col)
  
  for (i in 1:nrow(matrix_labels)){
    sector <- matrix_labels$x[i]
    var <- colnames(temp_data)[cols[i]]
    
    print(paste("Column", colnames(rand_mat)[i], "and Row", rownames(rand_mat)[i]))
    print(paste("Sector", sector, "in variable", var))
    
    sector_data <- temp_data[which(temp_data$Sector == sector),]
    
    if (nrow(sector_data)> 0 ){
      
      # First the intermediate consumption 
      matrix_coeffs <- rand_mat[,sector]
      shocks <- sector_data[,start_col:end_col]
      
      shocks <- sapply(shocks, function(x) {
        if(is.factor(x)) as.numeric(as.character(x)) else x
      })
      sapply(shocks, class)
      
      cons_shock <- shocks %*% matrix_coeffs
      sector_data$cons_weighted_shock <- cons_shock
      
      # Now the intermediate demand 
      matrix_coeffs <- t(rand_mat[sector,])
      
      dem_shock <- shocks %*% matrix_coeffs
      sector_data$dem_weighted_shock <- dem_shock
      
      weighted_shocks <- rbind(weighted_shocks, sector_data)
    }
    
  }
  
  if (shock == "abs"){
    weighted_shocks_abs <- weighted_shocks
  } else if (shock == "highlow"){
    weighted_shocks_highlow <- weighted_shocks
  }
  
}

weighted_shocks_abs$dem_weighted_shock <- as.numeric(weighted_shocks_abs$dem_weighted_shock)
weighted_shocks_highlow$dem_weighted_shock <- as.numeric(weighted_shocks_highlow$dem_weighted_shock)






# Run some test regressions
test <- felm(weighted_shocks_abs$abs_intra_day ~ weighted_shocks_abs$abs_control_shock +
               weighted_shocks_abs$dem_weighted_shock | weighted_shocks_abs$Code)
summary(test)

test <- felm(abs_intra_day ~ dem_weighted_shock + abs_control_shock| Code, data = weighted_shocks_abs)
summary(test)

test <- felm(abs_intra_day ~ abs_control_shock + cons_weighted_shock| Code , 
             data = weighted_shocks_highlow)
summary(test) 
test <- felm(abs_intra_day ~ abs_control_shock + cons_weighted_shock + plm::lag(abs_open_open, 0:5) | Code + Date, 
             data = weighted_shocks_abs)
summary(test) 
test <- felm(highlow ~ highlow_control_shock + cons_weighted_shock + plm::lag(highlow, 1:5) | Code + Date, 
             data = weighted_shocks_abs)
summary(test) 
test <- felm(highlow ~ highlow_control_shock + cons_weighted_shock + plm::lag(highlow, 1:5) | Code + Date, 
             data = weighted_shocks_highlow)
summary(test) 
test <- felm(highlow ~ highlow_control_shock + dem_weighted_shock + plm::lag(highlow, 1:5) | Code + Date, 
             data = weighted_shocks_highlow)
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
test <- felm(highlow ~ highlow_control_shock + cons_weighted_shock + plm::lag(highlow, 1:5) | Code + Date, 
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
clean_filename = paste(clean_dir, "FT/matched/cross_firm_news_data_I-O_placebo.csv", sep = "/")
write.csv(weighted_shocks, file = clean_filename, row.names = FALSE)
# weighted_shocks <- read.csv(clean_filename, stringsAsFactors = FALSE)
# weighted_shocks <- pdata.frame(weighted_shocks, index = c("Code", "Date"))














############################ abs intra day ############################ 


### Create a results table for weighted intermediate demand and consumption shocks on abs_intra_day
cons1 <- felm(abs_intra_day ~ abs_control_shock + cons_weighted_shock | Code + Date, data = weighted_shocks)
summary(cons1)
cons2 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_notme + 
                cons_weighted_shock | Code + Date, data = weighted_shocks)
summary(cons2)
cons3 <- felm(abs_intra_day ~ abs_control_shock + own_sec_news_abs_notme + 
                cons_weighted_shock + plm::lag(abs_open_open, 0:5)| Code + Date, data = weighted_shocks)
summary(cons3)


int_cons_table <- stargazer(cons1, cons2, cons3, column.sep.width = "1pt",
                            table.placement = "H", df = FALSE, title = "Abs Random matrix effects")





############################ highlow ############################ 


### Create a results table for weighted intermediate demand and consumption shocks on abs_intra_day
cons1 <- felm(highlow ~ highlow_control_shock + cons_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(cons1)
cons2 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_notme + 
                cons_weighted_shock_hl | Code + Date, data = weighted_shocks)
summary(cons2)
cons3 <- felm(highlow ~ highlow_control_shock + own_sec_news_highlow_notme + 
                cons_weighted_shock_hl + plm::lag(highlow, 1:5)| Code + Date, data = weighted_shocks)
summary(cons3)
cons4 <- felm(highlow ~ abs_intra_day + highlow_control_shock + own_sec_news_highlow_notme + 
                cons_weighted_shock_hl + plm::lag(highlow, 1:5)| Code + Date, data = weighted_shocks)
summary(cons4)


int_cons_table <- stargazer(cons1, cons2, cons3, cons4, column.sep.width = "1pt",
                            table.placement = "H", df = FALSE, title = "High-low Intermediate consumption effects")











# Generate random I-O matrices as a sense check
temp_mat <- as.matrix(cons_matrix)
diagonals <- diag(temp_mat)
n_zeroes <- sum(temp_m)






