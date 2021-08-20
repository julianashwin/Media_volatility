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
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)



### Import the topic distribution data for absolute change first
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_abs_slda_30proportions_full.csv", sep = "/")
abs_topic_props <- read.csv(import_filename, stringsAsFactors = FALSE)
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_highlow_slda_30proportions_full.csv", sep = "/")
highlow_topic_props <- read.csv(import_filename, stringsAsFactors = FALSE)



abs_all_data <- merge(total_data, abs_topic_props, by = c("Date", "Code"), all.x = TRUE)
highlow_all_data <- merge(total_data, highlow_topic_props, by = c("Date", "Code"), all.x = TRUE)
#abs_all_data <- pdata.frame(abs_all_data, index = c("Code", "Date"))
#highlow_all_data <- pdata.frame(highlow_all_data, index = c("Code", "Date"))


abs_all_data[which(is.na(abs_all_data$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                     "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                     "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                     "T29")] <- 0
highlow_all_data[which(is.na(highlow_all_data$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                             "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                             "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                             "T29")] <- 0


# Import the topic description data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_abs_slda_30topic_description.csv", sep = "/")
abs_topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)
import_filename = paste(clean_dir, "FT/matched/company_highlow_slda_30topic_description.csv", sep = "/")
rel_topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)


topicnumber <- (length(rel_topics[,1])/2)
wordnum <- 20


### Top words for abs data, to use as column labels
abs_topwords <- rep(":", topicnumber)
for (i in seq(1,topicnumber)){
  for(j in seq(2,6)){
    h <- 2*i
    word <- toString(abs_topics[h-1,j])
    abs_topwords[i] <-  paste(abs_topwords[i], word, sep = " ")
  }
}
topicstart <- which(colnames(abs_all_data)== "T0")
colnames(abs_all_data[,topicstart:(topicstart+topicnumber -1)]) 
abs_topiclabels <- paste(colnames(abs_all_data[topicstart:(topicstart+topicnumber -1)]), 
                         abs_topwords, sep = "") 


highlow_topwords <- rep(":", topicnumber)
for (i in seq(1,topicnumber)){
  for(j in seq(2,6)){
    h <- 2*i
    word <- toString(rel_topics[h-1,j])
    highlow_topwords[i] <-  paste(highlow_topwords[i], word, sep = " ")
  }
}
topicstart <- which(colnames(highlow_all_data)== "T0")
colnames(highlow_all_data[,topicstart:(topicstart+topicnumber -1)]) 
highlow_topiclabels <- paste(colnames(highlow_all_data[,topicstart:(topicstart+topicnumber -1)]), 
                     highlow_topwords, sep = "") 



colnames(abs_all_data)
colnames(highlow_all_data)
abs_all_data <- pdata.frame(abs_all_data, index = c("Code", "Date"))
highlow_all_data <- pdata.frame(highlow_all_data, index = c("Code", "Date"))

abs_fixed_baseline <- felm(abs_intra_day ~  mention | Code , data = abs_all_data)
summary(abs_fixed_baseline)
highlow_fixed_controls <- felm(highlow ~  mention + plm::lag(highlow,1:4) 
                     + plm::lag(IndexHighLow, 0:4)| Code + Date, data = highlow_all_data)
summary(highlow_fixed_controls)
highlow_lags <- felm(highlow ~ mention + plm::lag(highlow,1:10) | Code + Date,
                     data = highlow_all_data)
summary(highlow_lags)

highlow_fixed_controls <- felm(highlow ~  mention + plm::lag(highlow,1:10)| Code + Date, data = highlow_all_data)
summary(highlow_fixed_controls)



### Regress on the topics
abs_w_topics = felm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 | Code, data = abs_all_data)
summary(abs_w_topics)
abs_w_topics_controls = felm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                            + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                            +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                            +T27 + T28 + T29- 0 + plm::lag(abs_open_open,0:10) | Code + Date, data = abs_all_data)
summary(abs_w_topics_controls)

# And same for highlow
highlow_w_topics = felm(highlow ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                   + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                   +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                   +T27 + T28 + T29 - 0 | Code , data = highlow_all_data)
summary(highlow_w_topics)
highlow_w_topics_controls = felm(highlow ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                            + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                            +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                            +T27 + T28 + T29- 0 + plm::lag(highlow,1:10)| Code + Date, data = highlow_all_data)
summary(highlow_w_topics_controls)


abs_topics_table <- stargazer(abs_w_topics, abs_w_topics_controls,
                              table.placement = "H", df = FALSE, column.sep.width = "2pt",
                              covariate.labels = abs_topiclabels, font.size = "tiny",
                              title = "Absolute sLDA first stage regression (with some out-of-sample)")
highlow_topics_table <- stargazer(highlow_w_topics, highlow_w_topics_controls,
                              table.placement = "H", df = FALSE, column.sep.width = "2pt",
                              covariate.labels = highlow_topiclabels, font.size = "tiny",
                              title = "High-low sLDA first stage regression (with some out-of-sample)")




# Construct the the news shock using the estimated coefficients
abs_coefs <- as.numeric(abs_w_topics$coefficients[1:topicnumber])
abs_coefs_controls <- as.numeric(abs_w_topics_controls$coefficients[1:topicnumber])
highlow_coefs <- as.numeric(highlow_w_topics$coefficients[1:topicnumber])
highlow_coefs_controls <- as.numeric(highlow_w_topics_controls$coefficients[1:topicnumber])


# Get the phi parameters for each iteration
source_location <- "~/Documents/DPhil/Clean_Data/FT/matched"
phi_filename <- paste0(source_location, "/company_abs_slda_30phi.csv")
abs_phi <- read.csv(phi_filename)
cutoff <- which(abs_phi$X0 == 0)[1]-1
abs_phi <- abs_phi[cutoff,2:(topicnumber+1)]
abs_phi <- as.numeric(abs_phi)

source_location <- "~/Documents/DPhil/Clean_Data/FT/matched"
phi_filename <- paste0(source_location, "/company_highlow_slda_30phi.csv")
highlow_phi <- read.csv(phi_filename)
cutoff <- which(highlow_phi$X0 == 0)[1]-1
highlow_phi <- highlow_phi[cutoff,2:(topicnumber+1)]
highlow_phi <- as.numeric(highlow_phi)

start_col <- which(colnames(abs_all_data) == "T0")
end_col <- which(colnames(abs_all_data) == "T29")
cols <- seq(start_col, end_col)
topicnames <- colnames(abs_all_data)[cols]

# Store the sLDA predicted article effect
abs_all_data$abs_shock <- as.vector(as.matrix(abs_all_data[,topicnames])%*%abs_coefs)
abs_all_data$abs_control_shock <- as.vector(as.matrix(abs_all_data[,topicnames])%*%abs_coefs_controls)
abs_all_data$abs_control_shock_phi <- as.vector(as.matrix(abs_all_data[,topicnames])%*%abs_phi)
abs_all_data$highlow_shock <- as.vector(as.matrix(highlow_all_data[,topicnames])%*%highlow_coefs)
abs_all_data$highlow_control_shock <- as.vector(as.matrix(highlow_all_data[,topicnames])%*%highlow_coefs_controls)
abs_all_data$highlow_control_shock_phi <- as.vector(as.matrix(highlow_all_data[,topicnames])%*%highlow_phi)


# Check that this has worked properly - all of the coefficients on the shocks *should* be 1
test <- plm(abs_intra_day ~ abs_control_shock , data = abs_all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(abs_intra_day ~ abs_control_shock_phi , data = abs_all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- felm(abs_intra_day ~ abs_control_shock_phi + plm::lag(abs_open_open,0:10)|Code + Date, data = abs_all_data)
summary(test)
test <- felm(abs_intra_day ~ abs_control_shock + plm::lag(abs_open_open,0:10)|Code + Date, data = abs_all_data)
summary(test)
test <- felm(highlow ~ highlow_shock| Code , data = abs_all_data)
summary(test)
test <- felm(highlow ~ highlow_control_shock + plm::lag(highlow,1:10)| Code + Date , data = abs_all_data)
summary(test)
test <- felm(highlow ~ highlow_control_shock_phi + plm::lag(highlow,1:10)| Code + Date , data = abs_all_data)
summary(test)


# Add the shocks back into the total data frame to export for the cross-firm analysis
total_data$abs_shock <- abs_all_data$abs_shock
total_data$abs_control_shock <- abs_all_data$abs_control_shock
total_data$abs_control_shock_phi <- abs_all_data$abs_control_shock_phi
total_data$highlow_shock <- abs_all_data$highlow_shock
total_data$highlow_control_shock <- abs_all_data$highlow_control_shock
total_data$highlow_control_shock_phi <- abs_all_data$highlow_control_shock_phi

test <- felm(abs_intra_day ~ abs_shock| Code , data = total_data)
summary(test)
test <- felm(highlow ~ highlow_control_shock | Code, data = total_data)
summary(test)
test <- felm(highlow ~ highlow_control_shock_phi | Code, data = total_data)
summary(test)



# Write the sLDA shocks to a file, without the topics
clean_dir <- "~/Documents/DPhil/Clean_Data"
export_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_shocks.csv", sep = "/")
write.csv(total_data, file = export_filename, row.names = FALSE)
# total_data <- read.csv(export_filename, stringsAsFactors = FALSE)



# Create a larger data frame inculding the shocks and all of the topics
all_data <- total_data
for (i in 0:(topicnumber-1)){
  print(i)
  command <- paste0("all_data$T", i, 
                   "_highlow <- highlow_all_data$T", i)
  eval(parse(text=command))
  command <- paste0("all_data$T", i, 
                    "_abs <- abs_all_data$T", i)
  eval(parse(text=command))
}
table(all_data$T7_abs == abs_all_data$T7)
table(all_data$T7_highlow == highlow_all_data$T7)


clean_dir <- "~/Documents/DPhil/Clean_Data"
export_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_topics_shocks.csv", sep = "/")
write.csv(all_data, file = export_filename, row.names = FALSE)
# all_data <- read.csv(export_filename, stringsAsFactors = FALSE)





###########################