setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)

# Import the panel data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)



### Import the topic distribution data for absolute change first
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_abs_slda_30proportions.csv", sep = "/")
abs_topic_props <- read.csv(import_filename, stringsAsFactors = FALSE)
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_rel_slda_30proportions.csv", sep = "/")
rel_topic_props <- read.csv(import_filename, stringsAsFactors = FALSE)



abs_all_data <- merge(total_data, abs_topic_props, by = c("Date", "Code"), all.x = TRUE)
rel_all_data <- merge(total_data, rel_topic_props, by = c("Date", "Code"), all.x = TRUE)
abs_all_data <- pdata.frame(abs_all_data, index = c("Code", "Date"))
rel_all_data <- pdata.frame(rel_all_data, index = c("Code", "Date"))


abs_all_data[which(is.na(abs_all_data$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                     "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                     "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                     "T29")] <- 0
rel_all_data[which(is.na(rel_all_data$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                             "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                             "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                             "T29")] <- 0


# Import the topic description data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_abs_slda_30topic_description.csv", sep = "/")
abs_topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)
import_filename = paste(clean_dir, "FT/matched/company_rel_slda_30topic_description.csv", sep = "/")
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
colnames(abs_all_data[,37:(37+topicnumber -1)]) 
abs_topiclabels <- paste(colnames(abs_all_data[,37:(37+topicnumber -1)]), 
                         abs_topwords, sep = "") 


rel_topwords <- rep(":", topicnumber)
for (i in seq(1,topicnumber)){
  for(j in seq(2,6)){
    h <- 2*i
    word <- toString(rel_topics[h-1,j])
    rel_topwords[i] <-  paste(rel_topwords[i], word, sep = " ")
  }
}
colnames(rel_all_data[,37:(37+topicnumber -1)]) 
rel_topiclabels <- paste(colnames(rel_all_data[,37:(37+topicnumber -1)]), 
                     rel_topwords, sep = "") 



colnames(abs_all_data)
colnames(rel_all_data)

abs_fixed_baseline <- plm(abs_intra_day ~  mention , data = abs_all_data,
                     index = c("Code", "Date"), model = "within")
summary(abs_fixed_baseline)
rel_fixed_controls <- plm(intra_day ~  mention + lag(open_open,0:4) 
                     + lag(Index_Change, 0:4)
                     + Monday + Tuesday + Wednesday + Thursday, data = rel_all_data,
                     index = c("Code", "Date"), model = "within")
summary(rel_fixed_controls)



### Regress on the topics
abs_w_topics = plm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 , data = abs_all_data,
                index = c("Code", "Date"), model = "within")
summary(abs_w_topics)
abs_w_topics_controls = plm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                            + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                            +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                            +T27 + T28 + T29- 0 + lag(abs_open_open,0:4) 
                         + lag(Index_abs_Change, 0:4)
                         + Monday + Tuesday + Wednesday + Thursday, data = abs_all_data,
                            index = c("Code", "Date"), model = "within")
summary(abs_w_topics_controls)
rel_w_topics = plm(intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 , data = rel_all_data,
                index = c("Code", "Date"), model = "within")
summary(rel_w_topics)
rel_w_topics_controls = plm(intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                         + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                         +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                         +T27 + T28 + T29- 0 + lag(open_open,0:4) 
                         + lag(Index_Change, 0:4)
                         + Monday + Tuesday + Wednesday + Thursday, data = rel_all_data,
                         index = c("Code", "Date"), model = "within")
summary(rel_w_topics_controls)


abs_topics_table <- stargazer(abs_w_topics, abs_w_topics_controls,
                              table.placement = "H", df = FALSE, column.sep.width = "2pt",
                              covariate.labels = abs_topiclabels, font.size = "tiny",
                              title = "Absolute sLDA first stage regression")
rel_topics_table <- stargazer(rel_w_topics, rel_w_topics_controls,
                              table.placement = "H", df = FALSE, column.sep.width = "2pt",
                              covariate.labels = rel_topiclabels, font.size = "tiny",
                              title = "Relative sLDA first stage regression")




# Construct the the news shock using the estimated coefficients
abs_topicnames <- colnames(abs_all_data[,37:(37+topicnumber -1)])
rel_topicnames <- colnames(rel_all_data[,37:(37+topicnumber -1)])

abs_coefs <- as.numeric(abs_w_topics$coefficients[abs_topicnames])
abs_coefs_controls <- as.numeric(abs_w_topics_controls$coefficients[abs_topicnames])
rel_coefs <- as.numeric(rel_w_topics$coefficients[rel_topicnames])
rel_coefs_controls <- as.numeric(rel_w_topics_controls$coefficients[rel_topicnames])


# Store the sLDA predicted article effect
abs_all_data$abs_shock <- as.vector(as.matrix(abs_all_data[,abs_topicnames])%*%abs_coefs)
abs_all_data$abs_control_shock <- as.vector(as.matrix(abs_all_data[,abs_topicnames])%*%abs_coefs_controls)
rel_all_data$rel_shock <- as.vector(as.matrix(rel_all_data[,rel_topicnames])%*%rel_coefs)
rel_all_data$rel_control_shock <- as.vector(as.matrix(rel_all_data[,rel_topicnames])%*%rel_coefs_controls)



# Check that this has worked properly - all of the coefficients on the shocks *should* be 1
test <- plm(abs_intra_day ~ abs_shock , data = abs_all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(abs_intra_day ~ abs_control_shock + lag(abs_open_open,0:4) 
            + lag(Index_abs_Change, 0:4)
            + Monday + Tuesday + Wednesday + Thursday, data = abs_all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(intra_day ~ rel_shock , data = rel_all_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(intra_day ~ rel_control_shock + lag(open_open,0:4) 
                    + lag(Index_Change, 0:4)
                    + Monday + Tuesday + Wednesday + Thursday, data = rel_all_data,
                    index = c("Code", "Date"), model = "within")
summary(test)


# Add the shocks back into the total data frame to export for the cross-firm analysis
total_data$abs_shock <- abs_all_data$abs_shock
total_data$abs_control_shock <- abs_all_data$abs_control_shock
total_data$rel_shock <- rel_all_data$rel_shock
total_data$rel_control_shock <- rel_all_data$rel_control_shock


test <- plm(abs_intra_day ~ abs_shock , data = total_data,
            index = c("Code", "Date"), model = "within")
summary(test)
test <- plm(intra_day ~ rel_control_shock, data = total_data,
            index = c("Code", "Date"), model = "within")
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
                   "_rel <- rel_all_data$T", i)
  eval(parse(text=command))
  command <- paste0("all_data$T", i, 
                    "_abs <- abs_all_data$T", i)
  eval(parse(text=command))
}
table(all_data$T7_abs == abs_all_data$T7)
table(all_data$T7_rel == rel_all_data$T7)


clean_dir <- "~/Documents/DPhil/Clean_Data"
export_filename = paste(clean_dir, "FT/matched/clean_equities_articles_slda_absreltopics_shocks.csv", sep = "/")
write.csv(all_data, file = export_filename, row.names = FALSE)
# all_data <- read.csv(export_filename, stringsAsFactors = FALSE)





###########################