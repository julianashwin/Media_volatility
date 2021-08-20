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
import_filename = paste(clean_dir, "FT/matched/all_equityprices_companyarticles.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)

# Add an abspChange for robustness
total_data$pChange <- total_data$pChange*100
total_data$abspChange <- abs(total_data$pChange)

# Add a High-Low measure to look explicitly at volatility
total_data$highlow = 100*(total_data$High - total_data$Low)/total_data$High
total_data$highlow_close = 100*(total_data$High - total_data$Low)/total_data$Close
total_data$highlow_ln = 100*(log(total_data$High) - log(total_data$Low))

total_data$Index_High <- as.numeric(str_replace_all(total_data$Index_High, ",",""))
total_data$Index_Low <- as.numeric(str_replace_all(total_data$Index_Low, ",",""))
total_data$IndexHighLow <- 100*(total_data$Index_High - total_data$Index_Low)/total_data$Index_High




# Format for panel data analysis
total_data <- pdata.frame(total_data, index = c("Code", "Date"))

# Amend some likely measurement errors
clean_data <- total_data
View(clean_data[which(clean_data$Code == "RRS.L"),])
View(total_data[which(total_data$Code == "LSMRx.L^E11"),])
clean_data[which(clean_data$Date == "2002-04-18" & clean_data$Code == "PPB.L"), "Open"] <- 341.9333
clean_data[which(clean_data$Date == "2002-08-27" & clean_data$Code == "THUS.L^K08"), "Open"] <- 90.400
clean_data[which(clean_data$Date == "2000-01-17" & clean_data$Code == "RRS.L"), "Open"] <- 123.86098

# Create intra-day variables
clean_data$intra_day <- 100*(clean_data$Close - clean_data$Open)/clean_data$Open
clean_data$abs_intra_day <- abs(clean_data$intra_day)

View(clean_data[which((clean_data$abspChange==0 & clean_data$abs_intra_day!=0)),])
clean_data[which(clean_data$abspChange==0),"abs_intra_day"] <- 0


clean_data <- clean_data[which(clean_data$Code != "IPRgb.PZ^A12"),]
clean_data <- clean_data[which(clean_data$Code != "LSMRx.L^E11"),]


# Correct a few data errors when time: some negative high-low, as some fields are mixed up 
# sometimes the high is swapped with low or turnover.
clean_data$highlow = abs(clean_data$highlow) # A couple of observations seem to have the fields inverted

View(clean_data[which(clean_data$Code == "LANDn.L^C09"),])
clean_data <- clean_data[which(clean_data$Code != "LANDn.L^C09"),]

clean_data[which(clean_data$Date == "2008-11-14" & clean_data$Code == "FRES.L"), "Open"] <- 96.85
clean_data <- clean_data[which(clean_data$Code != "LANDn.L^C09" & clean_data$Date != "2003-08-27"),]

clean_data[which(clean_data$abspChange==0),"highlow"] <- 0




# Calculate residuals in both absolute and relative change cases
clean_data$open_open <- (clean_data$Open - plm::lag(clean_data$Open,1))/plm::lag(clean_data$Open,1)
clean_data$open_open <- clean_data$open_open*100
clean_data$abs_open_open <- abs(clean_data$open_open)



# Save the clean equity data 
# clean_dir <- "~/Documents/DPhil/Clean_Data"
# clean_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
# write.csv(clean_data, file = clean_filename, row.names = FALSE)
# clean_data <- read.csv(clean_filename, stringsAsFactors = FALSE)




### Get residuals for sLDA algorithm

# Baseline with firm fixed effects
abs_intra_firm <- felm(abs_intra_day~ mention  | Code , data = clean_data)
summary(abs_intra_firm)
highlow_firm <- felm(highlow ~ mention | Code, data = clean_data)
summary(highlow_firm)
intra_firm <- felm(intra_day ~ mention | Code, data = clean_data)
summary(intra_firm)

# Time fixed effects
abs_intra_time <- felm(formula = abs_intra_day ~ mention  | Date, data = clean_data)
summary(abs_intra_time)
highlow_time <- felm(formula = highlow ~ mention | Date, data = clean_data)
summary(highlow_time)
intra_time <- felm(intra_day ~ mention | Date, data = clean_data)
summary(intra_time)

# Firm and time fixed effects
abs_intra_both <- felm(formula = abs_intra_day ~ mention  | Code + Date, data = clean_data)
summary(abs_intra_both)
highlow_both <- felm(formula = highlow ~ mention | Code + Date, data = clean_data)
summary(highlow_both)
intra_both <- felm(intra_day ~ mention | Code + Date, data = clean_data)
summary(intra_both)

# Lags and both fixed effects 
abs_intra_lags <- felm(abs_intra_day ~ mention + plm::lag(abs_open_open,0:10) | Code + Date,
                data = clean_data)
summary(abs_intra_lags)
highlow_lags <- felm(highlow ~ mention + plm::lag(highlow,1:10) | Code + Date,
                     data = clean_data)
summary(highlow_lags)
intra_lags <- felm(intra_day ~ mention + plm::lag(open_open, 0:10)| Code + Date, data = clean_data)
summary(intra_lags)


# Tabulate these
residualising_table <- stargazer(abs_intra_firm, abs_intra_time, abs_intra_both, abs_intra_lags, 
                               highlow_firm, highlow_time, highlow_both, highlow_lags,
                               intra_firm, intra_time, intra_both, intra_lags,
                               table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                               title = "Candidates for sLDA", font.size = "small")




### Omit the mention dummy for the both fixed effect and both + lag models
# Fixed effects
nonews_abs_intra_both <- felm(formula = abs_intra_day ~ 1  | Code + Date, data = clean_data)
summary(nonews_abs_intra_both)
nonews_highlow_both <- felm(formula = highlow ~ 1 | Code + Date, data = clean_data)
summary(nonews_highlow_both)
nonews_intra_both <- felm(intra_day ~ 1 | Code + Date, data = clean_data)
summary(nonews_intra_both)
# Lags and both fixed effects 
nonews_abs_intra_lags <- felm(abs_intra_day ~ plm::lag(abs_open_open,0:10) | Code + Date, data = clean_data)
summary(nonews_abs_intra_lags)
nonews_highlow_lags <- felm(highlow ~  plm::lag(highlow,1:10) | Code + Date, data = clean_data)
summary(nonews_highlow_lags)
nonews_intra_lags <- felm(intra_day ~ plm::lag(open_open, 0:10)| Code + Date, data = clean_data)
summary(nonews_intra_lags)



#abs_intra_day_controls_table <- stargazer(nonews_abs_intra_both), nonews_highlow_both, nonews_intra_both,
#                                          nonews_abs_intra_lags, nonews_highlow_lags, nonews_intra_lags,
#                                          table.placement = "H", df = FALSE,
#                                          title = "Auxilliary regressions for sLDA")


### Store the panel residuals
abs_both_resids <- nonews_abs_intra_both$residuals
abs_lags_resids <- nonews_abs_intra_lags$residuals
highlow_both_resids <- nonews_highlow_both$residuals
highlow_lags_resids <- nonews_highlow_lags$residuals
intra_both_resids <- nonews_intra_both$residuals
intra_lags_resids <- nonews_intra_lags$residuals

# Store the residuals in data frames
# Absolute intra-day returns
abs_both_resid_data <- cbind(as.character(nonews_abs_intra_both$fe$Code), 
                             as.character(nonews_abs_intra_both$fe$Date), as.numeric(abs_both_resids))
abs_both_resid_data <- as.data.frame(abs_both_resid_data, stringsAsFactors = FALSE)
abs_lags_resid_data <- cbind(as.character(nonews_abs_intra_lags$fe$Code), 
                             as.character(nonews_abs_intra_lags$fe$Date), as.numeric(abs_lags_resids))
abs_lags_resid_data <- as.data.frame(abs_lags_resid_data, stringsAsFactors = FALSE)
colnames(abs_both_resid_data) <- c("Code", "Date", "abs_both_residual")
colnames(abs_lags_resid_data) <- c("Code", "Date", "abs_lags_residual")
abs_both_resid_data$abs_both_residual <- as.numeric(abs_both_resid_data$abs_both_residual)
abs_lags_resid_data$abs_lags_residual <- as.numeric(abs_lags_resid_data$abs_lags_residual)

# High-to-low volatility
highlow_both_resid_data <- cbind(as.character(nonews_highlow_both$fe$Code), 
                                 as.character(nonews_highlow_both$fe$Date), as.numeric(highlow_both_resids))
highlow_both_resid_data <- as.data.frame(highlow_both_resid_data, stringsAsFactors = FALSE)
highlow_lags_resid_data <- cbind(as.character(nonews_highlow_lags$fe$Code), 
                                 as.character(nonews_highlow_lags$fe$Date), as.numeric(highlow_lags_resids))
highlow_lags_resid_data <- as.data.frame(highlow_lags_resid_data, stringsAsFactors = FALSE)
colnames(highlow_both_resid_data) <- c("Code", "Date", "highlow_both_residual")
colnames(highlow_lags_resid_data) <- c("Code", "Date", "highlow_lags_residual")
highlow_both_resid_data$highlow_both_residual <- as.numeric(highlow_both_resid_data$highlow_both_residual)
highlow_lags_resid_data$highlow_lags_residual <- as.numeric(highlow_lags_resid_data$highlow_lags_residual)

# Intra-day returns
intra_both_resid_data <- cbind(as.character(nonews_intra_both$fe$Code), 
                               as.character(nonews_intra_both$fe$Date), as.numeric(intra_both_resids))
intra_both_resid_data <- as.data.frame(intra_both_resid_data, stringsAsFactors = FALSE)
intra_lags_resid_data <- cbind(as.character(nonews_intra_lags$fe$Code), 
                               as.character(nonews_intra_lags$fe$Date), as.numeric(intra_lags_resids))
intra_lags_resid_data <- as.data.frame(intra_lags_resid_data, stringsAsFactors = FALSE)
colnames(intra_both_resid_data) <- c("Code", "Date", "intra_both_residual")
colnames(intra_lags_resid_data) <- c("Code", "Date", "intra_lags_residual")
intra_both_resid_data$intra_both_residual <- as.numeric(intra_both_resid_data$intra_both_residual)
intra_lags_resid_data$intra_lags_residual <- as.numeric(intra_lags_resid_data$intra_lags_residual)


# Merge the residuals with the data (first need to convert the codes and dates to characters)
clean_data$Date <- as.character(clean_data$Date); clean_data$Code <- as.character(clean_data$Code);
merge_data <- merge(clean_data, abs_lags_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, abs_both_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, highlow_both_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, highlow_lags_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, intra_both_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, intra_lags_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- pdata.frame(merge_data, index = c("Code", "Date"))


test_felm <- felm(abs_both_residual ~ mention | Code + Date, data = merge_data)
summary(test_felm)
test_felm <- felm(highlow_both_residual ~ mention| Code + Date, data = merge_data)
summary(test_felm)
test_felm <- felm(highlow_lags_residual ~ mention + plm::lag(highlow,1:8)| Code + Date, data = merge_data)
summary(test_felm)
test_felm <- felm(highlow_both_residual ~ mention + plm::lag(highlow,1:10) + 
                    plm::lag(IndexHighLow, 1:10)| Code + Date, data = merge_data)
summary(test_felm)


#write.csv(merge_data, "headline_matched_residuals.csv", row.names = FALSE)
mentions_only <- merge_data[which(merge_data$mention == 1),]



### Create a variable for those observations within 3 sd of the mean 
abs_lags_range <- c(mean(mentions_only$abs_lags_residual, na.rm = TRUE) - 3*sd(mentions_only$abs_lags_residual, na.rm = TRUE), 
           mean(mentions_only$abs_lags_residual, na.rm = TRUE) + 3*sd(mentions_only$abs_lags_residual, na.rm = TRUE))
mentions_only$abs_lags_include <- 0
mentions_only$abs_lags_include[which(mentions_only$abs_lags_residual >= abs_lags_range[1] &
                                       mentions_only$abs_lags_residual <= abs_lags_range[2])] <- 1

abs_both_range <- c(mean(mentions_only$abs_both_residual, na.rm = TRUE) - 3*sd(mentions_only$abs_both_residual, na.rm = TRUE), 
                    mean(mentions_only$abs_both_residual, na.rm = TRUE) + 3*sd(mentions_only$abs_both_residual, na.rm = TRUE))
mentions_only$abs_both_include <- 0
mentions_only$abs_both_include[which(mentions_only$abs_both_residual >= abs_both_range[1] &
                                       mentions_only$abs_both_residual <= abs_both_range[2])] <- 1

highlow_lags_range <- c(mean(mentions_only$highlow_lags_residual, na.rm = TRUE) - 3*sd(mentions_only$highlow_lags_residual, na.rm = TRUE), 
                    mean(mentions_only$highlow_lags_residual, na.rm = TRUE) + 3*sd(mentions_only$highlow_lags_residual, na.rm = TRUE))
mentions_only$highlow_lags_include <- 0
mentions_only$highlow_lags_include[which(mentions_only$highlow_lags_residual >= highlow_lags_range[1] &
                                       mentions_only$highlow_lags_residual <= highlow_lags_range[2])] <- 1

highlow_both_range <- c(mean(mentions_only$highlow_both_residual, na.rm = TRUE) - 3*sd(mentions_only$highlow_both_residual, na.rm = TRUE), 
                    mean(mentions_only$highlow_both_residual, na.rm = TRUE) + 3*sd(mentions_only$highlow_both_residual, na.rm = TRUE))
mentions_only$highlow_both_include <- 0
mentions_only$highlow_both_include[which(mentions_only$highlow_both_residual >= highlow_both_range[1] &
                                       mentions_only$highlow_both_residual <= highlow_both_range[2])] <- 1


clean_dir <- "~/Documents/DPhil/Clean_Data"
export_filename = paste(clean_dir, "FT/matched/companyarticles_matched_residuals.csv", sep = "/")
write.csv(mentions_only, file = export_filename, row.names = FALSE)
# mentions_only <- read.csv(export_filename, stringsAsFactors = FALSE)


# Save the clean equity data 
clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
write.csv(merge_data, file = clean_filename, row.names = FALSE)
# clean_data <- read.csv(clean_filename, stringsAsFactors = FALSE)





# Plot the residuals to check if they're plausibly normal
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Plot the daily percentage change in price
ggplot(mentions_only[which(mentions_only$abs_both_include == 1),], aes(abs_both_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Intra-day Percentage Change Residual")
export_filename = paste(figure_location, "all_res_density.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)

ggplot(mentions_only, aes(highlow_lags_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Vol Residual")
export_filename = paste(figure_location, "headonly_res_density.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)

ggplot(mentions_only[which(mentions_only$highlow_lags_include == 1),], aes(highlow_lags_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Vol Residual")
export_filename = paste(figure_location, "headonly_res_density_trunc.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)

ggplot(headlines_only, aes(abs_intra_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Intra-day Absoute Percentage Change Residual")
export_filename = paste(figure_location, "headonly_abs_res_density.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)






########################################

# Investigate a good way of transforming these daily returns
x <- seq(-10,10, 0.01)
logx <- log(1+abs(x))*sign(x)
png("log_abs_x_graph.png", width = 480, height = 240)
plot(x,logx, type = "l", ylab = "transformed x")
dev.off()

clean_data$random <- rnorm(length(clean_data$Date), 0, 1)
clean_data$transform_random <- log(1+abs(clean_data$random))*sign(clean_data$random)
ggplot(clean_data, aes(random)) + geom_density()
ggsave("output/normal_density.png", width = 8, height = 4, dpi = 200)
ggplot(clean_data, aes(transform_random)) + geom_density()
ggsave("output/transformed_normal_density.png", width = 8, height = 4, dpi = 200)

no_out <- clean_data[which(clean_data$abs_Change_1day<5),]

clean_data$log_abs_Change_1day <- log(1+clean_data$abs_Change_1day)
clean_data$log_Change_1day <- log(1+abs(clean_data$Change_1day))*sign(clean_data$Change_1day)
clean_data$double_Change <- clean_data$Change_1day*2
ggplot(clean_data, aes(log_abs_Change_1day)) + geom_density()
ggplot(clean_data, aes(log_Change_1day)) + geom_density()
ggsave("output/transformed_change_density.png", width = 8, height = 4, dpi = 200)

ggplot(no_out, aes(Change_1day)) + geom_density()
ggplot(no_out, aes(double_Change)) + geom_density()