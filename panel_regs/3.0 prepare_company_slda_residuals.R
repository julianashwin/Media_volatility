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
import_filename = paste(clean_dir, "FT/matched/all_equityprices_companyarticles1.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)

# Add an abspChange for robustness
total_data$pChange <- total_data$pChange*100
total_data$abspChange <- abs(total_data$pChange)

# Format for panel data analysis
total_data <- pdata.frame(total_data, index = c("Code", "Date"))

# Amend some likely measurement errors
clean_data <- total_data
View(clean_data[which(clean_data$Code == "RRS.L"),])
View(total_data[which(total_data$Code == "LSMRx.L^E11"),])
clean_data[which(clean_data$Date == "2002-04-18" & clean_data$Code == "PPB.L"), "Open"] <- 341.9333
clean_data[which(clean_data$Date == "2002-08-27" & clean_data$Code == "THUS.L^K08"), "Open"] <- 90.400
clean_data[which(clean_data$Date == "2000-01-17" & clean_data$Code == "RRS.L"), "Open"] <- 123.86098

# Creat intra-day variables
clean_data$intra_day <- 100*(clean_data$Close - clean_data$Open)/clean_data$Open
clean_data$abs_intra_day <- abs(clean_data$intra_day)
clean_data[which(clean_data$abspChange==0),"abs_intra_day"] <- 0


clean_data <- clean_data[which(clean_data$Code != "IPRgb.PZ^A12"),]
clean_data <- clean_data[which(clean_data$Code != "LSMRx.L^E11"),]



# Calculate residuals in both absolute and relative change cases
clean_data$open_open <- (clean_data$Open - plm::lag(clean_data$Open,1))/plm::lag(clean_data$Open,1)
clean_data$open_open <- clean_data$open_open*100
clean_data$abs_open_open <- abs(clean_data$open_open)


clean_dir <- "~/Documents/DPhil/Clean_Data"
clean_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
write.csv(clean_data, file = clean_filename, row.names = FALSE)
# clean_data <- read.csv(clean_filename, stringsAsFactors = FALSE)



baseline <- plm(abs_intra_day~ mention, data = clean_data, index = c("Code", "Date"), model = "within")
summary(baseline)



abs_intra_plm <- plm(abs_intra_day ~ plm::lag(mention, 0) + plm::lag(abs_open_open,0:4) 
                + plm::lag(Index_abs_Change, 0:4)
                + Monday + Tuesday + Wednesday + Thursday
                , data = clean_data, index = c("Code", "Date"), model = "within")
# [which(clean_data$abs_intra_day<100),]
summary(abs_intra_plm)
intra_plm <- plm(intra_day ~ plm::lag(mention,0) + lag(open_open, 0:4) 
                + lag(Index_Change, 0:4)
                + Monday + Tuesday + Wednesday + Thursday
                , data = clean_data, index = c("Code", "Date"), model = "within")
summary(intra_plm)

# Also consider the close-to-close percentage change
abs_pchange_plm <- plm(abspChange ~ lag(mention, 0) + lag(abspChange, 1:4) 
                    + lag(Index_abs_Change, 0:4)
                    + Monday + Tuesday + Wednesday + Thursday
                    , data = clean_data, index = c("Code", "Date"), model = "within")
summary(abs_pchange_plm)
pchange_plm <- plm(pChange ~ plm::lag(mention,0) + lag(pChange, 1:4) 
                + lag(Index_Change, 0:4)
                + Monday + Tuesday + Wednesday + Thursday
                , data = clean_data, index = c("Code", "Date"), model = "within")
summary(pchange_plm)




# Omit the mention dummy 
nonews_abs_intra_plm <- plm(abs_intra_day ~ plm::lag(abs_open_open,0:4) 
                     + plm::lag(Index_abs_Change, 0:4)
                     + Monday + Tuesday + Wednesday + Thursday
                     , data = clean_data, index = c("Code", "Date"), model = "within")
summary(nonews_abs_intra_plm)

nonews_intra_plm <- plm(intra_day ~ lag(open_open, 0:4) 
                 + lag(Index_Change, 0:4)
                 + Monday + Tuesday + Wednesday + Thursday
                 , data = clean_data, index = c("Code", "Date"), model = "within")
summary(nonews_intra_plm)



abs_intra_day_controls_table <- stargazer(nonews_intra_plm, nonews_intra_plm,
                                          table.placement = "H", df = FALSE,
                                          title = "Auxilliary regressions for sLDA")


# Store the panel residuals
resids <- nonews_intra_plm$residuals
abs_resids <- nonews_abs_intra_plm$residuals
resid_data <- cbind(index(resids), as.numeric(resids))
abs_resid_data <- cbind(index(abs_resids), as.numeric(abs_resids))
colnames(resid_data) <- c(colnames(resid_data)[1:2], "intra_residual")
colnames(abs_resid_data) <- c(colnames(resid_data)[1:2], "abs_intra_residual")

merge_data <- merge(clean_data, resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- merge(merge_data, abs_resid_data, by = c("Code", "Date"), all.x = TRUE)
merge_data <- pdata.frame(merge_data, index = c("Code", "Date"))


test_plm <- plm(abs_intra_residual ~ mention,
                data = merge_data, index = c("Code", "Date"), model = "within")
summary(test_plm)

#write.csv(merge_data, "headline_matched_residuals.csv", row.names = FALSE)
headlines_only <- merge_data[which(merge_data$mention == 1 & 
                                     !is.na(merge_data$intra_residual)),]
write.csv(headlines_only, "headline_matched_residuals.csv", row.names = FALSE)
# headlines_only <- read.csv("headline_matched_residuals.csv", stringsAsFactors = FALSE)

clean_dir <- "~/Documents/DPhil/Clean_Data"
export_filename = paste(clean_dir, "FT/matched/companyarticles_matched_residuals.csv", sep = "/")
write.csv(headlines_only, file = export_filename, row.names = FALSE)
# total_short <- read.csv(total_filename, stringsAsFactors = FALSE)




# Plot the residuals to check if they're plausibly normal
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Plot the daily percentage change in price
ggplot(merge_data, aes(intra_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Intra-day Percentage Change Residual")
export_filename = paste(figure_location, "all_res_density.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)

ggplot(headlines_only, aes(intra_residual)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Intra-day Percentage Change Residual, mention-only")
export_filename = paste(figure_location, "headonly_res_density.png", sep = "/")
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