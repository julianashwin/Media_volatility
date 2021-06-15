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
import_filename = paste(clean_dir, "FT/matched/all_equityprices_companyarticles.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)

# Format for panel data analysis
total_data <- pdata.frame(total_data, index = c("Code", "Date"))

# Create a random indicator as a robustness test
mention_num <- table(total_data$mention)[2]
total_num <- length(total_data$mention)
rand_ind <- rbinom(total_num,1,(mention_num/total_num))
total_data$random_dummy <- rand_ind

# Add an abspChange for robustness
total_data$pChange <- total_data$pChange*100
total_data$abspChange <- abs(total_data$pChange)




### Some initial plots
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Plot the daily percentage change in price
ggplot(total_data, aes(pChange)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Daily Percentage Change")
export_filename = paste(figure_location, "daily_equity_pchange.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)

# Plot the intra-day percentage change in price
ggplot(total_data, aes(intra_day)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Daily Percentage Change")
export_filename = paste(figure_location, "intra_day_equity_pchange.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)




# Which firms do we identify some articles for?
total_data$Company <- as.factor(total_data$Company)

# Both
freq <- table(total_data[which(total_data$mention==1),]$Company)
length(freq)
col1 <- cbind(names(freq[1:35]),freq[1:35])
col2 <- cbind(names(freq[36:70]),freq[36:70]) 
col3 <- cbind(c(names(freq[71:100]),rep(NA,5)),
              c((freq[71:100]),rep(NA,5)))
#col4 <- cbind(names(freq[61:80]),freq[61:80])
#col5 <- cbind(names(freq[81:100]),freq[81:100])

freqtab <- as.matrix(cbind(col1,col2,col3))
stargazer(freqtab, rownames = FALSE, column.sep.width = "2pt",
          table.placement = "H")



# Plot some individual prices over time

# Anglo American
one_data <- data.frame(total_data[which(total_data$Code=="AAL.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Close)) + 
  geom_vline(aes(xintercept=as.numeric(Date)), color = "blue", linetype = "dashed",
             data = subset(one_data, mention == 1)) +
  labs(y = "AAL Price")
export_filename = paste(figure_location, "AAL_price_mentions.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)

# Just Eat
one_data <- data.frame(total_data[which(total_data$Code=="JE.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Close)) + 
  geom_vline(aes(xintercept=as.numeric(Date)), color = "blue", linetype = "solid",
             data = subset(one_data, mention == 1)) +
  geom_vline(aes(xintercept=as.numeric(Date)), color = "red", linetype = "dashed",
             data = subset(one_data, head_mention == 1)) +
  labs(y = "JE Price")
export_filename = paste(figure_location, "JE_price_mentions.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)


# Micro Focus
one_data <- data.frame(total_data[which(total_data$Code=="MCRO.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Close)) + 
  geom_vline(aes(xintercept=as.numeric(Date)), color = "blue", linetype = "dashed",
             data = subset(one_data, mention == 1)) +
  labs(y = "MCRO Price")
export_filename = paste(figure_location, "MCRO_price_mentions.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)



# Plot first difference of AAL price to show stationarity
one_data <- data.frame(total_data[which(total_data$Code=="AAL.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = intra_day)) + 
  labs(y = "AAL intra-day price change (%)")
export_filename = paste(figure_location, "AAL_intraday_pricechange.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)

# Plot first difference of JE price to show stationarity
one_data <- data.frame(total_data[which(total_data$Code=="JE.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = intra_day)) + 
  labs(y = "JE intra-day price change (%)")
export_filename = paste(figure_location, "JE_intraday_pricechange.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)

# Plot first difference of MCRO price to show stationarity
one_data <- data.frame(total_data[which(total_data$Code=="MCRO.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = intra_day)) + 
  labs(y = "MCRO intra-day price change (%)")
export_filename = paste(figure_location, "MCRO_intraday_pricechange.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)


# Plot volume of AAL price to eye-ball stationarity
one_data <- data.frame(total_data[which(total_data$Code=="AAL.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Volume)) + 
  labs(y = "Volume of AAL shares traded")
export_filename = paste(figure_location, "AAL_volume.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)

# Plot volume of JE price to eye-ball stationarity
one_data <- data.frame(total_data[which(total_data$Code=="JE.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Volume)) + 
  labs(y = "Volume of JE shares traded")
export_filename = paste(figure_location, "JE_volume.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)

# Plot volume of MCRO price to eye-ball stationarity
one_data <- data.frame(total_data[which(total_data$Code=="MCRO.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = Volume)) + 
  labs(y = "Volume of MCRO shares traded")
export_filename = paste(figure_location, "JE_volume.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)




### First compare the pooling and fixed effects with absolute percentage close-to-close change
model1 <- plm(abspChange ~ ner_mention, data = total_data,
                index = c("Code", "Date"), model = "pooling")
summary(model1)
model2 = plm(abspChange ~ head_mention, data = total_data, 
                index = c("Code", "Date"), model = "pooling")
summary(model2)
model3 <- plm(abspChange ~ mention, data = total_data,
                     index = c("Code", "Date"), model = "pooling")
summary(model3)
model4 = plm(abspChange ~ ner_mention, data = total_data, 
                index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(abspChange ~ head_mention, data = total_data, 
                index = c("Code", "Date"), model = "within")
summary(model5)
model6 = plm(abspChange ~ mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model6)

# Combine in table
abspChange_fixed_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                    table.placement = "H", df = FALSE,
                                    title = "Close-to-close returns and mention dummies")

### Compare fixed and pooling for intraday
model1 <- plm(abs_intra_day ~ ner_mention, data = total_data,
              index = c("Code", "Date"), model = "pooling")
summary(model1)
model2 = plm(abs_intra_day ~ head_mention, data = total_data, 
             index = c("Code", "Date"), model = "pooling")
summary(model2)
model3 <- plm(abs_intra_day ~ mention, data = total_data,
              index = c("Code", "Date"), model = "pooling")
summary(model3)
model4 = plm(abs_intra_day ~ ner_mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(abs_intra_day ~ head_mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model5)
model6 = plm(abs_intra_day ~ mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model6)


# Combine in table
abs_intra_day_fixed_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                    table.placement = "H", df = FALSE,
                                    title = "Intra-day returns and mention dummies")




### Compare fixed for intraday excluding large movements
model1 <- plm(abs_intra_day ~ mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(abs_intra_day ~ mention, data = total_data[which(all_data$abs_intra_day < 50),], 
             index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abs_intra_day ~ mention, data = total_data[which(all_data$abs_intra_day < 20),],
              index = c("Code", "Date"), model = "within")
summary(model3)
model4 = plm(abs_intra_day ~ mention, data = total_data[which(all_data$abs_intra_day < 8),], 
             index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(abs_intra_day ~ mention, data = total_data[which(all_data$abs_intra_day < 5),], 
             index = c("Code", "Date"), model = "within")
summary(model5)


# Combine in table
abs_intra_day_exclude_outliers_table <- stargazer(model1, model2, model3, model4, model5,
                                       table.placement = "H", df = FALSE,
                                       title = "Intra-day returns and mention dummies without large movements",
                                       column.labels = c("Full", "< 50", "< 20", "< 8", "< 5"))




### Formal tests of whether the individual effects significant?
plmtest(both_full_pool, effect = "individual", type = "honda")
plmtest(both_full_pool, effect = "individual", type = "bp")



### Look at effects on Volume 
model1 <- plm(Volume ~ ner_mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(Volume ~ head_mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(Volume ~ mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(Turnover ~ ner_mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(Turnover ~ head_mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model5)
model6 <- plm(Turnover ~ mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model6)
# Combine in table
abs_intra_day_fixed_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                       table.placement = "H", df = FALSE,
                                       title = "Volume, Turnover and mention dummies")





### Various controls for abs_intra_day
model1 <- plm(abs_intra_day ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_intra_day, 1:4), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_open_open, 0:4) + plm::lag(Index_abs_Change, 0:4), 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_open_open, 0:4) +  plm::lag(Index_abs_Change, 0:4) +
                Monday + Tuesday + Wednesday + Thursday, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model4)

# Combine in table
abs_intra_day_controls_table <- stargazer(model1, model2, model3, model4,
                                          table.placement = "H", df = FALSE,
                                          title = "Intra-day returns and mention dummies with controls")





### Look at effects of including lags and controls 
model1 <- plm(abs_intra_day ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 <- plm(abs_intra_day ~ plm::lag(mention,-2:2) , data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abs_intra_day ~ plm::lag(mention,-1), 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(abs_intra_day ~ plm::lag(mention,-1) +
                plm::lag(abs_open_open, 0:4) + plm::lag(Index_abs_Change, 0:4) +
                Monday + Tuesday + Wednesday + Thursday, 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model4)


abs_intra_day_lagsleads_table <- stargazer(model1, model2, model3, model4,
                                       table.placement = "H", df = FALSE,
                                       title = "Lags and leads of the mention dummy")



### Dependent variable lags for abspChange
model1 <- plm(abspChange ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 <- plm(abspChange ~ plm::lag(mention,0) +
                plm::lag(abspChange, 1:4), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abspChange ~ plm::lag(mention,0) +
                plm::lag(abspChange, 1:4) + plm::lag(Index_abs_Change, 0:4), 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(abspChange ~ plm::lag(mention,0) + 
                plm::lag(abspChange, 1:4) + 
                plm::lag(Index_abs_Change, 0:4) +
                Monday + Tuesday + Wednesday + Thursday , data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model4)


# Combine in table
abspChange_lags_table <- stargazer(model1, model2, model3, model4,
                                      table.placement = "H", df = FALSE,
                                   title = "Close to close returns and mention dummies")









### Time effect robustness checks

# Baseline
model1 = plm(abs_intra_day ~  mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)

# Time effect
model2 = plm(abs_intra_day ~  mention, data = total_data, 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model2)

# Remove large movements to see if this restores effect
model3 = plm(abs_intra_day ~  mention, data = total_data[which(total_data$abs_intra_day < 20),], 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model3) # Note that the time effect destroys the effect unless we exclude large movements???

time_effect_table <- stargazer(model1, model2, model3,
                                      table.placement = "H", df = FALSE)


### Variable coefficient model robustness checks

# Baseline
model1 = plm(abs_intra_day ~  mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)
# Variable coefficients
model2 <- pvcm(abs_intra_day ~  plm::lag(mention,0), data = total_data, 
               index = c("Code", "Date"), model = "within")
summary(model2)
# Variable coefficients with lag and lead
model3 <- pvcm(abs_intra_day ~  plm::lag(mention,-1:1), data = total_data, index = c("Code", "Date"), 
                  model = "within")
summary(model3)


# Plot the Variable Coefficient Model results
coefficient_data <- model2$coefficients
colnames(coefficient_data) <- c("Constant", "mention")
# Remove outlier (CCL) and NAs
coefficient_data <- subset(coefficient_data, !is.na(mention))

# Reshape to plot all on same graph
coefficient_data$id <- 1:nrow(coefficient_data)
df.m <- melt(coefficient_data, "id")

ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill = "blue"),kernel = "gaussian") +
  geom_vline(xintercept = 0) +
  facet_wrap(~variable) + theme(legend.position="none")  + 
  labs(y = "Density", x = "Coefficient values")
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"
export_filename = paste(figure_location, "vcm_coeffs.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)







#### Cross firm effects
sectors <- unique(no_out$Sector)

no_out_bysector <- no_out[0,]

for (s in sectors){
  print(s)
  
  sector_data <- no_out[which(no_out$Sector == s),]
  
  dates <- sector_data[which(sector_data$headline_mention == 1),]$Date
  print(dim(sector_data))
  
  for (d in dates){
    sector_data$sector_mention <- as.numeric(sector_data$Date == d)
  }
  if (length(dates)==0){
    sector_data$sector_mention <- 0
  }
  no_out_bysector <- rbind(no_out_bysector, sector_data)
  
}

no_out_bysector <- pdata.frame(no_out_bysector, index = c("Code", "Date"))

headlead_depindexlags <- plm(abs_Change_1day ~ lag(headline_mention,-1), 
                             data = no_out, index = c("Code", "Date"),
                             model = "within")
summary(headlead_depindexlags)





# Directional dummies
no_out <- pdata.frame(no_out, index = c("Code", "Date"))
no_out$change_direction <- sign(no_out$pChange)
no_out$Change_1lag <- plm::lag(no_out$pChange,1)
no_out$Change_1lead <- plm::lag(no_out$pChange,-1)
no_out$Change_2lag <- plm::lag(no_out$pChange,2)
no_out$Change_2lead <- plm::lag(no_out$pChange,-2)
no_out$direction <- no_out$both_mention * no_out$change_direction
no_out$lagdirection <- plm::lag(no_out$both_mention,1)* no_out$change_direction
no_out$leaddirection <- plm::lag(no_out$both_mention,-1) * no_out$change_direction
no_out$lag2direction <- plm::lag(no_out$both_mention,2) * no_out$change_direction
no_out$lead2direction <- plm::lag(no_out$both_mention,-2) * no_out$change_direction

head_alldirec <- plm(pChange ~ direction + lagdirection + lag2direction
                     + leaddirection + lead2direction, data = no_out,
                     index = c("Code", "Date"), model = "within")
summary(head_alldirec)
head_leaddirec <- plm(pChange ~ leaddirection, data = no_out,
                      index = c("Code", "Date"), model = "within")
summary(head_leaddirec)
head_leaddirec <- plm(pChange ~ plm::lag(change_direction,-1) +
                        plm::lag(pChange, 1:4) + 
                        plm::lag(Index_Change, 0:4), data = no_out,
                      index = c("Code", "Date"), model = "within")
summary(head_leaddirec)

# Look at persistence of effect - is there a reversal?
headdireclead_depindexlags <- plm(Change_1day ~ lag(mention_lagdirection,c(-1,-10:-20)) +
                                    lag(Change_1day, 1:4) + lag(Index_Change, 0:4), 
                                  data = no_out, index = c("Code", "Date"), model = "within")
summary(headdireclead_depindexlags)



