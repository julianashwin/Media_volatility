setwd("~/Documents/GitHub/Media_volatility")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)
require(lfe)
require(fixest)
require(lubridate)
require(DoubleML)
library(mediation)


felm_DK_se <- function(reg_formula, df_panel){
  
  # Estimate regressions with feols and felm
  model <- feols(reg_formula, data = df_panel)
  model_felm <- felm(reg_formula, data = df_panel)
  
  stopifnot(length(model_felm$se) ==  
              length(summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]))
  model_felm$se <- summary(model, vcov = DK ~ period)$coeftable[,"Std. Error"]
  model_felm$tval <- summary(model, vcov = DK ~ period)$coeftable[,"t value"]
  model_felm$pval <- summary(model, vcov = DK ~ period)$coeftable[,"Pr(>|t|)"]
  return(model_felm)
}

# Import the panel data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/BTR_FT_data.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)


panel_df <- pdata.frame(total_data, index = c("Code", "period"))
panel_df <- data.frame(panel_df[,which(!str_detect(names(panel_df), "text"))])


"
Time varying effect
"
med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put")
model_df <- panel_df[complete.cases(panel_df[,med_vars]),]
model <- summary(feols(highlow ~ mention*as.factor(year)-mention-as.factor(year) + 
                         highlow_1lag + VI_put|Code + period, model_df), vcov = DK ~ period)
coef_table <- data.frame(model$coeftable)
coef_table$year <- rownames(coef_table)
coef_table <- coef_table[which(!(coef_table$year %in% c("highlow_1lag", "VI_put"))),]
coef_table$year <- as.numeric(str_remove(coef_table$year, "mention:as\\.factor\\(year\\)"))

ggplot(coef_table, aes(x = year)) + theme_bw() + 
  geom_ribbon(aes(ymax = Estimate + 1.96*abs(Std..Error), ymin = Estimate - 1.96*abs(Std..Error)), alpha = 0.3) +
  geom_line(aes( y = Estimate)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed")
  


"
Mediation effect
mention -> lVolume -> highlow
"
med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put")
model_df <- panel_df[complete.cases(panel_df[,med_vars]),]

model_df$firm_highlow <- ave(model_df$highlow, model_df$Code)
model_df$time_highlow <- ave(model_df$highlow, model_df$Date)
model_df$firm_lVolume <- ave(model_df$lVolume, model_df$Code, FUN = mean)
model_df$time_lVolume <- ave(model_df$lVolume, model_df$Date, FUN = mean)
model_df$firmyear_highlow <- ave(model_df$highlow, model_df$Code, model_df$year, FUN = mean)
model_df$firmyear_lVolume <- ave(model_df$lVolume, model_df$Code, model_df$year, FUN = mean)
model_df$lVolume_diff <- model_df$lVolume - model_df$lVolume_1lag
model_df$lVolume_excess <- model_df$lVolume - model_df$firmyear_lVolume


df_agg <- aggregate(model_df[,c("mention", "highlow", "lVolume")], 
                    by = list(firm = model_df$Code), FUN = mean)
cor.test(df_agg$mention, df_agg$lVolume)


summary(lm(return ~ mention, model_df))

## Total effect
fit.totaleffect <- felm_DK_se(highlow ~ mention + abs_overnight +
                          Code*highlow_1lag-Code + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                          lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                          VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                          abs_intra_1lag + abs_intra_2lag + abs_intra_3lag + abs_intra_4lag + abs_intra_5lag +
                          abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                          return_1lag + return_2lag + return_3lag + return_4lag + return_5lag
                        | Code + period, model_df)
summary(fit.totaleffect)
# Mediator model 
fit.mediator <- felm_DK_se(lVolume ~ mention + abs_overnight +
                       Code*highlow_1lag-Code + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       abs_intra_1lag + abs_intra_2lag + abs_intra_3lag + abs_intra_4lag + abs_intra_5lag +
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag
                     | Code + Date, model_df)
summary(fit.mediator)
# Control for mediator on outcome
fit.dv <- felm_DK_se(highlow ~ mention + abs_overnight + abs_intra_day + lVolume +
                                Code*highlow_1lag-Code + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                                lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                                VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                                abs_intra_1lag + abs_intra_2lag + abs_intra_3lag + abs_intra_4lag + abs_intra_5lag +
                                abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                                return_1lag + return_2lag + return_3lag + return_4lag + return_5lag
                              | Code + period, model_df)
summary(fit.dv)



## Step 1: identify the total effect (X on Y)
fit.totaleffect <- lm(highlow ~ mention + highlow_1lag + lVolume_1lag + VI_put + 
                        Code + time_highlow + time_lVolume, model_df)
summary(fit.totaleffect)
fit.totaleffect <- lm(highlow ~ mention + highlow_1lag + lVolume_1lag + 
                        firm_highlow + time_highlow, model_df)
summary(fit.totaleffect)


## Step 2: effect of independent variable on the mediator (X on Z)
# Must be significant
# Can include covariates here
fit.mediator <- lm(lVolume ~ mention + highlow_1lag + lVolume_1lag + VI_put + 
                     Code + time_highlow + time_lVolume, model_df)
summary(fit.mediator)
fit.mediator <- lm(lVolume ~ mention + highlow_1lag + lVolume_1lag + 
                     firm_highlow + time_highlow, model_df)
summary(fit.mediator)

## Step 3: effect of the mediator on the dependent variable (Z on Y)
# Control for independent variable!
fit.dv <- felm(highlow ~ mention  + lVolume + abs_intra_day +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 abs_intra_1lag + abs_intra_2lag + abs_intra_3lag + abs_intra_4lag + abs_intra_5lag
               | Code + Date, model_df)
summary(fit.dv)
fit.dv <- lm(highlow ~ mention + lVolume + highlow_1lag + lVolume_1lag + VI_put + 
               Code, model_df)
summary(fit.dv)
fit.dv <- lm(highlow ~ mention + lVolume + highlow_1lag + lVolume_1lag + 
               firm_highlow + time_highlow, model_df)
summary(fit.dv)


## Step 4: Causal mediation analysis that estimates and combines models
total_effect <- fit.totaleffect$coefficients["mention",]
acme <- fit.mediator$coefficients["mention",]*fit.dv$coefficients["abs_intra_day",]
acme/total_effect
total_effect <- fit.totaleffect$coefficients["mention",]
acme <- fit.mediator$coefficients["mention",]*fit.dv$coefficients["lVolume",]
acme/total_effect


fit.mediator$coefficients["mention"]

results <- mediate(fit.mediator, fit.dv, treat = "mention", mediator = "lVolume", boot = T, sims = 50)
                  #,covariates = c("highlow_1lag","lVolume_1lag","VI_put","Code","time_highlow","time_lVolume"))
summary(results)
plot(results)


Xnames <- c("highlow_1lag", "lVolume_1lag", "firm_highlow", "time_highlow")
m.med <- multimed(outcome = "highlow", med.main = "lVolume", 
                  med.alt = "abs_intra_day", treat = "mention", 
                  covariates = Xnames, data = model_df, sims = 25)
summary(m.med)




## Step 2b: effect of independent variable on the mediator (X on Z)
# Must be significant
# Can include covariates here
fit.mediatorb <- felm(abs_intra_day ~ mention + highlow_1lag + VI_put | Code + Date, model_df)
summary(fit.mediatorb)

## Step 3b: effect of the mediator on the dependent variable (Z on Y)
# Control for independent variable!
fit.dvb <- felm(highlow~mention + abs_intra_day + highlow_1lag + VI_put | Code + Date, model_df)
summary(fit.dvb)




panel_df$lVol_m_firmyear <- panel_df$lVolume - panel_df$firm_lVolume
summary(feols(highlow ~ mention + highlow_1lag + lVol_m_firmyear |Code + Date, panel_df))

summary(lm(log(mean_Volume) ~ weekday, plot_df))

summary(feols(highlow ~ mention*year_period |Code, panel_df))
summary(feols(highlow ~ mention + log(Volume) |Code, total_data))
summary(feols(highlow ~ mention + log(Volume) + Volume_logdiff |Code, total_data))
summary(felm(highlow ~ mention + period|Code+Date|0|Code+Date, total_data))

summary(felm(highlow ~ mention*as.factor(year_period)-as.factor(year_period)|Code + Date, total_data))

model <- feols(highlow ~ mention*as.factor(year_period)-as.factor(year_period)|Code + Date, total_data)


model1 <- feols(highlow ~ log(Volume) + highlow_1lag|Code+Date, 
                panel_df[which(!is.na(panel_df$highlow) & !is.na(panel_df$mention)),])
model2 <- feols(mention ~ log(Volume) + highlow_1lag|Code+Date, 
                panel_df[which(!is.na(panel_df$highlow) & !is.na(panel_df$mention)),])
summary(lm(model1$residuals ~ model2$residuals))

model1 <- feols(highlow ~ log(Volume) + highlow_1lag|Code+Date, 
                panel_df[which(!is.na(panel_df$highlow) & !is.na(panel_df$highlow_1lag)),])
model2 <- feols(mention ~ log(Volume)|Code+Date, 
                panel_df[which(!is.na(panel_df$highlow) & !is.na(panel_df$highlow_1lag)),])

summary(lm(model1$residuals ~ model2$residuals))


model <- feols(log(Volume) ~ mention + log(Volume_1lag)  + log(Volume_2lag) + log(Volume_3lag) +
                 highlow_1lag + highlow_2lag + highlow_3lag + 
                 abs_intra_day + VI_put + VI_call
               |Code+Date, panel_df)
summary(model, vcov = DK ~ period)


model <- feols(highlow ~ mention + log(Volume) + highlow_1lag|Code+Date, panel_df)
summary(model, vcov = DK ~ period)
model <- feols(log(Volume) ~ mention + log(Volume_1lag)|Code+Date, panel_df)
summary(model, vcov = DK ~ period)

model <- feols(highlow ~  mention + log(Volume) + 
                 highlow_1lag + 
                 log(Volume_1lag) + log(Volume_2lag) |Code+Date, panel_df)
summary(model, vcov = DK ~ period)
model <- feols(highlow ~  mention + 
                 highlow_1lag + VI_put + 
                 log(Volume_1lag) + log(Volume_2lag) |Code+Date, panel_df)
summary(model, vcov = DK ~ period)

model <- feols(Volume_logdiff ~  mention , panel_df)
summary(model, vcov = DK ~ period)

model <- feols(highlow ~ mention + abs_intra_day + 
                 VI_put + VI_put_1lag + highlow_1lag|Code+Date, panel_df)
summary(model, vcov = DK ~ period)

model <- feols(VI_put ~ VI_put_1lag |Code+Date, panel_df)
summary(model, vcov = DK ~ period)

summary(felm(log(Volume) ~ mention + highlow + highlow_1lag|Code+Date|0|Code+Date, total_data))

summary(felm(highlow ~ mention + highlow_1lag|Code+Date|0|Code+Date, total_data))

summary(felm(highlow ~ mention + highlow_1lag+ VI_put + VI_call|Code+Date|0|Code+Date, total_data))

model <- feols(highlow ~ mention 
               + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag
               + abs_intra_day + VI_put + VI_call|Code+Date, total_data)
summary(model, vcov = "iid")
summary(model, vcov = DK ~ Date)
summary(model, vcov = NW ~ Code + Date)
summary(model, vcov = "twoway")


model <- feols(highlow ~ mention + log(Volume) + Volume + 
               + highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag
               + abs_intra_day + VI_put + VI_call|Code+Date, total_data)
summary(model, vcov = "iid")
summary(model, vcov = DK ~ Date)
summary(model, vcov = NW ~ Code + Date)
summary(model, vcov = "twoway")



ggplot(panel_df) +
  geom_density(aes(x = log(Volume)))
ggplot(panel_df) +
  geom_density(aes(x = Volume_logdiff))
ggplot(total_data) +
  geom_density(aes(x = log(Turnover)))
ggplot(total_data) +
  geom_density(aes(x = year_period))

summary(felm(highlow ~ mention + highlow_1lag+ VI_put + VI_call + log(Volume) + log(Turnover)
             |Code+Date|0|Code+Date, total_data))

# Add an abspChange for robustness
total_data$pChange <- total_data$pChange*100
total_data$abspChange <- abs(total_data$pChange)
total_data$abs_open_open <- abs((total_data$Open - plm::lag(total_data$Open))/(total_data$Open))

# Add a High-Low measure to look explicitly at volatility
total_data$highlow = 100*(total_data$High - total_data$Low)/total_data$High
total_data$highlow_close = 100*(total_data$High - total_data$Low)/total_data$Close
total_data$highlow_ln = 100*(log(total_data$High) - log(total_data$Low))

total_data$Index_High <- as.numeric(str_replace_all(total_data$Index_High, ",",""))
total_data$Index_Low <- as.numeric(str_replace_all(total_data$Index_Low, ",",""))


total_data$IndexHighLow <- 100*(total_data$Index_High - total_data$Index_Low)/total_data$Index_High


# Correct a few data errors when time: some negative high-low, as some fields are mixed up 
# sometimes the high is swapped with low or turnover.






### Some initial plots
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Plot the daily percentage change in price
ggplot(total_data, aes(highlow)) + geom_density(kernel = "gaussian") +
  labs(y = "Density", x= "Daily Percentage Change")
export_filename = paste(figure_location, "daily_equity_highlow.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)



# Plot first difference of AAL price to show stationarity
one_data <- data.frame(total_data[which(total_data$Code=="AAL.L"),])
one_data$Date <- as.Date(one_data$Date)
ggplot(one_data, aes(Date)) + geom_line(aes(y = highlow)) + 
  labs(y = "AAL high-low price change (%)")
export_filename = paste(figure_location, "AAL_highlow.png", sep = "/")
ggsave(export_filename, width = 10, height = 4, dpi = 200)






### First compare the pooling and fixed effects with absolute percentage close-to-close change
model1 <- plm(highlow ~ ner_mention, data = total_data,
                index = c("Code", "Date"), model = "pooling")
summary(model1)
model2 = plm(highlow ~ head_mention, data = total_data, 
                index = c("Code", "Date"), model = "pooling")
summary(model2)
model3 <- plm(highlow_ln ~ mention, data = total_data,
                     index = c("Code", "Date"), model = "pooling")
summary(model3)
model4 = plm(highlow ~ ner_mention, data = total_data, 
                index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(highlow ~ head_mention, data = total_data, 
                index = c("Code", "Date"), model = "within")
summary(model5)
model6 = plm(highlow ~ mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model6)

# Combine in table
highlow_fixed_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                    table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                                    title = "High-to-low volatility and mention dummies")

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
                                    table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                                    title = "Intra-day returns and mention dummies")




### Compare fixed for intraday excluding large movements
model1 <- plm(highlow ~ mention, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(highlow ~ mention, data = total_data[which(total_data$highlow < 50),], 
             index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(highlow ~ mention, data = total_data[which(total_data$highlow < 20),],
              index = c("Code", "Date"), model = "within")
summary(model3)
model4 = plm(highlow ~ mention, data = total_data[which(total_data$highlow < 8),], 
             index = c("Code", "Date"), model = "within")
summary(model4)
model5 = plm(highlow ~ mention, data = total_data[which(total_data$highlow < 5),], 
             index = c("Code", "Date"), model = "within")
summary(model5)


# Combine in table
abs_intra_day_exclude_outliers_table <- stargazer(model1, model2, model3, model4, model5,
                                       table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                                       title = "Intra-day returns and mention dummies without large movements",
                                       column.labels = c("Full", "< 50", "< 20", "< 8", "< 5"))




### Formal tests of whether the individual effects significant?
plmtest(both_full_pool, effect = "individual", type = "honda")
plmtest(both_full_pool, effect = "individual", type = "bp")





### Various controls for abs_intra_day
model1 <- plm(abs_intra_day ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_open_open, 1:4), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_open_open, 1:10) + plm::lag(Index_abs_Change, 0:10), 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(abs_intra_day ~ plm::lag(mention,0) +
                plm::lag(abs_open_open, 1:10) +  plm::lag(Index_abs_Change, 0:10) +
                Monday + Tuesday + Wednesday + Thursday, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model4)

# And also for high-to-low volatility
model5 <- plm(highlow ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model5)
model6 <- plm(highlow ~ plm::lag(mention,0) +
                plm::lag(highlow, 1:4), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model6)
model7 <- plm(highlow ~ plm::lag(mention,0) +
                plm::lag(highlow, 1:4) + plm::lag(IndexHighLow, 0:4), 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model7)
model8 <- plm(highlow ~ plm::lag(mention,0) +
                plm::lag(highlow, 1:10) +  plm::lag(IndexHighLow, 0:10) +
                Monday + Tuesday + Wednesday + Thursday, data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model8)

# Combine in table
abs_intra_day_controls_table <- stargazer(model1, model2, model3, model4, model5, model6, model7, model8,
                                          table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                                          title = "Intra-day returns and mention dummies with controls")





### Look at effects of including lags and leads 
model1 <- plm(abs_intra_day ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model1)
model2 <- plm(abs_intra_day ~ plm::lag(mention,-5:5) , data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model2)
model3 <- plm(abs_intra_day ~ plm::lag(mention,-5:5) +
                plm::lag(highlow, 1:4) +  plm::lag(Index_abs_Change, 0:4) +
                Monday + Tuesday + Wednesday + Thursday, 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model3)
model4 <- plm(highlow ~ plm::lag(mention,0), data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model4)
model5 <- plm(highlow ~ plm::lag(mention,-5:5) , data = total_data,
              index = c("Code", "Date"), model = "within")
summary(model5)
model6 <- plm(highlow ~ plm::lag(mention,-5:5) +
                plm::lag(highlow, 1:4) +  plm::lag(IndexHighLow, 0:4) +
                Monday + Tuesday + Wednesday + Thursday, 
              data = total_data, index = c("Code", "Date"), model = "within")
summary(model6)


abs_intra_day_lagsleads_table <- stargazer(model1, model2, model3, model4, model5, model6,
                                       table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                                       title = "Lags and leads of the mention dummy")






### Time effect robustness checks

# Baseline
model1 = plm(abs_intra_day ~  mention , data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(highlow ~  mention , data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model2)

# Time effect
model3 = plm(abs_intra_day ~  mention , data = total_data, 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model3)
model4 = plm(highlow ~  mention , data = total_data, 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model4)

# Remove large movements to see if this restores effect
model5 = plm(abs_intra_day ~  mention , data = total_data[which(total_data$abs_intra_day < 20),], 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model5) # Note that the time effect destroys the effect unless we exclude large movements???
model6 = plm(highlow ~  mention , data = total_data[which(total_data$highlow < 20),], 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model6) 

# Both
model7 <- felm(formula = abs_intra_day ~ mention  | Code + Date, data = total_data)
summary(model7)
model8 <- felm(formula = highlow ~ mention | Code + Date, data = total_data)
summary(model8)



time_effect_table <- stargazer(model1, model3, model5, model7, model2, model4, model6, model8,
                               table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                               title = "Firm and time fixed effects", font.size = "small")



### Time effect robustness checks with lags

# Baseline
model1 = plm(abs_intra_day ~  mention + plm::lag(highlow, 1:10), data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)
model2 = plm(highlow ~  mention + plm::lag(highlow, 1:10), data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model2)

# Time effect
model3 = plm(abs_intra_day ~  mention + plm::lag(highlow, 1:10), data = total_data, 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model3)
model4 = plm(highlow ~  mention + plm::lag(highlow, 1:10), data = total_data, 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model4)

# Remove large movements to see if this restores effect
model5 = plm(abs_intra_day ~  mention + plm::lag(highlow, 1:10), data = total_data[which(total_data$abs_intra_day < 20),], 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model5) # Note that the time effect destroys the effect unless we exclude large movements???
model6 = plm(highlow ~  mention + plm::lag(highlow, 1:10), data = total_data[which(total_data$highlow < 20),], 
             index = c("Code", "Date"), model = "within", effect = "time")
summary(model6) 

# Both
model7 <- felm(formula = abs_intra_day ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = total_data)
summary(model7)
model8 <- felm(formula = highlow ~ mention + plm::lag(highlow, 1:10) | Code + Date, data = total_data)
summary(model8)



time_effect_table <- stargazer(model1, model3, model5, model7, model2, model4, model6, model8,
                               table.placement = "H", df = FALSE, column.sep.width	= "2pt",
                               title = "Firm and time fixed effects with lag controls", font.size = "small")





### Variable coefficient model robustness checks

# Baseline
model1 = plm(abs_intra_day ~  mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model1)
# Variable coefficients
model2 <- pvcm(abs_intra_day ~  plm::lag(mention,0), data = total_data, 
               index = c("Code", "Date"), model = "within")
summary(model2)
# Baseline
model3 = plm(highlow ~  mention, data = total_data, 
             index = c("Code", "Date"), model = "within")
summary(model3)
# Variable coefficients
model4 <- pvcm(highlow ~  plm::lag(mention,0), data = total_data, 
               index = c("Code", "Date"), model = "within")
summary(model4)





# Plot the Variable Coefficient Model results for abs_intra_day
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
  labs(y = "Density", x = "Coefficient values") + ggtitle("Absolute intra-day change")
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"
export_filename = paste(figure_location, "vcm_coeffs_absintraday.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)




# Plot the Variable Coefficient Model results for highlow
coefficient_data <- model4$coefficients
colnames(coefficient_data) <- c("Constant", "mention")
# Remove outlier (CCL) and NAs
coefficient_data <- subset(coefficient_data, !is.na(mention))

# Reshape to plot all on same graph
coefficient_data$id <- 1:nrow(coefficient_data)
df.m <- melt(coefficient_data, "id")

ggplot(data = df.m, aes(x=value)) + geom_density(aes(fill = "blue"),kernel = "gaussian") +
  geom_vline(xintercept = 0) +
  facet_wrap(~variable) + theme(legend.position="none")  + 
  labs(y = "Density", x = "Coefficient values") + ggtitle("High-to-low volatility")
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"
export_filename = paste(figure_location, "vcm_coeffs_highlow.png", sep = "/")
ggsave(export_filename, width = 8, height = 4, dpi = 200)









