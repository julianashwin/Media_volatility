setwd("~/Documents/GitHub/Media_volatility")
rm(list=ls())

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(beepr)
library(lfe)
library(stringr)

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/BTR_FT_data.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)

total_data$firm_highlow <- ave(total_data$highlow, total_data$Code)

Xnames <- c("abs_overnight", 
            "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
            "highlow_6lag", "highlow_7lag", "highlow_8lag", "highlow_9lag", "highlow_10lag",
            "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag", 
            "lVolume_6lag", "lVolume_7lag", "lVolume_8lag", "lVolume_9lag", "lVolume_10lag", 
            "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag", 
            "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag",
            "abs_return_6lag", "abs_return_7lag", "abs_return_8lag", "abs_return_9lag", "abs_return_10lag",
            "return_1lag", "return_2lag", "return_3lag", "return_4lag", "return_5lag",
            "return_6lag", "return_7lag", "return_8lag", "return_9lag", "return_10lag")


panel_df <- total_data[,c("Code", "period", "highlow", "mention", "lVolume", "abs_intra_day", Xnames)]
panel_df <- panel_df[complete.cases(panel_df),]
panel_df$firm_highlow <- ave(panel_df$highlow, panel_df$Code)
panel_df$day_highlow <- ave(panel_df$highlow, panel_df$period)
panel_df$firm_lVolume <- ave(panel_df$lVolume, panel_df$Code)
panel_df$day_lVolume <- ave(panel_df$lVolume, panel_df$period)
#panel_df$Code <- as.factor(panel_df$Code)



DoubleML_effects <- data.frame(
  model = c("RF + volatility, volume and return lags + implied volatility",
            "RF + volatility, volume and return lags + implied volatility + realised return",
            "RF + volatility, volume and return lags + implied volatility + trading volume",
            "RF + volatility, volume and return lags + implied volatility + realised return + trading volume"),
  lr_coef = 0, lr_se = 0, rf_reg_coef = 0, rf_reg_se = 0, rf_class_coef = 0, rf_class_se = 0)


"
## Without Volume and realised return
"
# Define variables
x_vars <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", Xnames)
# Estimate y = g(X)
fm_vola <- formula(str_c("highlow ~ ",paste(x_vars, collapse = "+")))
lr_highlow <- lm(fm_vola, panel_df)
rf_highlow <- ranger(
  formula = fm_vola, data = panel_df, 
  num.trees=500)
# Estimate T = m(X) 
fm_mention <- formula(str_c("mention ~ ",paste(x_vars, collapse = "+")))
lr_mention <- lm(fm_mention, panel_df)
# as regression
rf_mention_reg <- ranger(
  formula = fm_mention, data = panel_df, 
  num.trees=500)
# as classification
rf_mention_class <- ranger(
  formula = fm_mention, data = panel_df,  
  classification = TRUE,
  probability = TRUE,
  num.trees=500)
beep()

# Compute residuals of highlow
res_lr_highlow <- lr_highlow$residuals
res_rf_highlow <- rf_highlow$predictions - panel_df$highlow
# Compute residuals of mention
res_lr_mention <- lr_mention$residuals
res_rf_reg_mention <- rf_mention_reg$predictions - panel_df$mention
res_rf_class_mention <- rf_mention_class$predictions[,2] - panel_df$mention
# Compute effects
lr_effect <- summary(lm(res_lr_highlow ~ res_lr_mention))
rf_reg_effect <- summary(lm(res_rf_highlow ~ res_rf_reg_mention))
rf_class_effect <- summary(lm(res_rf_highlow ~ res_rf_class_mention))
# Store results
rowid <- which(DoubleML_effects$model == "RF + volatility, volume and return lags + implied volatility")
DoubleML_effects$lr_coef[rowid] <- lr_effect$coefficients[2,1]
DoubleML_effects$lr_se[rowid] <- lr_effect$coefficients[2,2]
DoubleML_effects$rf_reg_coef[rowid] <- rf_reg_effect$coefficients[2,1]
DoubleML_effects$rf_reg_se[rowid] <- rf_reg_effect$coefficients[2,2]
DoubleML_effects$rf_class_coef[rowid] <- rf_class_effect$coefficients[2,1]
DoubleML_effects$rf_class_se[rowid] <- rf_class_effect$coefficients[2,2]




"
## With realised return
"
# Define variables
x_vars_wret <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", "abs_intra_day", Xnames)
# Estimate y = g(X)
fm_vola_wret <- formula(str_c("highlow ~ ",paste(x_vars_wret, collapse = "+")))
lr_highlow_wret <- lm(fm_vola_wret, panel_df)
rf_highlow_wret <- ranger(
  formula = fm_vola_wret, data = panel_df, 
  num.trees=500)
# Estimate T = m(X) 
fm_mention_wret <- formula(str_c("mention ~ ",paste(x_vars_wret, collapse = "+")))
lr_mention_wret <- lm(fm_mention_wret, panel_df)
# as regression
rf_mention_wret_reg <- ranger(
  formula = fm_mention_wret, data = panel_df, 
  num.trees=500)
# as classification
rf_mention_wret_class <- ranger(
  formula = fm_mention_wret, data = panel_df,  
  classification = TRUE,
  probability = TRUE,
  num.trees=500)
beep()


# Compute residuals of highlow
res_lr_highlow_wret <- lr_highlow_wret$residuals
res_rf_highlow_wret <- rf_highlow_wret$predictions - panel_df$highlow
# Compute residuals of mention
res_lr_mention_wret <- lr_mention_wret$residuals
res_rf_reg_mention_wret <- rf_mention_wret_reg$predictions - panel_df$mention
res_rf_class_mention_wret <- rf_mention_wret_class$predictions[,2] - panel_df$mention
# Compute effects
lr_wret_effect <- summary(lm(res_lr_highlow_wret ~ res_lr_mention_wret))
rf_wret_reg_effect <- summary(lm(res_rf_highlow_wret ~ res_rf_reg_mention_wret))
rf_wret_class_effect <- summary(lm(res_rf_highlow_wret ~ res_rf_class_mention_wret))
# Store results
rowid <- which(DoubleML_effects$model == 
                 "RF + volatility, volume and return lags + implied volatility + realised return")
DoubleML_effects$lr_coef[rowid] <- lr_wret_effect$coefficients[2,1]
DoubleML_effects$lr_se[rowid] <- lr_wret_effect$coefficients[2,2]
DoubleML_effects$rf_reg_coef[rowid] <- rf_wret_reg_effect$coefficients[2,1]
DoubleML_effects$rf_reg_se[rowid] <- rf_wret_reg_effect$coefficients[2,2]
DoubleML_effects$rf_class_coef[rowid] <- rf_wret_class_effect$coefficients[2,1]
DoubleML_effects$rf_class_se[rowid] <- rf_wret_class_effect$coefficients[2,2]




"
## With Volume
"
# Define variables
x_vars_wVol <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", "lVolume", Xnames)
# Estimate y = g(X)
fm_vola_wVol <- formula(str_c("highlow ~ ",paste(x_vars_wVol, collapse = "+")))
lr_highlow_wVol <- lm(fm_vola_wVol, panel_df)
rf_highlow_wVol <- ranger(
  formula = fm_vola_wVol, data = panel_df, 
  num.trees=1000)
# Estimate T = m(X) 
fm_mention_wVol <- formula(str_c("mention ~ ",paste(x_vars_wVol, collapse = "+")))
lr_mention_wVol <- lm(fm_mention_wVol, panel_df)
# as regression
rf_mention_wVol_reg <- ranger(
  formula = fm_mention_wVol, data = panel_df, 
  num.trees=1000)
# as classification
rf_mention_wVol_class <- ranger(
  formula = fm_mention_wVol, data = panel_df,  
  classification = TRUE,
  probability = TRUE,
  num.trees=500)
beep()

# Compute residuals of highlow
res_lr_highlow_wVol <- lr_highlow_wVol$residuals
res_rf_highlow_wVol <- rf_highlow_wVol$predictions - panel_df$highlow
# Compute residuals of mention
res_lr_mention_wVol <- lr_mention_wVol$residuals
res_rf_reg_mention_wVol <- rf_mention_wVol_reg$predictions - panel_df$mention
res_rf_class_mention_wVol <- rf_mention_wVol_class$predictions[,2] - panel_df$mention
# Compute effects
lr_wVol_effect <- summary(lm(res_lr_highlow_wVol ~ res_lr_mention_wVol))
rf_wVol_reg_effect <- summary(lm(res_rf_highlow_wVol ~ res_rf_reg_mention_wVol))
rf_wVol_class_effect <- summary(lm(res_rf_highlow_wVol ~ res_rf_class_mention_wVol))

# Store results
rowid <- which(DoubleML_effects$model == 
                 "RF + volatility, volume and return lags + implied volatility + trading volume")
DoubleML_effects$lr_coef[rowid] <- lr_wVol_effect$coefficients[2,1]
DoubleML_effects$lr_se[rowid] <- lr_wVol_effect$coefficients[2,2]
DoubleML_effects$rf_reg_coef[rowid] <- rf_wVol_reg_effect$coefficients[2,1]
DoubleML_effects$rf_reg_se[rowid] <- rf_wVol_reg_effect$coefficients[2,2]
DoubleML_effects$rf_class_coef[rowid] <- rf_wVol_class_effect$coefficients[2,1]
DoubleML_effects$rf_class_se[rowid] <- rf_wVol_class_effect$coefficients[2,2]




"
## With Volume and realised return
"
# Define variables
x_vars_wVolret <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", 
                 "abs_intra_day", "lVolume", Xnames)
# Estimate y = g(X)
fm_vola_wVolret <- formula(str_c("highlow ~ ",paste(x_vars_wVolret, collapse = "+")))
lr_highlow_wVolret <- lm(fm_vola_wVolret, panel_df)
rf_highlow_wVolret <- ranger(
  formula = fm_vola_wVolret, data = panel_df, 
  num.trees=500)
# Estimate T = m(X) 
fm_mention_wVolret <- formula(str_c("mention ~ ",paste(x_vars_wVolret, collapse = "+")))
lr_mention_wVolret <- lm(fm_mention_wVolret, panel_df)
# as regression
rf_mention_wVolret_reg <- ranger(
  formula = fm_mention_wVolret, data = panel_df, 
  num.trees=500)
# as classification
rf_mention_wVolret_class <- ranger(
  formula = fm_mention_wVolret, data = panel_df,  
  classification = TRUE,
  probability = TRUE,
  num.trees=500)
beep()

# Compute residuals of highlow
res_lr_highlow_wVolret <- lr_highlow_wVolret$residuals
res_rf_highlow_wVolret <- rf_highlow_wVolret$predictions - panel_df$highlow
# Compute residuals of mention
res_lr_mention_wVolret <- lr_mention_wVolret$residuals
res_rf_reg_mention_wVolret <- rf_mention_wVolret_reg$predictions - panel_df$mention
res_rf_class_mention_wVolret <- rf_mention_wVolret_class$predictions[,2] - panel_df$mention
# Compute effects
lr_wVolret_effect <- summary(lm(res_lr_highlow_wVolret ~ res_lr_mention_wVolret))
rf_wVolret_reg_effect <- summary(lm(res_rf_highlow_wVolret ~ res_rf_reg_mention_wVolret))
rf_wVolret_class_effect <- summary(lm(res_rf_highlow_wVolret ~ res_rf_class_mention_wVolret))

# Store results
rowid <- which(DoubleML_effects$model == 
                 "RF + volatility, volume and return lags + implied volatility + realised return + trading volume")
DoubleML_effects$lr_coef[rowid] <- lr_wVolret_effect$coefficients[2,1]
DoubleML_effects$lr_se[rowid] <- lr_wVolret_effect$coefficients[2,2]
DoubleML_effects$rf_reg_coef[rowid] <- rf_wVolret_reg_effect$coefficients[2,1]
DoubleML_effects$rf_reg_se[rowid] <- rf_wVolret_reg_effect$coefficients[2,2]
DoubleML_effects$rf_class_coef[rowid] <- rf_wVolret_class_effect$coefficients[2,1]
DoubleML_effects$rf_class_se[rowid] <- rf_wVolret_class_effect$coefficients[2,2]


write.csv(DoubleML_effects, "figures/DoubleML_effects.csv", row.names = F)






"
Using the DoubleML package
"

# Data
x_vars_Vol <- c("abs_overnight", "lVolume",
            "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
            "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag",
            "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag",
            "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag",
            "firm_highlow", "day_highlow")
dml_Vol_data = DoubleMLData$new(panel_df,
    y_col = "highlow",
    d_cols = "mention",
    x_cols = x_vars_Vol)
summary(felm(highlow ~ mention + lVolume + 
               abs_overnight + 
               highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag +
               lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag + 
               abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag + 
               VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag + 
               firm_highlow + day_highlow, panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_vars))), 
              max.depth = 5, min.node.size = 2)
#learner = lrn("regr.ranger", num.trees=500, mtry=3, max.depth=100, min.node.size=2)
ml_l = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_Vol = DoubleMLPLR$new(dml_Vol_data, ml_l=ml_l, ml_m=ml_m)
dml_Vol$fit()
beep()
print(dml_Vol)

squareroot <- function(x){
  if (x >= 0){
    out <- sqrt(x)
  } else {
    stop('x must be weakly positive')
  }
  return(out)
}

squareroot(0)


































"
Estimate without Volume and abs_intra_day
"
# Data
x_vars <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", Xnames)
dml_noVol_data = DoubleMLData$new(panel_df,
                                  y_col = "highlow",
                                  d_cols = "mention",
                                  x_cols = x_vars)
summary(felm(formula(str_c("highlow ~ mention + ",paste(x_vars, collapse = "+"))), panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_vars))), 
              max.depth = 5, min.node.size = 2)
#learner = lrn("regr.ranger", num.trees=500, mtry=6, max.depth=100, min.node.size=2)
ml_l = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_noVol = DoubleMLPLR$new(dml_noVol_data, ml_l=ml_l, ml_m=ml_m)
dml_noVol$fit()
beep()
dml_noVol$summary()
dml_noVol$predictions
print(dml_noVol)


"
Estimate with abs_intra_day
"
# Data
x_abs_vars <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume", "abs_intra_day", Xnames)
dml_intra_data = DoubleMLData$new(panel_df,
                                  y_col = "highlow",
                                  d_cols = "mention",
                                  x_cols = x_abs_vars)
summary(felm(formula(str_c("highlow ~ mention + ",paste(x_abs_vars, collapse = "+"))), panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_vars))), 
              max.depth = 5, min.node.size = 2)
#learner = lrn("regr.ranger", num.trees=500, mtry=6, max.depth=100, min.node.size=2)
ml_l = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_intra = DoubleMLPLR$new(dml_intra_data, ml_l=ml_l, ml_m=ml_m, n_folds = 3, n_rep = 3)
dml_intra$fit()
beep()
dml_intra$summary()
dml_intra$predictions
print(dml_intra)



"
Estimate with Volume and without abs_intra_day
"
# Data
x_vars <- c("firm_highlow", "day_highlow", "firm_lVolume", "day_lVolume",  "lVolume", Xnames)
dml_wVol_data = DoubleMLData$new(panel_df,
                                 y_col = "highlow",
                                 d_cols = c("mention"),
                                 x_cols = x_vars)
fm_wVol = formula(str_c("highlow ~ mention +",paste(x_vars, collapse = "+")))
summary(felm(fm_wVol, panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees = 500, mtry = floor(sqrt(length(x_vars))), 
              max.depth = 5, min.node.size = 2)
#learner = lrn("regr.ranger", num.trees=500, mtry=6, max.depth=100, min.node.size=2)
ml_l = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_wVol = DoubleMLPLR$new(dml_wVol_data, ml_l=ml_l, ml_m=ml_m, n_folds = 3, n_rep = 1,
                           dml_procedure = "dml1")
dml_wVol$tune()
dml_wVol$fit()
beep()
dml_wVol$summary()


