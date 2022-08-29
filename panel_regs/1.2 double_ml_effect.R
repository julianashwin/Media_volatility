setwd("~/Documents/GitHub/Media_volatility")
rm(list=ls())

library(DoubleML)
library(mlr3)
library(mlr3learners)
library(ranger)
library(beepr)
library(lfe)

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/BTR_FT_data.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)

total_data$firm_highlow <- ave(total_data$highlow, total_data$Code)

panel_df <- total_data[,c("Code", "Date",
                          "highlow", "mention", "lVolume", 
                          "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
                          "firm_highlow", "abs_intra_day", "VI_put", "VI_call", "IndexHighLow")]
panel_df <- panel_df[complete.cases(panel_df),]
panel_df$Code <- as.factor(panel_df$Code)



"
Estimate without Volume
"
# Data
dml_noVol_data = DoubleMLData$new(panel_df,
                            y_col = "highlow",
                            d_cols = "mention",
                            x_cols = c("Code", "IndexHighLow",
                                       "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
                                       "VI_put", "VI_call"))
summary(felm(highlow ~ mention + highlow_1lag + highlow_2lag  + highlow_3lag + highlow_4lag + highlow_5lag + 
               VI_put + VI_call + IndexHighLow| Code , panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees=500, mtry=floor(sqrt(n_vars)), max.depth=5, min.node.size=2)
learner = lrn("regr.ranger", num.trees=500, mtry=3, max.depth=100, min.node.size=2)
ml_g = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_noVol = DoubleMLPLR$new(dml_noVol_data, ml_g=ml_g, ml_m=ml_m)
dml_noVol$fit()
beep()
print(dml_noVol)


"
Estimate with Volume
"
# Data
dml_Vol_data = DoubleMLData$new(panel_df,
                                  y_col = "highlow",
                                  d_cols = "mention",
                                  x_cols = c("lVolume", "Code", "IndexHighLow",
                                             "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
                                             "VI_put", "VI_call"))
summary(felm(highlow ~ mention + lVolume + highlow_1lag + highlow_2lag  + highlow_3lag + highlow_4lag + highlow_5lag + 
             VI_put + VI_call + IndexHighLow | Code, panel_df))
# Models
lgr::get_logger("mlr3")$set_threshold("warn")
learner = lrn("regr.ranger", num.trees=500, mtry=floor(sqrt(n_vars)), max.depth=5, min.node.size=2)
learner = lrn("regr.ranger", num.trees=500, mtry=3, max.depth=100, min.node.size=2)
ml_g = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_Vol = DoubleMLPLR$new(dml_Vol_data, ml_g=ml_g, ml_m=ml_m)
dml_Vol$fit()
beep()
print(dml_Vol)








