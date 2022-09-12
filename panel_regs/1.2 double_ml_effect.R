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
                          "highlow", "mention", "lVolume", "abs_overnight",
                          "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
                          "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag",
                          "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag",
                          "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag",
                          "abs_intra_day", "VI_put", "IndexHighLow")]
panel_df <- panel_df[complete.cases(panel_df),]
panel_df$firm_highlow <- ave(panel_df$highlow, panel_df$Code)
panel_df$day_highlow <- ave(panel_df$highlow, panel_df$Date)
#panel_df$Code <- as.factor(panel_df$Code)



"
Estimate without Volume
"
# Data
x_vars <- c("abs_overnight",
            "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
            "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag",
            "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag",
            "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag",
            "firm_highlow", "day_highlow")
dml_noVol_data = DoubleMLData$new(panel_df,
    y_col = "highlow",
    d_cols = "mention",
    x_cols = x_vars)
summary(felm(highlow ~ mention + 
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
#learner = lrn("regr.ranger", num.trees=500, mtry=6, max.depth=100, min.node.size=2)
ml_l = learner$clone()
ml_m = learner$clone()
# Fit
set.seed(3141)
dml_noVol = DoubleMLPLR$new(dml_noVol_data, ml_l=ml_l, ml_m=ml_m)
dml_noVol$fit()
beep()
print(dml_noVol)


"
Estimate with Volume
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








