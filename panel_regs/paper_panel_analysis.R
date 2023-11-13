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
library(DescTools)
library(tidyverse)


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

get_quantile <- function(x, qnt, as_numeric = TRUE){
  
  x_qnts <- cut(x, breaks = Quantile(x, probs = seq(0,1,1/qnt), na.rm = T),
                labels = 1:qnt, include.lowest = TRUE)
  if (as_numeric){
    x_qnts <- as.numeric(x_qnts)
  }
  return(x_qnts)
}


# Import the panel data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/BTR_FT_data.csv", sep = "/")
total_data <- read_csv(import_filename)

total_data <- total_data %>%
  mutate(earnings_count = str_count(text, "earnings")) 
  
as_tibble(total_data) %>%
  filter(text != "") %>% 
  dplyr::select(Code, Date, text) %>%
  mutate(report_count = str_count(text, "earnings")) %>%
  ggplot(aes(x = report_count)) + theme_bw() + geom_histogram()

as_tibble(total_data) %>%
  filter(Code == "MCRO.L") %>% #, Date >="2011-01-01" & Date <= "2012-01-01") %>%
  mutate(Date = as.Date(Date)) %>%
  ggplot(aes(x = Date, y = highlow)) + theme_bw() + 
  geom_line() + 
  geom_vline(data = filter(total_data, mention == 1 & Code == "MCRO.L"),
             aes(xintercept=as.Date(Date)), color = "blue", linetype = "dashed") +
  labs(y = "MCRO volatility")
ggsave("figures/MCRO_price_mentions.pdf", width = 6, height = 2.5)




"
First look
"
model1 <- felm_DK_se(highlow ~ mention, total_data)
model2 <- felm_DK_se(highlow ~ mention | Code, total_data)
model3 <- felm_DK_se(highlow ~ mention | Code + period, total_data)
model4 <- felm_DK_se(highlow ~ mention + highlow_1lag | Code + period, total_data)
model5 <- felm_DK_se(highlow ~ mention + highlow_1lag + abs_overnight | Code + period, total_data)
model6 <- felm_DK_se(highlow ~ mention + highlow_1lag + abs_overnight + 
                       VI_put_1lag + VI_call_1lag| Code + period, total_data)
model7 <- felm_DK_se(highlow ~ mention + abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag +
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     | Code + period, total_data)
models <- c(model1, model2, model3, model4, model5, model6, model7)
stargazer(model1, model2, model3, model4, model5, model6, model7,
          table.placement = "H", df = F)



"
Various controls
"
persist_coefs <- data.frame(model = c("1) Baseline", "2) LR + volatility lags", "3) LR + volatility, volume and return lags", 
                                      "4) DoubleML + volatility, volume and return lags",
                                      "5) LR + volatility, volume and return lags + implied volatility",
                                      "6) LR + volatility, volume and return lags + implied volatility + realised return",
                                      "7) DoubleML + volatility, volume and return lags + implied volatility",
                                      "8) DoubleML + volatility, volume and return lags + implied volatility + realised return"), 
                            coef = 0, se = 0)

persist_coefs$model_factor <- factor(persist_coefs$model, ordered = T,
                                     c("1) Baseline", "2) LR + volatility lags", "3) LR + volatility, volume and return lags", 
                                       "4) DoubleML + volatility, volume and return lags",
                                       "5) LR + volatility, volume and return lags + implied volatility",
                                       "6) LR + volatility, volume and return lags + implied volatility + realised return",
                                       "7) DoubleML + volatility, volume and return lags + implied volatility",
                                       "8) DoubleML + volatility, volume and return lags + implied volatility + realised return"))


## Baseline model
model1 <- felm_DK_se(highlow ~ mention + abs_overnight + 
                       highlow_1lag | Code + period, total_data)
persist_coefs$coef[which(persist_coefs$model == "1) Baseline")] <- summary(model1)$coefficients["mention","Estimate"]
persist_coefs$se[which(persist_coefs$model == "1) Baseline")] <- summary(model1)$coefficients["mention","Std. Error"]

## More highlow lags
model2 <- felm_DK_se(highlow ~ mention  + abs_overnight + 
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag
                     |Code + period, total_data)
persist_coefs$coef[which(persist_coefs$model == "2) LR + volatility lags")] <- summary(model2)$coefficients["mention","Estimate"]
persist_coefs$se[which(persist_coefs$model == "2) LR + volatility lags")] <- summary(model2)$coefficients["mention","Std. Error"]

## More price movement lags
model3 <- felm_DK_se(highlow ~ mention + abs_overnight + 
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
persist_coefs$coef[which(persist_coefs$model == "3) LR + volatility, volume and return lags")] <- summary(model3)$coefficients["mention","Estimate"]
persist_coefs$se[which(persist_coefs$model == "3) LR + volatility, volume and return lags")] <- summary(model3)$coefficients["mention","Std. Error"]

## Implied volatility lags
model4 <- felm_DK_se(highlow ~ mention + abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
persist_coefs$coef[which(persist_coefs$model == "5) LR + volatility, volume and return lags + implied volatility")] <- summary(model4)$coefficients["mention","Estimate"]
persist_coefs$se[which(persist_coefs$model == "5) LR + volatility, volume and return lags + implied volatility")] <- summary(model4)$coefficients["mention","Std. Error"]


## Realised returns
model5 <- felm_DK_se(highlow ~ mention + abs_overnight + abs_intra_day +
                     VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
persist_coefs$coef[which(persist_coefs$model == 
                           "6) LR + volatility, volume and return lags + implied volatility + realised return")] <- 
  summary(model5)$coefficients["mention","Estimate"]
persist_coefs$se[which(persist_coefs$model == 
                         "6) LR + volatility, volume and return lags + implied volatility + realised return")] <- 
  summary(model5)$coefficients["mention","Std. Error"]

#DoubleML_effects <- read.csv("figures/DoubleML_effects.csv")

stargazer(model1, model2, model3, model4, model5,
          table.placement = "H", df = F, 
          title = "Controlling for a wide set of past price movements and trading activity")


## Random forest 
persist_coefs$coef[which(persist_coefs$model == 
                           "4) DoubleML + volatility, volume and return lags")] <- 0.15284
persist_coefs$se[which(persist_coefs$model == 
                         "4) DoubleML + volatility, volume and return lags")] <- 0.02116

persist_coefs$coef[which(persist_coefs$model == 
                           "7) DoubleML + volatility, volume and return lags + implied volatility")] <- 0.16793
persist_coefs$se[which(persist_coefs$model == 
                         "7) DoubleML + volatility, volume and return lags + implied volatility")] <- 0.02209

## Random forest with realised returns
persist_coefs$coef[which(persist_coefs$model == 
                           "8) DoubleML + volatility, volume and return lags + implied volatility + realised return")] <- 0.14254
persist_coefs$se[which(persist_coefs$model == 
                         "8) DoubleML + volatility, volume and return lags + implied volatility + realised return")] <- 0.01946 




ggplot(persist_coefs, aes(y=fct_rev(model_factor), x=coef)) + theme_bw() + 
  geom_point(shape=21, size=3, fill="white") +
  geom_errorbar(width=.3, aes(xmin=coef-1.96*se, xmax=coef+1.96*se)) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = "Model", x = "FT article effect")

ggsave("figures/effect_controls.pdf", width = 8, height = 2.5)


## Import the DoubleML effects. For some reason get different results doing manually than with the DOubleML package?
## Gonna go with the DoubleML package for now...
DoubleML_effects <- read.csv("figures/DoubleML_effects.csv")
DoubleML_keep  <- DoubleML_effects %>%
  rename(coef = rf_reg_coef, se = rf_reg_se) %>%
  select(model, coef, se) %>%
  mutate(coef = round(coef, 3), se = round(se,))
DoubleML_keep


as_tibble(total_data) %>% filter(Code == "RBS.L", Date < "2009-01-19") %>% 
  select(Code, highlow, abs_intra_day, VI_put, VI_call) %>% 
  group_by(Code) %>% 
  summarise_all(mean, na.rm = T)
as_tibble(total_data) %>% filter(Code == "RBS.L", Date < "2009-01-19") %>% 
  select(Code, highlow, abs_intra_day, VI_put, VI_call) %>% 
  group_by(Code) %>% 
  summarise_all(sd, na.rm = T)

as_tibble(total_data) %>% 
  filter(Date %in% c("2009-01-12","2009-01-13","2009-01-14","2009-01-15",
                     "2009-01-16","2009-01-19","2009-01-20") & Code == "RBS.L") %>% 
  select(Code, Date, highlow, abs_intra_day, VI_put, VI_call) 

as_tibble(total_data) %>% 
  filter(Date %in% c("2009-01-12","2009-01-13","2009-01-14","2009-01-15",
                     "2009-01-16","2009-01-19","2009-01-20") & Code == "RBS.L") %>% 
  select(Code, Date, highlow, abs_overnight, abs_intra_day, VI_put, VI_call) %>% 
  ggplot(aes(x = as.Date(Date))) + theme_bw() + 
  geom_line(aes(y = highlow/max(highlow), color = "volatility")) +
  geom_line(aes(y = abs_intra_day/max(abs_intra_day), color = "abs return")) + 
  geom_line(aes(y = VI_put/max(VI_put), color = "VI_put")) + 
  geom_line(aes(y = VI_call/max(VI_call), color = "VI_call"))  


0.1229409*0.576 + 0.6969798*66.4 + 0.6332801*1.13 

"
Forward looking and persistence experiments variables
"
## Import the LIWC dictionaries
import_filename = paste(clean_dir, "FT/matched/LIWC_2015_Results_short_articles.csv", sep = "/")
LIWC_data <- read.csv(import_filename, stringsAsFactors = FALSE) %>%
  dplyr::select(-highlow, - article_id, -headline, -short_text, -obs_id, -clean_text)

merge_data <- as_tibble(total_data) %>%
  left_join(LIWC_data, by = c("Code", "Date")) %>%
  mutate_at(names(LIWC_data)[3:ncol(LIWC_data)], ~replace_na(.,0)) %>%
  group_by(Code) %>%
  mutate(mention_1lead = dplyr::lead(mention, n = 1, order_by = period))
  


model1 <- felm_DK_se(highlow ~ mention + abs_overnight + 
                      VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                      VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                      highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                      highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                      lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                      lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                      abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                      abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                      return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                      return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                    |Code + period, merge_data)

model2 <- felm_DK_se(highlow ~ mention + focusfuture + focuspast + 
                      abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)

model3 <- felm_DK_se(highlow ~ mention*Monday-Monday + focusfuture + focuspast + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)

model4 <- felm_DK_se(highlow ~ mention*Monday-Monday + mention*mention_1lead + focusfuture + focuspast + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)

summary(model4)

stargazer(model1, model2, model3, model4,
          table.placement = "H", df = F)




"
Controlling for specific text features
"

merge_data %>%
  filter(mention == 1) %>%
  ggplot() + theme_bw() + 
  geom_density(aes(x = LM_sentiment))

merge_data$neg_sentiment <- as.numeric(merge_data$LM_sentiment < 0)
merge_data$pos_sentiment <- as.numeric(merge_data$LM_sentiment > 0)
merge_data$earnings <- as.numeric(merge_data$earnings != 0)

model1 <- felm_DK_se(highlow ~ mention + LM_sentiment + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)
model2 <- felm_DK_se(highlow ~ pos_sentiment + neg_sentiment +
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)
model3 <- felm_DK_se(return ~ LM_sentiment + 
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)
model4 <- felm_DK_se(overnight ~ LM_sentiment + 
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)
model5 <- felm_DK_se(intra_day ~ LM_sentiment +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, merge_data)
summary(model5)


stargazer(model1, model3, model4, model5,
          table.placement = "H", df = F)


  
  
"
Time varying effect
"
med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put_1lag")
model_df <- total_data[complete.cases(total_data[,med_vars]), which(!str_detect(names(total_data), "text"))]
model <- summary(feols(highlow ~ mention*as.factor(year)-mention-as.factor(year) + 
                         abs_overnight + 
                         VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                         VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                         highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                         highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                         lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                         lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                         abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                         abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                         return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                         return_6lag + return_7lag + return_8lag + return_9lag + return_10lag|Code + period,
                       total_data), vcov = DK ~ period)
model <- summary(feols(highlow ~ mention*as.factor(year)-mention-as.factor(year) + 
                         highlow_1lag + VI_put_1lag|Code + period, model_df), vcov = DK ~ period)

coef_table <- data.frame(model$coeftable)
coef_table$year <- rownames(coef_table)
coef_table <- coef_table[which(str_detect(coef_table$yea,"as\\.factor")),]
coef_table$year <- as.numeric(str_remove(coef_table$year, "mention:as\\.factor\\(year\\)"))

ggplot(coef_table, aes(x = year)) + theme_bw() + 
  geom_ribbon(aes(ymax = Estimate + 1.96*abs(Std..Error), ymin = Estimate - 1.96*abs(Std..Error)), alpha = 0.2) +
  geom_line(aes( y = Estimate)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(x = "Year", y = "FT article effect")
ggsave("figures/effect_year.pdf", width = 6, height = 2.5)





"
Persistence of effect 
"
med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put")
model_df <- total_data[complete.cases(total_data[,med_vars]), which(!str_detect(names(total_data), "text"))]
model_df <- model_df %>% 
  group_by(Code) %>%
  mutate(highlow_1lead = dplyr::lead(highlow, n = 1, order_by = period)) %>%
  mutate(highlow_2lead = dplyr::lead(highlow, n = 2, order_by = period)) %>%
  mutate(highlow_3lead = dplyr::lead(highlow, n = 3, order_by = period)) %>%
  mutate(highlow_4lead = dplyr::lead(highlow, n = 4, order_by = period))
  relocate(highlow, highlow_1lead, .after = Open)
model <- summary(feols(highlow ~ mention + abs_overnight +
                         VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                         VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                         highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                         highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                         lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                         lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                         abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                         abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                         return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                         return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                       |Code + period, model_df), vcov = DK ~ period)
model_1lead <- summary(feols(highlow_1lead ~ mention + abs_overnight +
                               VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                               VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                               highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                               highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                               lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                               lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                               abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                               abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                               return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                               return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                       |Code + period, model_df), vcov = DK ~ period)
model_2lead <- summary(feols(highlow_2lead ~ mention + abs_overnight +
                               VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                               VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                               highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                               highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                               lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                               lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                               abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                               abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                               return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                               return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                             |Code + period, model_df), vcov = DK ~ period)
model_3lead <- summary(feols(highlow_3lead ~ mention + abs_overnight +
                               VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                               VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                               highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                               highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                               lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                               lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                               abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                               abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                               return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                               return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                             |Code + period, model_df), vcov = DK ~ period)
model_4lead <- summary(feols(highlow_4lead ~ mention + abs_overnight +
                               VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                               VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                               highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                               highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                               lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                               lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                               abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                               abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                               return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                               return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                             |Code + period, model_df), vcov = DK ~ period)

coef_table <- data.frame(lead = 0:10, coef = 0, se = 0)
coef_table$coef[coef_table$lead == 0] <- model$coefficients["mention"]
coef_table$se[coef_table$lead == 0] <- model$se["mention"]
coef_table$coef[coef_table$lead == 1] <- model_1lead$coefficients["mention"]
coef_table$se[coef_table$lead == 1] <- model_1lead$se["mention"]
coef_table$coef[coef_table$lead == 2] <- model_2lead$coefficients["mention"]
coef_table$se[coef_table$lead == 2] <- model_2lead$se["mention"]
coef_table$coef[coef_table$lead == 3] <- model_3lead$coefficients["mention"]
coef_table$se[coef_table$lead == 3] <- model_3lead$se["mention"]
coef_table$coef[coef_table$lead == 4] <- model_4lead$coefficients["mention"]
coef_table$se[coef_table$lead == 4] <- model_4lead$se["mention"]

coef_table %>%
  ggplot(aes(x = lead)) + theme_bw() + 
  geom_ribbon(aes(ymax = coef + 1.96*abs(se), ymin = coef - 1.96*abs(se)), alpha = 0.2) +
  geom_line(aes( y = coef)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  labs(y = "FT article effect", x = "Days since article")
  
ggsave("figures/effect_persistence.pdf", width = 4, height = 3)



"
Firm heterogeneity effect
"
model_eachfirm <- summary(feols(highlow ~ mention*as.factor(Code)-mention-as.factor(Code) + 
                         abs_overnight +
                         VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                         VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                         highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                         highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                         lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                         lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                         abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                         abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                         return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                         return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                       |Code + period, total_data), vcov = DK ~ period)
coefs <- summary(model_eachfirm)$coefficients
mention_effects <- coefs[str_detect(names(coefs), "mention:as.factor")]
mean(mention_effects)
hist(mention_effects)


med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put")
model_df <- total_data[complete.cases(total_data[,med_vars]), which(!str_detect(names(total_data), "text"))]
annual_df <- model_df %>%
  group_by(Code, year) %>%
  summarise(firmyear_highlow = mean(highlow, na.rm= TRUE),
            firmyear_lVolume = mean(lVolume, na.rm= TRUE),
            firmyear_lmarket_value = mean(lmarket_value, na.rm= TRUE)) %>%
  group_by(Code) %>%
  arrange(Code, year) %>%
  mutate(firmyear_highlow_lag = dplyr::lag(firmyear_highlow, n = 1),
         firmyear_lVolume_lag = dplyr::lag(firmyear_lVolume, n = 1),
         firmyear_lmarket_value_lag = dplyr::lag(firmyear_lmarket_value, n = 1)) %>%
  group_by(year) %>%
  filter(!is.na(firmyear_highlow_lag)) %>%
  arrange(year) %>%
  mutate(firmyear_lmarket_value_lag_quart = get_quantile(firmyear_lmarket_value_lag, 4),
         firmyear_highlow_lag_quart = get_quantile(firmyear_highlow_lag, 4),
         firmyear_lVolume_lag_quart = get_quantile(firmyear_lVolume_lag, 4))
model_df <- model_df %>%
  left_join(annual_df) %>%
  group_by(Code) %>%
  mutate(firm_mention = mean(mention, na.rm= TRUE)) %>%
  group_by(year) %>%
  mutate(lmarket_value_quart = get_quantile(lmarket_value_1lag, 4)) %>%
  mutate(highlow_quart = get_quantile(highlow, 4)) %>%
  mutate(lVolume_quart = get_quantile(lVolume, 4),
         lhighlow = log(highlow + 1)) %>%
  ungroup() %>%
  mutate(mention_quart = get_quantile(firm_mention, 4))
 
cor.test(model_df$firmyear_lmarket_value_lag_quart, model_df$firm_mention)
cor.test(model_df$firmyear_lmarket_value_lag_quart, model_df$lmarket_value_quart)


model1 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_lmarket_value_lag_quart)-mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model1)
model2 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_highlow_lag_quart) - mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model2)
model3 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_lVolume_lag_quart)-mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model3)

stargazer(model1, model2, model3, 
          table.placement = "H", df = F)


"
Coefficients without new information controls
"
model1 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_lmarket_value_lag_quart)-mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model1)
model2 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_highlow_lag_quart) - mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model2)
model3 <- felm_DK_se(highlow ~ mention*as.factor(firmyear_lVolume_lag_quart)-mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model3)


stargazer(model1, model2, model3, title = "No information controls",
          table.placement = "H", df = F)




"
Coefficients with log(1 + volatility)
"
model1 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_lmarket_value_lag_quart)-mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model1)
model2 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_highlow_lag_quart) - mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model2)
model3 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_lVolume_lag_quart)-mention + 
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model3)


stargazer(model1, model2, model3, title = "log volatility",
          table.placement = "H", df = F)






"
Coefficients with no information controls and log(1 + volatility)
"
model1 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_lmarket_value_lag_quart)-mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model1)
model2 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_highlow_lag_quart) - mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model2)
model3 <- felm_DK_se(lhighlow ~ mention*as.factor(firmyear_lVolume_lag_quart)-mention + 
                       #abs_overnight +
                       #VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       #VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, model_df)
summary(model3)


stargazer(model1, model2, model3, title = "log volatility and no new information",
          table.placement = "H", df = F)











"
Mediation effect
mention -> lVolume -> highlow
"

model_df <- model_df[]
med_vars <- c("highlow","mention","lVolume","highlow_1lag", "lVolume_1lag", "VI_put_1lag")
model_df <- total_data[complete.cases(total_data[,med_vars]), which(!str_detect(names(total_data), "text"))]

## Baseline model
model1 <- felm_DK_se(highlow ~ mention + 
                                abs_overnight +
                                VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                                VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                                highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                                highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                                lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                                lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                                abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                                abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                                return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                                return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                              | Code + period, total_data)
## lVolume model
model2 <- felm_DK_se(lVolume ~ mention +
                             abs_overnight +
                             VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                             VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                             highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                             highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                             lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                             lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                             abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                             abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                             return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                             return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                           | Code + period, total_data)
model3 <- felm_DK_se(abs_intra_day ~ mention +
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     | Code + period, total_data)
model4 <- felm_DK_se(highlow ~ mention + abs_intra_day + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
model5 <- felm_DK_se(highlow ~ mention + lVolume + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
model6 <- felm_DK_se(highlow ~ mention + abs_intra_day +  lVolume + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F)


summary(model5)
total_effect <- model1$coefficients["mention",]
Volume_acme <- model2$coefficients["mention",]*model5$coefficients["lVolume",]
return_acme <- model3$coefficients["mention",]*model4$coefficients["abs_intra_day",]
Volume_direct_effect <- model5$coefficients["mention",]
return_direct_effect <- model4$coefficients["mention",]

Volume_acme/total_effect
return_acme/total_effect


"
New version of Volume effect
"
med_vars <- c("highlow","mention","lVolume", "abs_overnight", "VI_put_1lag", "VI_put_2lag", "VI_put_3lag", "VI_put_4lag", "VI_put_5lag", 
                       "VI_call_1lag", "VI_call_2lag", "VI_call_3lag", "VI_call_4lag", "VI_call_5lag", 
                       "highlow_1lag", "highlow_2lag", "highlow_3lag", "highlow_4lag", "highlow_5lag",
                       "highlow_6lag", "highlow_7lag", "highlow_8lag", "highlow_9lag", "highlow_10lag", 
                       "lVolume_1lag", "lVolume_2lag", "lVolume_3lag", "lVolume_4lag", "lVolume_5lag", 
                       "lVolume_6lag", "lVolume_7lag", "lVolume_8lag", "lVolume_9lag", "lVolume_10lag",
                       "abs_return_1lag", "abs_return_2lag", "abs_return_3lag", "abs_return_4lag", "abs_return_5lag", 
                       "abs_return_6lag", "abs_return_7lag", "abs_return_8lag", "abs_return_9lag", "abs_return_10lag", 
                       "return_1lag", "return_2lag", "return_3lag", "return_4lag", "return_5lag", 
                       "return_6lag", "return_7lag", "return_8lag", "return_9lag", "return_10lag")
model_df <- total_data[complete.cases(total_data[,med_vars]), which(!str_detect(names(total_data), "text"))]

## Baseline model
model1 <- felm_DK_se(lVolume ~ mention, model_df)
model_df$lVolume_fit <- model1$fitted.values
model_df$lVolume_res <- model1$residuals
## lVolume model
model2 <- felm_DK_se(highlow ~ lVolume_fit + lVolume_res +
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     | Code + period, model_df)
stargazer(model1, model2, title = "log Volume and volatility",
          table.placement = "H", df = F)


model1 <- felm_DK_se(abs_intra_day ~ mention, model_df)
model_df$abs_intra_day_fit <- model1$fitted.values
model_df$abs_intra_day_res <- model1$residuals
## lVolume model
model2 <- felm_DK_se(highlow ~ abs_intra_day_fit + abs_intra_day_res + lVolume +  
                       abs_overnight +
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     | Code + period, model_df)
stargazer(model1, model2, title = "Intra day return and volatility",
          table.placement = "H", df = F)

model3 <- felm_DK_se(abs_intra_day ~ mention +
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     | Code + period, total_data)
model4 <- felm_DK_se(highlow ~ mention + abs_intra_day + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
model5 <- felm_DK_se(highlow ~ mention + lVolume + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
model6 <- felm_DK_se(highlow ~ mention + abs_intra_day +  lVolume + 
                       abs_overnight + 
                       VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                     |Code + period, total_data)
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F)




ttests_df <- read_csv("/Users/julianashwin/Documents/GitHub/Media_volatility/data/quartile_coefs.csv")
ttests_df %>%
  filter(var2 <= var1) %>%
  mutate(sig_level = case_when(abs(test) > 2.576 ~ "***",abs(test) > 1.96 ~ "**", abs(test) > 1.645 ~ "*", TRUE ~ "")) %>%
  mutate(test_text = str_c(sprintf(fmt = "%01.3f",test), sig_level)) %>%
  ggplot(aes(x = as.character(var1), y = as.character(var2), fill = test)) + theme_bw() + 
  facet_wrap(~depvar) +
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-2.5,2.5), space = "Lab") +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust=1)) + 
  geom_text(aes(label = test_text), color = "black", size = 2.3) + 
  labs(x = "Quartile coefficient 1", y = "Quartile coefficient 2", fill = "t statistic") # + 
  #ggtitle("Test for difference in coefficients across quartiles, with no information controls and logged volatility")
ggsave("/Users/julianashwin/Documents/GitHub/Media_volatility/data/quartile_coefs_test.pdf", 
       width = 8, height = 2.5)

  

"
Spillover effects
"

weighted_mentions_short <- read.csv(str_c(clean_dir, "/FT/matched/weighted_mention_avg.csv"))

model_df <- total_data %>%
  left_join(weighted_mentions_short, by = c("Code", "Date", "Sector"))

# Baseline
model1 <- felm(highlow ~ mention + 
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, data = model_df)
# Only cons
model2 <- felm(highlow ~ mention + cons_weighted_mention_avg +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
# Only dem
model3 <- felm(highlow ~ mention + dem_weighted_mention_avg +
               abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
# Both
model4 <- felm(highlow ~ mention +  dem_weighted_mention_avg + cons_weighted_mention_avg +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
# Both plus own sector
model5 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 own_sec_mention_avg_notme  +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
# Both, own sec mention and vol
model6 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
                 | Code + Date, 
               data = model_df)
summary(model6) 

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F) 





# Chuck in a placebo
model1 <- felm(highlow ~ mention + placebo_weighted_mention_avg + 
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, data = model_df)
summary(model1) 
model2 <- felm(highlow ~ mention + placebo_weighted_mention_avg + 
                 own_sec_mention_avg_notme + own_sec_highlow_notme +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, data = model_df)
stargazer(model1, model2, table.placement = "H", df = F) 

# Without the abs_intra_day and VI
model8 <- felm(highlow ~ mention 
               + own_sec_mention_avg_notme + own_sec_highlow_notme
               + dem_weighted_mention_avg + cons_weighted_mention_avg
               + plm::lag(highlow, 1:10) | Code + Date, 
               data = model_df)
summary(model8) 



"
Spillovers to volume and returns
"
model1 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
model2 <- felm(lVolume ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                + own_sec_mention_avg_notme + own_sec_highlow_notme +
                abs_overnight + 
                VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
              | Code + Date, 
              data = model_df)
summary(model2) 
model3 <- felm(abs_intra_day ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
model4 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme + lVolume +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
model5 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme + abs_intra_day +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)
model6 <- felm(highlow ~ mention + dem_weighted_mention_avg + cons_weighted_mention_avg +
                 + own_sec_mention_avg_notme + own_sec_highlow_notme + lVolume + abs_intra_day +
                 abs_overnight + 
                 VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                 VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                 highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                 highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                 lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                 lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                 abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                 abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                 return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                 return_6lag + return_7lag + return_8lag + return_9lag + return_10lag
               | Code + Date, 
               data = model_df)

stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F) 





"
Index level effect
"
index_data <- total_data %>%
  group_by(period, Date, weekday) %>%
  summarise(mention = mean(mention, na.rm = T), 
            IndexHighLow = mean(IndexHighLow, na.rm = T),
            highlow = mean(highlow, na.rm = T),
            Index_abs_Change = mean(Index_abs_Change, na.rm = T),
            abs_intra_day = mean(abs_intra_day, na.rm = T),
            VI_put = mean(VI_put, na.rm = T)) %>%
  ungroup() %>%
  mutate(IndexHighLow_1lag = lag(IndexHighLow, order_by = period)) %>%
  mutate(highlow_1lag = lag(highlow, n=1, order_by = period)) %>%
  mutate(highlow_2lag = lag(highlow, n=2, order_by = period)) %>%
  mutate(highlow_3lag = lag(highlow, n=3, order_by = period)) %>%
  mutate(highlow_4lag = lag(highlow, n=4, order_by = period)) %>%
  mutate(highlow_5lag = lag(highlow, n=5, order_by = period)) %>%
  filter(mention < 0.1)


ggplot(index_data) + 
  geom_line(aes(x = as.Date(Date), y = mention*100, color = "mention")) + 
  geom_line(aes(x = as.Date(Date), y = highlow, color = "highlow"))

model1 <- lm(IndexHighLow ~ mention, index_data)
model2 <- lm(IndexHighLow ~ mention + IndexHighLow_1lag, index_data) 
stargazer(model1, model2, table.placement = "H", df = F)



"
Mediation (might revisit)
"