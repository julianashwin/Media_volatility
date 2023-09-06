setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")
rm(list=ls())

library(tidyverse)
library(janitor)
library(lfe)
library(fixest)
library(lubridate)
library(beepr)
library(progress)
library(stargazer)


"
Define some functions and variables
"
felm_DK_se <- function(reg_formula, df_panel, nlags = 3){
  
  # Estimate regressions with feols and felm
  model <- feols(reg_formula, data = df_panel)
  model_felm <- felm(reg_formula, data = df_panel)
  
  stopifnot(length(model_felm$se) ==  
              length(summary(model, vcov = DK(nlags) ~ period)$coeftable[,"Std. Error"]))
  model_felm$se <- summary(model, vcov = DK(nlags) ~ period)$coeftable[,"Std. Error"]
  model_felm$tval <- summary(model, vcov = DK(nlags) ~ period)$coeftable[,"t value"]
  model_felm$pval <- summary(model, vcov = DK(nlags) ~ period)$coeftable[,"Pr(>|t|)"]
  return(model_felm)
}


# Codes where the intra-day data doesn;t seem to match Eikon at all
bad_codes <- c("CNE.L", "COLT.F", "CRH.L", "CS.L", "CWC.L", "DXNS.L", "EMG.L", "INVP.L", "LLOY.L",
               "LMI.L", "MRON.L", "PO_p.L", "RSA.L", "VOD.L", "XTA.L")
bad_codes %in% unique(panel_df$code_clean)



"
Import data
"
panel_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/JFM_rev_data.rds") %>%
  mutate(code_clean = str_replace_all(Code, "\\^.*", ""))

company_codes <- read_csv("/Users/julianashwin/Documents/GitHub/Media_volatility/data/company_codes.csv") %>%
  mutate(code_clean = str_replace_all(Code, "\\^.*", ""))

if (FALSE){
  ## Find all files 
  raw_dir <- "/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_data/"
  files <- dir(raw_dir)
  # Initialise tibble to store all of the data
  intraday_df <- tibble()
  # Loop through files
  for (file in files){
    year_df <- read_csv(str_c(raw_dir, file)) %>%
      rename(code_clean = `#RIC`, DateTime = `Date-Time`, 
             NumTrades = `No. Trades`, NumBids = `No. Bids`,  NumAsks = `No. Asks`, 
             OpenBid = `Open Bid`, HighBid = `High Bid`, LowBid = `Low Bid`, CloseBid = `Close Bid`, 
             OpenAsk = `Open Ask`, HighAsk = `High Ask`, LowAsk = `Low Ask`, CloseAsk = `Close Ask`) %>%
      mutate(Date = as.Date(DateTime)) %>%
      select(code_clean, Date, DateTime, Open, Last, High, Low, Volume, NumTrades, NumBids, NumAsks,
             OpenBid, HighBid, LowBid, CloseBid, OpenAsk, HighAsk, LowAsk, CloseAsk)
    
    year_codes <- unique(year_df$code_clean)
    print(str_c("Unmatched codes for ", file, ": ", paste(year_codes[which(!(year_codes %in% company_codes$code_clean))], collapse = ", ")))
    
    intraday_df <- rbind(intraday_df, year_df)
  }
  intraday_df %>%
    saveRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_5min_data.rds")
}
intraday_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_5min_data.rds")

"
Identify some stock split adjustments, taking median over 9 day window
"
split_adj_smooth <- intraday_df %>%
  rename(Open_5min = Open, Close_5min = Last, High_5min = High, Low_5min = Low, Volume_5min = Volume) %>%
  drop_na(Open_5min, Close_5min, High_5min, Low_5min) %>%
  inner_join(select(panel_df, Date, code_clean, Open, Close, High, Low)) %>%
  select(code_clean, Date, DateTime, Open_5min, Open, Close_5min, Close, High_5min, High, Low_5min, Low) %>% 
  mutate(is_8am = as.numeric(hour(DateTime) == 8 & minute(DateTime) == 0)) %>%
  relocate(is_8am, .after = DateTime) %>%
  arrange(code_clean, Date, -is_8am, DateTime) %>%
  group_by(code_clean, Date) %>%
  mutate(High_max = max(High_5min, na.rm = T), Low_min = min(Low_5min, na.rm = T)) %>%
  mutate(Open_8am = first(Open_5min),
         Open_adj = Open_8am/Open,
         High_adj = High_max/High,
         Low_adj = Low_min/Low) %>%
  group_by(code_clean, Date) %>%
  summarise(Open = first(Open), Open_adj = first(Open_adj), Open_median = median(Open_5min),
            High = first(High), High_adj = first(High_adj), High_median = median(High_5min),
            Low = first(Low), Low_adj = first(Low_adj), Low_median = median(Low_5min),
            Close = first(Close), Close_median = median(Close_5min)) %>%
  arrange(code_clean, Date) %>%
  group_by(code_clean) %>%
  mutate(Open_adj_1lag = lag(Open_adj, n = 1, order_by = Date), Open_adj_2lag = lag(Open_adj, n = 2, order_by = Date),
         Open_adj_3lag = lag(Open_adj, n = 3, order_by = Date), Open_adj_4lag = lag(Open_adj, n = 4, order_by = Date),
         Open_adj_1lead = lead(Open_adj, n = 1, order_by = Date), Open_adj_2lead = lead(Open_adj, n = 2, order_by = Date),
         Open_adj_3lead = lead(Open_adj, n = 3, order_by = Date), Open_adj_4lead = lead(Open_adj, n = 4, order_by = Date)) %>% 
  pivot_longer(cols = c(Open_adj_4lag, Open_adj_3lag, Open_adj_2lag, Open_adj_1lag, Open_adj, 
                        Open_adj_1lead, Open_adj_2lead, Open_adj_3lead, Open_adj_4lead), 
               names_to = "Horizon", values_to = "Open_adj_factors") %>%
  arrange(code_clean, Date, Horizon) %>%
  group_by(code_clean, Date, Open, High, Low, Close, High_adj, Low_adj) %>%
  summarise(Open_adj_smooth = median(Open_adj_factors, na.rm = T), 
            Open_adj = first(Open_adj_factors)) %>%
  ungroup() 

split_adj_smooth %>%
  ggplot() + theme_bw() + 
  geom_density(aes(x = Open_adj_smooth))

split_adj_smooth %>%
  filter(code_clean %in% bad_codes) %>%
  ggplot() + theme_bw() + facet_wrap(~code_clean) + 
  geom_hline(aes(yintercept = 1), linetype = "dashed") + 
  geom_line(aes(x = Date, y = Open_adj, color = "Open")) +
  geom_line(aes(x = Date, y = Open_adj_smooth, color = "Open (smooth)")) +
  coord_cartesian(ylim = c(0,10)) 


"
Clean intraday data to be consistent with daily
"
intraday_df_clean <- intraday_df %>%
  rename(Open_5min = Open, Close_5min = Last, High_5min = High, Low_5min = Low, Volume_5min = Volume) %>%
  # Merge and adjust
  left_join(select(split_adj_smooth, code_clean, Date, Open_adj_smooth, Open, Close, High, Low)) %>%
  relocate(Open_adj_smooth, .after = DateTime) %>%
  mutate(Open_5min_clean = Open_5min/Open_adj_smooth) %>%
  relocate(Open, Open_5min_clean, .before = Open_5min) %>%
  mutate(Close_5min_clean = Close_5min/Open_adj_smooth) %>%
  relocate(Close, Close_5min_clean, .before = Close_5min) %>%
  mutate(High_5min_clean = High_5min/Open_adj_smooth) %>%
  relocate(High, High_5min_clean, .before = High_5min) %>%
  mutate(Low_5min_clean = Low_5min/Open_adj_smooth) %>%
  relocate(Low, Low_5min_clean, .before = Low_5min) %>%
  # Keep only trades in business hours
  mutate(hour = hour(DateTime), minute = minute(DateTime)) %>%
  filter(hour >= 8, hour < 17, hour != 16 | minute <= 35) %>%
  group_by(code_clean, Date) %>%
  # If there were no trades in a 5 min window, then use previous close price
  fill(Close_5min_clean, .direction = "down") %>%
  mutate(Open_5min_clean = case_when(is.na(Open_5min_clean) ~ Close_5min_clean, TRUE ~ Open_5min_clean),
         High_5min_clean = case_when(is.na(High_5min_clean) ~ Close_5min_clean, TRUE ~ High_5min_clean),
         Low_5min_clean = case_when(is.na(Low_5min_clean) ~ Close_5min_clean, TRUE ~ Low_5min_clean)) %>%
  # Remove outlier/fat finger trades that contradict the daily data
  # If prices are above the daily High and Low then adjust
  mutate(Open_5min_clean = case_when(Open_5min_clean > High ~ High, Open_5min_clean < Low ~ Low, TRUE ~ Open_5min_clean),
         Close_5min_clean = case_when(Close_5min_clean > High ~ High, Close_5min_clean < Low ~ Low, TRUE ~ Close_5min_clean),
         High_5min_clean = case_when(High_5min_clean > High ~ High, High_5min_clean < Low ~ Low, TRUE ~ High_5min_clean),
         Low_5min_clean = case_when(Low_5min_clean > High ~ High, Low_5min_clean < Low ~ Low, TRUE ~ Low_5min_clean))
beep(sound = 9)



## Plot an example
ANTO_df <- intraday_df_clean %>%
  filter(code_clean == "ANTO.L")
ANTO_df %>%
  filter(Date >= "2006-01-01" & Date < "2006-03-01" ) %>%
  ggplot() + theme_bw() +
  geom_ribbon(aes(x = DateTime, ymax = High, ymin = Low), color = "grey", fill = "grey", alpha = 0.5) +
  geom_line(aes(x = DateTime, y = Close)) + 
  geom_point(aes(x = DateTime, y = Open_5min_clean), color = "red", size = 0.01)

cor.test(intraday_df_clean$Open, intraday_df_clean$Open_5min_clean)
cor.test(intraday_df_clean$Open, intraday_df_clean$Close)
cor.test(intraday_df_clean$Open_5min_clean, intraday_df_clean$Close_5min_clean)


"
Aggregate to 5min, 30min and 60min intervals
"
intraday_5min_df <- intraday_df_clean %>%
  relocate(hour, minute, .after = DateTime) %>%
  select(-c(Open_5min, Close_5min, High_5min, Low_5min)) %>%
  rename(Open_5min = Open_5min_clean, Close_5min = Close_5min_clean, High_5min = High_5min_clean, Low_5min = Low_5min_clean) %>%
  rename(NumTrades_5min = NumTrades, NumBids_5min = NumBids, NumAsks_5min = NumAsks) %>%
  group_by(code_clean, Date) %>%  
  select(code_clean, Date, hour, minute, Open_5min, Close_5min, High_5min, Low_5min, NumTrades_5min, NumBids_5min, NumAsks_5min) %>%
  mutate(Close_5min_lag  = lag(Close_5min)) %>%
  mutate(return_5min_oc = (Close_5min - Open_5min)/Open_5min, return_5min_cc = (Close_5min - Close_5min_lag)/Close_5min_lag,
         log_return_5min_oc = log(1+Close_5min) - log(1+Open_5min), log_return_5min_cc = log(1+Close_5min) - log(1+Close_5min_lag)) %>%
  ungroup()

intraday_30min_df <- intraday_df_clean %>%
  select(-c(Open_5min, Close_5min, High_5min, Low_5min)) %>%
  relocate(hour, minute, .after = DateTime) %>%
  mutate(minute = case_when(minute %in% c(0,5,10,15,20,25) ~ 0, minute %in% c(30,35,40,45,50,55) ~ 30)) %>%
  arrange(code_clean, Date, hour, minute) %>%
  group_by(code_clean, Date, hour, minute) %>%
  summarise(Open = first(Open), Close = first(Close), High = first(High), Low = first(Low), 
            Open_30min = first(Open_5min_clean), Close_30min = last(Close_5min_clean), 
            High_30min = max(High_5min_clean), Low_30min = min(Low_5min_clean), 
            NumTrades_30min = sum(NumTrades, na.rm = T), NumBids_30min = sum(NumBids, na.rm = T), NumAsks_30min = sum(NumAsks, na.rm = T)) %>%
  group_by(code_clean, Date) %>%  
  mutate(Close_30min_lag  = lag(Close_30min)) %>%
  mutate(return_30min_oc = (Close_30min - Open_30min)/Open_30min, return_30min_cc = (Close_30min - Close_30min_lag)/Close_30min_lag,
         log_return_30min_oc = log(1+Close_30min) - log(1+Open_30min), log_return_30min_cc = log(1+Close_30min) - log(1+Close_30min_lag)) %>%
  ungroup()

intraday_60min_df <- intraday_df_clean %>%
  select(-c(Open_5min, Close_5min, High_5min, Low_5min)) %>%
  relocate(hour, minute, .after = DateTime) %>%
  arrange(code_clean, Date, hour, minute) %>%
  group_by(code_clean, Date, hour) %>%
  summarise(Open = first(Open), Close = first(Close), High = first(High), Low = first(Low), 
            Open_60min = first(Open_5min_clean), Close_60min = last(Close_5min_clean), 
            High_60min = max(High_5min_clean), Low_60min = min(Low_5min_clean), 
            NumTrades_60min = sum(NumTrades, na.rm = T), NumBids_60min = sum(NumBids, na.rm = T), NumAsks_60min = sum(NumAsks, na.rm = T)) %>%
  group_by(code_clean, Date) %>%  
  mutate(Close_60min_lag  = lag(Close_60min)) %>%
  mutate(return_60min_oc = (Close_60min - Open_60min)/Open_60min, return_60min_cc = (Close_60min - Close_60min_lag)/Close_60min_lag,
         log_return_60min_oc = log(1+Close_60min) - log(1+Open_60min), log_return_60min_cc = log(1+Close_60min) - log(1+Close_60min_lag)) %>%
  ungroup()

intraday_5min_df %>%
  saveRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_5min_data_clean.rds")
intraday_30min_df %>%
  saveRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_30min_data_clean.rds")
intraday_60min_df %>%
  saveRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/intraday_60min_data_clean.rds")
beep(sound = 9)


intraday_5min_df %>% 
  filter(code_clean == "AAL.L" & Date == "2006-01-03")
intraday_30min_df %>% 
  filter(code_clean == "AAL.L" & Date == "2006-01-03")
intraday_60min_df %>% 
  filter(code_clean == "AAL.L" & Date == "2006-01-03")


"
Get daily aggregates for the different intervals
"
intraday_5min_agg <- intraday_5min_df %>%
  group_by(code_clean, Date) %>%
  summarise(Open_5min = first(Open_5min), Close_5min = last(Close_5min), High_5min = max(High_5min), Low_5min = min(Low_5min),
            NumTrades_5min = sum(NumTrades_5min, na.rm = T), NumBids_5min = sum(NumBids_5min, na.rm = T), NumAsks_5min = sum(NumAsks_5min, na.rm = T),
            mean_return_5min_oc = mean(return_5min_oc), mean_return_5min_cc = mean(return_5min_cc, na.rm = T), 
            mean_logreturn_5min_oc = mean(log_return_5min_oc), mean_logreturn_5min_cc = mean(log_return_5min_cc, na.rm = T), 
            mean_absreturn_5min_oc = mean(abs(return_5min_oc)), mean_absreturn_5min_cc = mean(abs(return_5min_cc), na.rm = T), 
            mean_abslogreturn_5min_oc = mean(abs(log_return_5min_oc)), mean_abslogreturn_5min_cc = mean(abs(log_return_5min_cc), na.rm = T), 
            mean_sqreturn_5min_oc = mean(return_5min_oc^2), mean_sqreturn_5min_cc = mean(return_5min_cc^2, na.rm = T), 
            mean_sqlogreturn_5min_oc = mean(log_return_5min_oc^2), mean_sqlogreturn_5min_cc = mean(log_return_5min_cc^2, na.rm = T))

intraday_30min_agg <- intraday_30min_df %>%
  group_by(code_clean, Date) %>%
  summarise(Open_30min = first(Open_30min), Close_30min = last(Close_30min), High_30min = max(High_30min), Low_30min = min(Low_30min),
            NumTrades_30min = sum(NumTrades_30min, na.rm = T), NumBids_30min = sum(NumBids_30min, na.rm = T), NumAsks_30min = sum(NumAsks_30min, na.rm = T),
            mean_return_30min_oc = mean(return_30min_oc), mean_return_30min_cc = mean(return_30min_cc, na.rm = T), 
            mean_logreturn_30min_oc = mean(log_return_30min_oc), mean_logreturn_30min_cc = mean(log_return_30min_cc, na.rm = T), 
            mean_absreturn_30min_oc = mean(abs(return_30min_oc)), mean_absreturn_30min_cc = mean(abs(return_30min_cc), na.rm = T), 
            mean_abslogreturn_30min_oc = mean(abs(log_return_30min_oc)), mean_abslogreturn_30min_cc = mean(abs(log_return_30min_cc), na.rm = T), 
            mean_sqreturn_30min_oc = mean(return_30min_oc^2), mean_sqreturn_30min_cc = mean(return_30min_cc^2, na.rm = T), 
            mean_sqlogreturn_30min_oc = mean(log_return_30min_oc^2), mean_sqlogreturn_30min_cc = mean(log_return_30min_cc^2, na.rm = T))

intraday_60min_agg <- intraday_60min_df %>%
  group_by(code_clean, Date) %>%
  summarise(Open_60min = first(Open_60min), Close_60min = last(Close_60min), High_60min = max(High_60min), Low_60min = min(Low_60min),
            NumTrades_60min = sum(NumTrades_60min, na.rm = T), NumBids_60min = sum(NumBids_60min, na.rm = T), NumAsks_60min = sum(NumAsks_60min, na.rm = T),
            mean_return_60min_oc = mean(return_60min_oc), mean_return_60min_cc = mean(return_60min_cc, na.rm = T), 
            mean_logreturn_60min_oc = mean(log_return_60min_oc), mean_logreturn_60min_cc = mean(log_return_60min_cc, na.rm = T), 
            mean_absreturn_60min_oc = mean(abs(return_60min_oc)), mean_absreturn_60min_cc = mean(abs(return_60min_cc), na.rm = T), 
            mean_abslogreturn_60min_oc = mean(abs(log_return_60min_oc)), mean_abslogreturn_60min_cc = mean(abs(log_return_60min_cc), na.rm = T), 
            mean_sqreturn_60min_oc = mean(return_60min_oc^2), mean_sqreturn_60min_cc = mean(return_60min_cc^2, na.rm = T), 
            mean_sqlogreturn_60min_oc = mean(log_return_60min_oc^2), mean_sqlogreturn_60min_cc = mean(log_return_60min_cc^2, na.rm = T))
beep(sound = 9)

feols(Open_5min ~ Close_5min | code_clean + Date, intraday_5min_df)
feols(Open_30min ~ Close_30min | code_clean + Date, intraday_30min_df)
feols(Open_60min ~ Close_60min | code_clean + Date, intraday_60min_df)
feols(Open ~ Close | code_clean + Date, panel_df)

panel_intraday_df <- panel_df %>%
  inner_join(intraday_5min_agg) %>%
  inner_join(intraday_30min_agg) %>%
  inner_join(intraday_60min_agg) %>%
  mutate(highlow_new = 100*abs(log(High) - log(Low)),
         highlow_5min = 100*abs(log(High_5min) - log(Low_5min)),
         highlow_30min = 100*abs(log(High_30min) - log(Low_30min)),
         highlow_60min = 100*abs(log(High_60min) - log(Low_60min)))

panel_intraday_df %>%
  saveRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/panel_intraday_df.rds")
beep(sound = 9)
#panel_intraday_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/panel_intraday_df.rds")



cor.test(panel_intraday_df$mean_sqlogreturn_5min_oc, panel_intraday_df$highlow)

"
Robustness check regressions
"
# Controls
controls_base <- "abs_overnight + VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag +
abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
return_6lag + return_7lag + return_8lag + return_9lag + return_10lag"
controls_ownlag <- "abs_overnight + 
VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
volatility_1lag + volatility_2lag + volatility_3lag + volatility_4lag + volatility_5lag +
volatility_6lag + volatility_7lag + volatility_8lag + volatility_9lag + volatility_10lag +
lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
return_6lag + return_7lag + return_8lag + return_9lag + return_10lag"
controls_bothlag <- "abs_overnight + 
VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
volatility_1lag + volatility_2lag + volatility_3lag + volatility_4lag + volatility_5lag +
volatility_6lag + volatility_7lag + volatility_8lag + volatility_9lag + volatility_10lag +
highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
return_6lag + return_7lag + return_8lag + return_9lag + return_10lag"


summary(felm_DK_se(as.formula(str_c("highlow ~ mention + ", controls_base, "| Code + period")), panel_intraday_df))
summary(felm_DK_se(as.formula(str_c("highlow ~ mention + ", controls_5lags, "| Code + period")), panel_intraday_df))
summary(felm_DK_se(as.formula(str_c("highlow ~ mention + ", controls_noVI, "| Code + period")), panel_intraday_df))
summary(felm_DK_se(as.formula(str_c("mean_abslogreturn_5min_oc ~ mention + ", controls_base, " | Code + period")), panel_intraday_df))

intraday_coefs <- crossing(window = c("5min", "30min", "60min"), return_type = c("cc", "oc"), 
                           abs_sq = c("abs", "sq"), log = c("log", ""), VI = c("VI", "noVI"), lags = c(1,5,10),
                           vol_lags = c("highlow", "own", "both"),
                           coef = NA, se = NA, tstat = NA, pval = NA, N = NA, fm = NA)

pb <- progress_bar$new(total = nrow(intraday_coefs), format = " estimating [:bar], :eta remaining")
for (ii in 1:nrow(intraday_coefs)){
  vol_measure <- str_c("mean_", intraday_coefs$abs_sq[ii], intraday_coefs$log[ii], "return_", intraday_coefs$window[ii], "_", intraday_coefs$return_type[ii])
  panel_intraday_df$volatility <- unlist(panel_intraday_df[,vol_measure])
  
  panel_intraday_df <- panel_intraday_df %>% 
    group_by(Code) %>%
    mutate(volatility_1lag = lag(volatility, n = 1, order_by = period),
           volatility_2lag = lag(volatility, n = 2, order_by = period),
           volatility_3lag = lag(volatility, n = 3, order_by = period),
           volatility_4lag = lag(volatility, n = 4, order_by = period),
           volatility_5lag = lag(volatility, n = 5, order_by = period),
           volatility_6lag = lag(volatility, n = 6, order_by = period),
           volatility_7lag = lag(volatility, n = 7, order_by = period),
           volatility_8lag = lag(volatility, n = 8, order_by = period),
           volatility_9lag = lag(volatility, n = 9, order_by = period),
           volatility_10lag = lag(volatility, n = 10, order_by = period)) %>%
    relocate(period, volatility, volatility_10lag, .after = Date)
  
  # Choose which type of volatility lags to include
  if (intraday_coefs$vol_lags[ii] == "highlow"){
    controls <- str_squish(controls_base)
  } else if (intraday_coefs$vol_lags[ii] == "both"){
    controls <- str_squish(controls_bothlag)
  } else if (intraday_coefs$vol_lags[ii] == "own"){
    controls <- str_squish(controls_ownlag)
  }
  # How many lags
  if (intraday_coefs$lags[ii] == 1){
    controls <- str_remove_all(str_remove_all(controls, "\\+ [aA-zZ]+_[2-9]lag"), "\\+  [aA-zZ]+_10lag")
    controls <- str_remove_all(str_remove_all(controls, "\\+ [aA-zZ]+_[aA-zZ]+_[2-9]lag"), "\\+ [aA-zZ]+_10lag")
  } else if (intraday_coefs$lags[ii] == 5){
    controls <- str_remove_all(str_remove_all(controls, "\\+ [aA-zZ]+_[5-9]lag"), "\\+ [aA-zZ]+_10lag")
    controls <- str_remove_all(str_remove_all(controls, "\\+ [aA-zZ]+_[aA-zZ]+_[5-9]lag"), "\\+ [aA-zZ]+_10lag")
  }
  # Include implied volatility?
  if (intraday_coefs$VI[ii] == "noVI"){
    controls <- str_remove_all(controls, " VI_[a-z]+_[0-9]lag \\+")
  }
  # Estimate model
  fm <- as.formula(str_c(vol_measure, " ~ mention + ", controls, "| Code + period"))
  model <- felm_DK_se(fm, panel_intraday_df)
  model_summary <- summary(model)$coefficients
  # Populate coefs
  intraday_coefs$coef[ii] <- model_summary["mention", "Estimate"]
  intraday_coefs$se[ii] <- model_summary["mention", "Std. Error"]
  intraday_coefs$tstat[ii] <- model_summary["mention", "t value"]
  intraday_coefs$pval[ii] <- model_summary["mention", "Pr(>|t|)"]
  intraday_coefs$N[ii] <- model$N
  intraday_coefs$fm[ii] <- paste(as.character(model$formula), collapse = " ")
  pb$tick()
}

intraday_coefs_clean <- intraday_coefs %>%
  #filter(return_type == "oc" & log %in% c("log", "")) %>% # & vol_lags %in% c("own", "highlow", "both"), lags == 10) %>%
  mutate(return_measure = str_c(log,"return", "_", return_type)) %>%
  mutate(adjustment = case_when(window == "5min" ~ 104, window == "30min" ~ 18, window == "60min" ~ 9),
         coef_adj = 100*coef*adjustment,
         se_adj = 100*se*adjustment) %>% 
  mutate(coef_adj = case_when(abs_sq == "sq" ~ sign(coef_adj)*sqrt(abs(coef_adj)), TRUE ~ coef_adj),
         se_adj = case_when(abs_sq == "sq" ~ sign(se_adj)*sqrt(abs(se_adj)), TRUE ~ se_adj)) %>%
  select(-adjustment)


intraday_coefs_clean %>%
  filter(return_measure == "logreturn_oc", window == "5min", lags == 10, VI == "VI")

intraday_coefs_clean %>%
  filter(VI == "VI", vol_lags %in% c("own", "both", "highlow"), lags == 10) %>%
  ggplot() + theme_bw() + facet_wrap(~abs_sq + window) + 
  geom_point(aes(x = tstat, y = return_measure, color = interaction(VI, vol_lags))) + 
  geom_vline(aes(xintercept = 1.96), linetype = "dashed")

intraday_coefs_clean %>%
  mutate(sig_level = case_when(pval <= 0.01 ~ "1%",pval <= 0.05 ~ "5%",pval <= 0.1 ~ "10%",TRUE ~ ">10%")) %>%
  filter(VI == "VI", vol_lags %in% c("own", "both", "highlow"), lags == 10) %>%
  ggplot(aes(y = interaction(return_measure, VI, vol_lags), color = sig_level)) + 
  theme_bw() + facet_wrap(~abs_sq + window, scales = "free") + 
  geom_point(aes(x = coef)) + 
  geom_errorbar(width=.3, aes(xmin=coef-1.96*se, xmax=coef+1.96*se)) + 
  geom_vline(aes(xintercept = 0), linetype = "dashed")

intraday_coefs_clean %>%
  filter(VI == "VI", vol_lags %in% c("own", "both", "highlow"), lags == 10,
         return_type == "oc", abs_sq == "abs", window == "5min")


panel_intraday_df <- panel_intraday_df %>%
  mutate(absreturn_5min = 104*100*mean_abslogreturn_5min_oc,
         absreturn_30min = 18*100*mean_abslogreturn_30min_oc,
         absreturn_60min = 9*100*mean_abslogreturn_60min_oc,
         sqreturn_5min = 104*100*mean_sqlogreturn_5min_oc,
         sqreturn_30min = 18*100*mean_sqlogreturn_30min_oc,
         sqreturn_60min = 9*100*mean_sqlogreturn_60min_oc) %>%
  group_by(Code) %>%
  mutate(absreturn_5min_1lag = lag(absreturn_5min, n = , order_by = period),
         absreturn_30min_1lag = lag(absreturn_30min, n = 1, order_by = period),
         absreturn_60min_1lag = lag(absreturn_60min, n = 1, order_by = period),
         sqreturn_5min_1lag = lag(sqreturn_5min, n = 1, order_by = period),
         sqreturn_30min_1lag = lag(sqreturn_30min, n = 1, order_by = period),
         sqreturn_60min_1lag = lag(sqreturn_60min, n = 1, order_by = period)) 

model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ",controls_base, " | Code + period")), filter(panel_intraday_df, !is.na(absreturn_5min), !is.na(VI_put)))
model2 <- felm_DK_se(as.formula(str_c("absreturn_5min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
model3 <- felm_DK_se(as.formula(str_c("absreturn_30min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
model4 <- felm_DK_se(as.formula(str_c("absreturn_60min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
model5 <- felm_DK_se(as.formula(str_c("sqreturn_5min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
model6 <- felm_DK_se(as.formula(str_c("sqreturn_30min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
model7 <- felm_DK_se(as.formula(str_c("sqreturn_60min ~ mention + ",controls_base, " | Code + period")), panel_intraday_df)
models <- list(model1, model2, model3, model4, model5, model6, model7)
stargazer(models,
          table.placement = "H", df = F, 
          title = "Article effect is robust to using alternative volatility measures")


model1 <- felm(highlow ~ mention + highlow_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, filter(panel_intraday_df, !is.na(sqreturn_5min) & !is.na(sqreturn_5min_1lag)))
model2 <- felm(absreturn_5min ~ mention + absreturn_30min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight| Code + period, panel_intraday_df)
model3 <- felm(absreturn_30min ~ mention + absreturn_30min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, panel_intraday_df)
model4 <- felm(absreturn_60min ~ mention + absreturn_60min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, panel_intraday_df)
model5 <- felm(sqreturn_5min ~ mention + sqreturn_5min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, panel_intraday_df)
model6 <- felm(sqreturn_30min ~ mention + sqreturn_30min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, panel_intraday_df)
model7 <- felm(sqreturn_60min ~ mention + sqreturn_60min_1lag + VI_put_1lag + VI_call_1lag + abs_overnight | Code + period, panel_intraday_df)
models <- list(model1, model2, model3, model4, model5, model6, model7)
stargazer(model1, model2, model3, model4, model5, model6, model7,
          table.placement = "H", df = F, 
          title = "Intraday volatility measure robustness (fewer controls)")





intraday_coefs_clean %>%
  filter(VI == "VI", vol_lags == "own", return_type == "oc", lags == 10)

mean_sqlogreturn_5min_oc

model <- felm_DK_se(as.formula(str_c("mean_abslogreturn_5min_oc ~ mention + mean_abslogreturn_5min_oc_1lag + ",controls_base, " | Code + period")), panel_intraday_df)
summary(model)

model <- felm_DK_se(as.formula(str_c("mean_abslogreturn_5min_oc ~ mention + ", controls_base, " + 
                                     mean_abslogreturn_5min_oc_2lag + mean_abslogreturn_5min_oc_2lag + 
                                     mean_abslogreturn_5min_oc_3lag + mean_abslogreturn_5min_oc_4lag + 
                                     mean_abslogreturn_5min_oc_5lag + mean_abslogreturn_5min_oc_6lag + 
                                     mean_abslogreturn_5min_oc_7lag + mean_abslogreturn_5min_oc_8lag + 
                                     mean_abslogreturn_5min_oc_9lag + mean_abslogreturn_5min_oc_10lag | Code + period")), panel_intraday_df)
summary(model)

"
Look at effect throughout the day
"

intraday_30min_df1 <- intraday_30min_df %>%
  left_join(panel_df) %>%
  mutate(time = str_c(hour,":",minute),
         absreturn_30min_oc = abs(return_30min_oc),
         absreturn_30min_cc = abs(return_30min_cc),
         abslogreturn_30min_oc = abs(log_return_30min_oc),
         abslogreturn_30min_cc = abs(log_return_30min_cc))
intraday_30min_df1 <- intraday_30min_df1 %>%
  mutate(sqlogreturn_30min_oc = (abslogreturn_30min_oc)^2)


intraday_60min_df1 <- intraday_60min_df %>%
  left_join(panel_df) %>%
  mutate(time = str_c(hour,":00"),
         absreturn_60min_oc = abs(return_60min_oc),
         absreturn_60min_cc = abs(return_60min_cc),
         abslogreturn_60min_oc = abs(log_return_60min_oc),
         abslogreturn_60min_cc = abs(log_return_60min_cc),
         sqlogreturn_60min_oc = (log_return_60min_oc)^2,
         sqlogreturn_60min_cc = (log_return_60min_cc)^2)


model_30min <- felm_DK_se(as.formula(str_c("abslogreturn_30min_oc ~ time:mention + time + ", controls_base, "| Code + period")), intraday_30min_df1)
model_30minsq <- felm_DK_se(as.formula(str_c("sqlogreturn_30min_oc ~ time:mention + time + ", controls_base, "| Code + period")), intraday_30min_df1)
model_60min <- felm_DK_se(as.formula(str_c("abslogreturn_60min_oc ~ time:mention + time +", controls_base, "| Code + period")), intraday_60min_df1)
model_60minsq <- felm_DK_se(as.formula(str_c("sqlogreturn_60min_oc ~ time:mention + time +", controls_base, "| Code + period")), intraday_60min_df1)



hourly_coefs <- tibble()
# 30minsq
temp_coefs <- data.frame(summary(model_30minsq)$coefficients)
temp_coefs$regressor <- rownames(temp_coefs) 
temp_coefs$model <- "sqlogreturn_30min_oc"
hourly_coefs <- rbind(hourly_coefs, temp_coefs)
# 30min
temp_coefs <- data.frame(summary(model_30min)$coefficients)
temp_coefs$regressor <- rownames(temp_coefs) 
temp_coefs$model <- "abslogreturn_30min_oc"
hourly_coefs <- rbind(hourly_coefs, temp_coefs)
# 60minsq
temp_coefs <- data.frame(summary(model_60minsq)$coefficients)
temp_coefs$regressor <- rownames(temp_coefs) 
temp_coefs$model <- "sqlogreturn_60min_oc"
hourly_coefs <- rbind(hourly_coefs, temp_coefs)
# 60min
temp_coefs <- data.frame(summary(model_60min)$coefficients)
temp_coefs$regressor <- rownames(temp_coefs) 
temp_coefs$model <- "abslogreturn_60min_oc"
hourly_coefs <- rbind(hourly_coefs, temp_coefs)

hourly_coefs <- hourly_coefs %>%
  filter(str_detect(regressor, "time")) %>%
  mutate(time = str_extract(regressor, "[0-9]+:[0-9]+"),
         type = case_when(str_detect(regressor, "mention") ~ "Article effect", 
                          str_detect(model, "30min") ~ "Half hour fixed effects", 
                          TRUE ~ "Hour fixed effect"),
         coef = Estimate, se = `Std..Error`) %>%
  mutate(time = str_replace_all(time, ":0$", ":00")) %>%
  filter(str_detect(model, "60min") | time != "16:30") %>%
  # Adjust for fact that last hour is shorter
  mutate(coef = case_when(str_detect(time, "16") & str_detect(model, "60min") ~ coef*2, TRUE ~ coef),
         se = case_when(str_detect(time, "16") & str_detect(model, "60min") ~ se*2, TRUE ~ se)) %>%
  mutate(time = factor(time, ordered = T, levels = c("8:00", "8:30", "9:00", "9:30", "10:00", "10:30", "11:00", "11:30", 
                                                     "12:00", "12:30", "13:00", "13:30", "14:00", "14:30", "15:00", "15:30", "16:00", "16:30")))

## 30min plot
hourly_coefs %>%
  filter(model == "abslogreturn_30min_oc") %>%
  ggplot(aes(x = time)) + theme_bw() + facet_wrap(~type) + 
  geom_ribbon(aes(ymax = coef + 1.96*abs(se), ymin = coef - 1.96*abs(se), group = 1), alpha = 0.2) +
  geom_line(aes( y = coef, group = 1)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coefficient (30 min absolute return)", x = "Time on day of article")
ggsave("figures/effect_timeofday_30min.pdf", width = 6, height = 3)
## 30min squared plot
hourly_coefs %>%
  filter(model == "sqlogreturn_30min_oc") %>%
  ggplot(aes(x = time)) + theme_bw() + facet_wrap(~type) + 
  geom_ribbon(aes(ymax = coef + 1.96*abs(se), ymin = coef - 1.96*abs(se), group = 1), alpha = 0.2) +
  geom_line(aes( y = coef, group = 1)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coefficient (30 min squared return)", x = "Time on day of article")
ggsave("figures/effect_timeofday_30minsq.pdf", width = 6, height = 3)
## 60 min plot
hourly_coefs %>%
  filter(model == "abslogreturn_60min_oc") %>%
  ggplot(aes(x = time)) + theme_bw() + facet_wrap(~type) + 
  geom_ribbon(aes(ymax = coef + 1.96*abs(se), ymin = coef - 1.96*abs(se), group = 1), alpha = 0.2) +
  geom_line(aes( y = coef, group = 1)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coefficient (60 min absolute return)", x = "Time on day of article")
ggsave("figures/effect_timeofday_60min.pdf", width = 6, height = 3)
## 60min squared plot
hourly_coefs %>%
  filter(model == "sqlogreturn_60min_oc") %>%
  ggplot(aes(x = time)) + theme_bw() + facet_wrap(~type) + 
  geom_ribbon(aes(ymax = coef + 1.96*abs(se), ymin = coef - 1.96*abs(se), group = 1), alpha = 0.2) +
  geom_line(aes( y = coef, group = 1)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(y = "Coefficient (60 min squared return)", x = "Time on day of article")
ggsave("figures/effect_timeofday_60minsq.pdf", width = 6, height = 3)


