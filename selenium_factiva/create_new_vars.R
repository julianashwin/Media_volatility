setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")

library(tidyverse)
library(janitor)
library(lfe)
library(fixest)




felm_DK_se <- function(reg_formula, df_panel, nlags = 3){
  
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



## Get more article details


BTR_data <- read_csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/BTR_FT_data.csv")

BTR_data %>%
  filter(!is.na(VI_put)) %>%
  group_by(Code, Company) %>%
  summarise(Date_min = min(Date), Date_max = max(Date)) %>%
  write_csv("tickers_intraday.csv")


## Get data to use for identifying online articles
headline_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/headline_companyarticles.csv"))
NER_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/NER_companyarticles.csv"))
#both_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/both_companyarticles_collapsed.csv"))

matched_data <- inner_join(NER_data, select(headline_data, article_id, Date, Code, headline)) %>%
  arrange(Code, Date) %>%
  inner_join(select(BTR_data,Code,Date)) %>%
  mutate(date_num = Date)

matched_data %>%
  filter(!is.na(orgs_mentioned)) %>%
  mutate(Date_lag = as.Date(Date) - 1) %>%
  select(article_id, Code, Date, Date_lag, Company, headline, orgs_mentioned) %>%
  mutate(key_phrase = str_squish(str_replace_all(orgs_mentioned, "[^[:alpha:]]", " "))) %>%
  write.csv("selenium_factiva/article_info.csv", row.names = F)

matched_data %>%
  select(Code, Date) %>%
  unique() 

# Import the data that's since been matched to the online articles
online_article_info <- read_csv("selenium_factiva/article_info_matched.csv")

online_articles <- online_article_info %>%
  mutate(online_found = as.numeric(online_found == "Yes"),
         online_today = replace_na(online_today, 0), online_yday = replace_na(online_yday, 0),
         closeness_online_today = replace_na(closeness_online_today, 0), closeness_online_yday = replace_na(closeness_online_yday, 0)) %>%
  group_by(Code, Date) %>%
  summarise(online_found_max = max(online_found), online_found_mean = mean(online_found),
            online_today_max = max(online_today), online_today_mean = mean(online_today),
            online_yday_max = max(online_yday), online_yday_mean = mean(online_yday),
            closeness_online_today_max  = max(closeness_online_today), closeness_online_today_mean = mean(closeness_online_today),
            closeness_online_yday_max  = max(closeness_online_yday), closeness_online_yday_mean = mean(closeness_online_yday))


"
Import the full info for articles
"
full_df <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/FTarticles_full.csv"))
full_df <- full_df %>%
  filter(date_num >= "1998-05-11")

"
Merge full info back into the matched articles
"
matched_extra <- inner_join(matched_data, select(full_df, date_num, headline, main_text, section, author))



extra_vars <- matched_extra %>%
  select(date_num, Code, article_id, headline, section, author) %>%
  mutate(page = str_extract(section, "Pg\\. [0-9]+")) %>%
  mutate(page_no = case_when(str_detect(page, "[0-9]+") ~ as.numeric(str_extract(page, "[0-9]+")), 
         TRUE ~ 999999)) %>% 
  mutate(author_identified = as.numeric(!is.na(author))) %>%
  rename(Date = date_num) %>% mutate(Date = as.Date(Date)) %>%
  group_by(Code, Date) %>%
  summarise(page_no = case_when(any(page_no != 999999) ~ min(page_no, na.rm = T), TRUE ~ NA_real_),
            author_identified = mean(author_identified, na.rm = T)) %>%
  ungroup() %>%
  left_join(online_articles, by = c("Code", "Date"), values) %>%
  right_join(select(BTR_data, Code, Date, mention)) %>%
  mutate(page_no = replace_na(mention*page_no, 0), author_identified = replace_na(mention*author_identified, 0),
         online_found_max = replace_na(mention*online_found_max, 0), online_found_mean = replace_na(mention*online_found_mean, 0),
         online_today_max = replace_na(mention*online_today_max, 0), online_today_mean = replace_na(mention*online_today_mean, 0),
         online_yday_max = replace_na(mention*online_yday_max, 0), online_yday_mean = replace_na(mention*online_yday_mean, 0),
         closeness_online_today_max = replace_na(mention*closeness_online_today_max, 0), 
         closeness_online_today_mean = replace_na(mention*closeness_online_today_mean, 0),
         closeness_online_yday_max = replace_na(mention*closeness_online_yday_max, 0), 
         closeness_online_yday_mean = replace_na(mention*closeness_online_yday_mean, 0)) %>%
  select(-mention)

tabyl(is.na(extra_vars$online_found_max))

new_data <- BTR_data %>%
  left_join(extra_vars, by = c("Code", "Date")) %>%
  mutate(frontpage = as.numeric(page_no == 1))

new_data %>%
  tabyl(online_found_max, online_today_max, online_yday_max)
table(new_data$frontpage)

mean(new_data$highlow)


"
New analysis
"
# Units of the market value variable are in thousands of pounds, as market cap of AAL is between 10 and 50 billion, 
# so if I divide by 1e3, it is in millions
new_data %>%
  filter(Code == "AAL.L") %>%
  group_by(year) %>%
  summarise(market_value_mean = mean(market_value, na.rm = T), market_value_last = last(market_value, order_by = Date),
            market_value_first = first(market_value, order_by = Date))

# Work out what the ten basis points effect is worth 
new_data %>%
  select(Code, Date, year, mention, Open, market_value, Volume) %>%
  filter(mention == 1) %>%
  group_by(Date, year) %>%
  summarise(market_value = sum(market_value, na.rm = T), volume_traded = sum(Volume*Open/1e6), 
            extra_vol = sum(0.05*Volume*Open/1e6)) %>%
  mutate(extra_movement = 0.001*market_value/1e3) %>%
  group_by(year) %>%
  summarise(extra_movement = sum(extra_movement, na.rm = T)/252, extra_vol = sum(extra_vol, na.rm = T)/252)
  

model <- felm_DK_se(highlow ~ mention*author_identified-author_identified + highlow_1lag + abs_overnight + 
                       VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)
model <- felm_DK_se(highlow ~ mention*frontpage-frontpage + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)
model <- felm_DK_se(highlow ~ mention *log(1+page_no)- log(1+page_no) + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)
model <- felm_DK_se(highlow ~ mention + frontpage + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)

model <- felm_DK_se(highlow ~ online_yday_max + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)


new_data %>%
  filter(page_no > 0 ) %>%
  ggplot() + theme_bw() + 
  geom_bar(aes(x = page_no, y = 1), stat = "summary", fun = "sum")
  
           
select(full_df, date_num, headline, section, author) %>%
  unique()

matched_articles <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/short_articles.csv"))




for (ii in 1:nrow(matched_articles)){
  
  hline <- matched_articles$headline[ii]
  date <- matched_articles$date_num [ii]

}

full_df <- full_df %>%
  rename(article_id = X)


headmatched_articles <- inner_join(full_df, head_mentions, by = "article_id")



matched_articles <- matched_articles %>%
  mutate(date_num = Date) %>%
  distinct(article_id, date_num, headline, .keep_all = TRUE)
  

full_df %>%
  filter(str_detect(headline, "Anglo American takes full control"))

full_matched <- inner_join(matched_articles, full_df, by = c("date_num", "headline"))


which(!(matched_articles$article_id %in% full_matched$article_id))

matched_articles %>%
  distinct(date_num, headline, .keep_all = TRUE)


full_df$section[1:10]


BATS_df <- as_tibble(read.csv("/Users/julianashwin/Downloads/BATS.L_intraday.csv")) %>%
  mutate(code = "BATS")
AAL_df <- as_tibble(read.csv("/Users/julianashwin/Downloads/AAL.L_intraday.csv")) %>%
  mutate(code = "AAL")
thirtymin_df <- rbind(AAL_df, BATS_df)


AAL_df <- as_tibble(read.csv("/Users/julianashwin/Downloads/AAL.L_5min.csv")) %>%
  mutate(code = "AAL")



daily_df <-
  thirtymin_df %>%
  filter(Open != "" & Close != "") %>%
  filter(!is.na(Open) & !is.na(Close)) %>%
  mutate(Local.Date = as.Date(Local.Date, format = "%d-%b-%Y")) %>%
  arrange(code, Local.Date, Local.Time) %>%
  mutate(Close = as.numeric(str_remove(Close, ",")),
         Open = as.numeric(str_remove(Open, ",")),
         High = as.numeric(str_remove(High, ",")),
         Low = as.numeric(str_remove(Low, ","))) %>%
  group_by(code, Local.Date) %>%
  mutate(Return = log(Close) - lag(log(Close), order_by = Local.Time)) %>%
  summarise(Open = first(Open, order_by = Local.Time), Close = last(Close, order_by = Local.Time), 
            Low = min(Low, na.rm = T), High = max(High, na.rm = T), 
            Variance = sum(Return^2, na.rm = T)) %>%
  mutate(HighLow = (High - Low)/Low) %>%
  mutate(IntraDay = abs(Close - Open)/Open)
daily_df

cor.test(daily_df$HighLow, (daily_df$IntraDay))


cor.test(daily_df$IntraDay, sqrt(daily_df$Variance))
