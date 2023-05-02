setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")

library(tidyverse)

## Get more article details

BTR_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/BTR_FT_data.csv"))

## Get data to use for identifying online articles
headline_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/headline_companyarticles.csv"))
NER_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/NER_companyarticles.csv"))
both_data <- as_tibble(read.csv("/Users/julianashwin/Documents/DPhil/Clean_Data/FT/matched/both_companyarticles_collapsed.csv"))

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
  mutate(page_no = as.numeric(str_extract(page, "[0-9]+"))) %>% 
  mutate(author_identified = as.numeric(!is.na(author))) %>%
  rename(Date = date_num) %>%
  group_by(Code, Date) %>%
  summarise(page_no = min(page_no, na.rm = T), 
            author_identified = mean(author_identified, na.rm = T)) %>%
  ungroup() %>%
  mutate(page_no = case_when(is.infinite(page_no) ~ NA_real_, TRUE ~ page_no)) 

new_data <- BTR_data %>%
  left_join(extra_vars, by = c("Code", "Date")) %>%
  mutate(author_identified = replace_na(mention*author_identified, 0)) %>%
  mutate(page_no = replace_na(mention*page_no, 0))
new_data <- new_data %>%
  mutate(frontpage = as.numeric(page_no == 1))

new_data %>%
  select(page_no, mention) %>%
  table()
table(new_data$frontpage)

model <- felm_DK_se(highlow ~ mention + author_identified + highlow_1lag + abs_overnight + 
                       VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)
model <- felm_DK_se(highlow ~ mention + log(1+page_no) + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)
model <- felm_DK_se(highlow ~ mention + frontpage + highlow_1lag + abs_overnight + 
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
