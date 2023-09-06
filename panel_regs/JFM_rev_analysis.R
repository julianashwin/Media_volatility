setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")

library(tidyverse)
library(janitor)
library(lfe)
library(fixest)
library(lubridate)
library(tictoc)
library(stargazer)



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

get_quantile <- function(x, qnt, as_numeric = TRUE){
  
  x_qnts <- cut(x, breaks = Quantile(x, probs = seq(0,1,1/qnt), na.rm = T),
                labels = 1:qnt, include.lowest = TRUE)
  if (as_numeric){
    x_qnts <- as.numeric(x_qnts)
  }
  return(x_qnts)
}


base_controls <- "abs_overnight + VI_put_1lag + VI_put_2lag + VI_put_3lag + VI_put_4lag + VI_put_5lag +
                       VI_call_1lag + VI_call_2lag + VI_call_3lag + VI_call_4lag + VI_call_5lag +
                       highlow_1lag + highlow_2lag + highlow_3lag + highlow_4lag + highlow_5lag + 
                       highlow_6lag + highlow_7lag + highlow_8lag + highlow_9lag + highlow_10lag +
                       lVolume_1lag + lVolume_2lag + lVolume_3lag + lVolume_4lag + lVolume_5lag +
                       lVolume_6lag + lVolume_7lag + lVolume_8lag + lVolume_9lag + lVolume_10lag + 
                       abs_return_1lag + abs_return_2lag + abs_return_3lag + abs_return_4lag + abs_return_5lag +
                       abs_return_6lag + abs_return_7lag + abs_return_8lag + abs_return_9lag + abs_return_10lag +
                       return_1lag + return_2lag + return_3lag + return_4lag + return_5lag + 
                       return_6lag + return_7lag + return_8lag + return_9lag + return_10lag"


# Import data
panel_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/JFM_rev_data.rds")

model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + period")), panel_df)
summary(model1)





"
Point 1 - Robustness with intraday volatility as sum of squared stock returns
"
### see clean_intraday_data.R script for this




"
Point 2 - Economic significance of effect 
"
# Units of the market value variable are in thousands of pounds, as market cap of AAL is between 10 and 50 billion, 
# so if I divide by 1e3, it is in millions
panel_df %>%
  filter(Code == "AAL.L") %>%
  group_by(year) %>%
  summarise(market_value_mean = mean(market_value, na.rm = T), market_value_last = last(market_value, order_by = Date),
            market_value_first = first(market_value, order_by = Date))

# Get the US dollar to GBP exchange rate 
fx_df <- read_csv("data/AEXUSUK.csv") %>%
  mutate(year = year(DATE)) %>% dplyr::select(-DATE)

# Get GDP deflator
defl_df <- read_csv("data/USAGDPDEFAISMEI.csv") %>%
  mutate(year = year(DATE)) %>% dplyr::select(-DATE)



# Work out what the ten basis points effect is worth 
panel_df %>%
  select(Code, Date, year, mention, Open, market_value, Volume) %>%
  filter(mention == 1) %>%
  group_by(Date, year) %>%
  summarise(market_value = sum(market_value, na.rm = T), volume_traded = sum(Volume*Open/1e6), 
            extra_vol = sum(0.055*Volume*Open/1e6)) %>%
  mutate(extra_movement = 0.001*market_value/1e3) %>%
  group_by(year) %>%
  summarise(extra_movement = sum(extra_movement, na.rm = T)/252, extra_vol = sum(extra_vol, na.rm = T)/252) %>%
  mutate(extra_vol_bils = extra_vol/1000) %>%
  left_join(fx_df) %>%
  left_join(defl_df) %>%
  mutate(extra_vol_bils = AEXUSUK*extra_vol_bils/USAGDPDEFAISMEI*100, 
         extra_movement = AEXUSUK*extra_movement/USAGDPDEFAISMEI*100) %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = year, y = extra_vol_bils*25, linetype = "Average daily value of volume effect")) +
  geom_line(aes(x = year, y = extra_movement, linetype = "Average daily value of volatilty effect")) + 
  scale_y_continuous(sec.axis = sec_axis(~ . /25, name = "Daily volume effect (2015 US$ billion)")) + 
  labs(x = "Year", y = "Daily volatility effect (2015 US$ million)", linetype = "")
ggsave("figures/econ_significance.pdf", width = 8, height = 3.5)


panel_df %>%
  select(Code, Date, year, mention, Open, market_value, Volume) %>%
  filter(mention == 1) %>%
  group_by(Date, year) %>%
  summarise(market_value = sum(market_value, na.rm = T), volume_traded = sum(Volume*Open/1e6), 
            extra_vol = sum(0.055*Volume*Open/1e6)) %>%
  mutate(extra_movement = 0.001*market_value/1e3) %>%
  group_by(year) %>%
  summarise(extra_movement = sum(extra_movement, na.rm = T)/252, extra_vol = sum(extra_vol, na.rm = T)/252) %>%
  mutate(extra_vol_bils = extra_vol/1000) %>%
  left_join(fx_df) %>%
  left_join(defl_df) %>%
  mutate(extra_vol_bils = AEXUSUK*extra_vol_bils/USAGDPDEFAISMEI*100, 
         extra_movement = AEXUSUK*extra_movement/USAGDPDEFAISMEI*100) %>%
  summarise(extra_vol_bils = sum(extra_vol_bils)*252, extra_movement = sum(extra_movement)*252)


"
Point 3: How should we think about the possibility of new information in the FT?
"

## Get sentiment of online versions
sentiment_df <- panel_df %>% 
  filter(mention == 1) %>%
  select(Code, Date, mention, headline, main_text, 
         closest_online_headline_yday, closest_online_headline_today,
         closest_online_text_yday, closest_online_text_today) %>%
  mutate(main_text = replace_na(str_squish(main_text), ""),
         headline = replace_na(str_squish(headline), ""),
         closest_online_text_yday = replace_na(str_squish(closest_online_text_yday), ""),
         closest_online_text_today = replace_na(str_squish(closest_online_text_today), ""),
         closest_online_headline_yday = replace_na(str_squish(closest_online_headline_yday), ""),
         closest_online_headline_today = replace_na(str_squish(closest_online_headline_today), ""),
         LM_sentiment_main_text = NA_real_,
         LM_sentiment_headline = NA_real_,
         LM_sentiment_closest_online_text_yday = NA_real_,
         LM_sentiment_closest_online_headline_yday = NA_real_,
         LM_sentiment_closest_online_text_today = NA_real_,
         LM_sentiment_closest_online_headline_today = NA_real_)

library(SentimentAnalysis)
pb = txtProgressBar(min = 1, max = nrow(sentiment_df), initial = 1) 
for (ii in 1:nrow(sentiment_df)){
  # Headline
  if (sentiment_df$headline[ii] != ""){
    para <- sentiment_df$headline[ii]
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_headline[ii] <- lm_sentiment[1,1]
  } 
  # Main text
  if (sentiment_df$main_text[ii] != ""){
    para <- str_sub(sentiment_df$main_text[ii], 1,1200)
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_main_text[ii] <- lm_sentiment[1,1]
  } 
  # Closest online headline yday
  if (sentiment_df$closest_online_headline_yday[ii] != ""){
    para <- sentiment_df$closest_online_headline_yday[ii]
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_closest_online_headline_yday[ii] <- lm_sentiment[1,1]
  } 
  # Closest online main text yday
  if (sentiment_df$closest_online_text_yday[ii] != ""){
    para <- sentiment_df$closest_online_text_yday[ii]
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_closest_online_text_yday[ii] <- lm_sentiment[1,1]
  }
  # Closest online headline today
  if (sentiment_df$closest_online_headline_today[ii] != ""){
    para <- sentiment_df$closest_online_headline_today[ii]
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_closest_online_headline_today[ii] <- lm_sentiment[1,1]
  } 
  # Closest online main text today
  if (sentiment_df$closest_online_text_today[ii] != ""){
    para <- sentiment_df$closest_online_text_today[ii]
    lm_sentiment <- analyzeSentiment(para,rules=list("SentimentLM"=list(ruleSentiment,loadDictionaryLM())))
    sentiment_df$LM_sentiment_closest_online_text_today[ii] <- lm_sentiment[1,1]
  }
  setTxtProgressBar(pb,ii)
}
sentiment_df <- sentiment_df %>%
  mutate(main_text_nwords = replace_na(str_count(main_text, " "), 0),
         headline_nwords = replace_na(str_count(headline, " "), 0),
         main_text_closest_yday_nwords = replace_na(str_count(closest_online_text_yday, " "), 0),
         headline_nwords_closest_yday = replace_na(str_count(closest_online_headline_yday, " "), 0),
         main_text_closest_today_nwords = replace_na(str_count(closest_online_text_today, " "), 0),
         headline_nwords_closest_today = replace_na(str_count(closest_online_headline_today, " "), 0))


### Similarity of online and print editions. 
if (FALSE){
  library(doc2vec)
  library(lsa)
  doc2vec_df <- sentiment_df %>%
    select(Code, Date, headline, main_text, closest_online_headline_yday, closest_online_headline_today, 
           closest_online_text_today, closest_online_text_yday) %>%
    pivot_longer(cols = -c(Code, Date), names_to = "type", values_to = "text") %>%
    filter(text != "") %>%
    mutate(text = str_sub(text, 1, 5000)) %>%
    mutate(doc_id = str_c(Code, "**", Date, "**", type)) %>%
    select(doc_id, text)
  doc2vec_df
  
  tic() 
  doc2vec_model <- paragraph2vec(doc2vec_df, type = "PV-DM", dim = 100, iter = 50)
  toc()
  
  # Extract and merge embedding vectors back in 
  embeddings <- as.data.frame(as.matrix(doc2vec_model, which = "docs"))
  embeddings$doc_id <- rownames(embeddings)
  embeddings <- embeddings %>%
    tibble() %>% relocate(doc_id, .before = V1)
  
  embeddings_df <- doc2vec_df %>%
    left_join(tibble(embeddings)) %>%
    separate(doc_id, into = c("Code", "Date", "type"), sep = "\\*\\*") %>%
    pivot_longer(cols = `V1`:`V100`) %>%
    arrange(Code, Date, type) %>%
    pivot_wider(id_cols = c(Code, Date, name), names_from = type, values_from = value)
  
  
  saveRDS(embeddings_df, "/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/article_embeddings.rds")
  
}

embeddings_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/article_embeddings.rds")

similarity_df <- embeddings_df %>%
  group_by(Code, Date) %>%
  summarise(headline_main_print = cosine(headline, main_text)[1],
            headline_headline_yday = cosine(headline, closest_online_headline_yday)[1],
            main_main_yday = cosine(main_text, closest_online_text_yday)[1],
            headline_headline_today = cosine(headline, closest_online_headline_today)[1],
            main_main_today = cosine(main_text, closest_online_text_today)[1]) %>%
  mutate(Date = as.Date(Date)) %>%
  left_join(sentiment_df)


panel_online_df <- similarity_df %>%
  select(Code, Date, headline_main_print, headline_headline_yday, main_main_yday, headline_headline_today, main_main_today,
         LM_sentiment_headline, LM_sentiment_main_text, LM_sentiment_closest_online_headline_yday, LM_sentiment_closest_online_text_yday,
         main_text_nwords, main_text_closest_yday_nwords) %>%
  right_join(panel_df)


# Plot cosine similarity 
panel_online_df %>%
  filter(mention == 1) %>%
  pivot_longer(cols = c(main_main_today, main_main_yday), names_to = "Combo", values_to = "similarity") %>%
  mutate(Combo = case_when(Combo == "main_main_today" ~ "Same day", Combo == "main_main_yday" ~ "Previous day")) %>%
  ggplot() + theme_bw() + 
  geom_histogram(aes(x = similarity, group = Combo, fill = Combo), position = "dodge") +
  labs(x = "Cosine similarity of online and print article", y = "Count", 
       fill = "Matched online article")
ggsave("figures/online_article_similarity.pdf", width = 5, height = 3.5)

# How many matches?
tabyl(panel_online_df$mention[panel_online_df$Date > "2000-09-01"])
tabyl(panel_online_df$online_today_max)
tabyl(panel_online_df$online_yday_max)

# Some examples
similarity_df[9892,]
as.character(similarity_df$headline[9892])
as.character(similarity_df$main_text[9892])
as.character(similarity_df$closest_online_headline_yday[9892])
as.character(similarity_df$closest_online_text_yday[9892])
similarity_df$main_main_yday[9892]


panel_online_df <- panel_online_df %>%
  mutate(online_yday_0p6 = replace_na(online_yday_max*(main_main_yday >= 0.6), 0),
         online_yday_0p65 = replace_na(online_yday_max*(main_main_yday >= 0.65), 0),
         online_yday_0p7 = replace_na(online_yday_max*(main_main_yday >= 0.7), 0),
         online_yday_0p75 = replace_na(online_yday_max*(main_main_yday >= 0.75), 0),
         online_yday_0p8 = replace_na(online_yday_max*(main_main_yday >= 0.8), 0),
         online_yday_0p85 = replace_na(online_yday_max*(main_main_yday >= 0.85), 0),
         online_yday_0p9 = replace_na(online_yday_max*(main_main_yday >= 0.9), 0),
         online_yday_0p95 = replace_na(online_yday_max*(main_main_yday >= 0.95), 0))

panel_online_df %>%
  ungroup() %>%
  summarise(online_yday_0p6 = sum(online_yday_0p6), online_yday_0p65 = sum(online_yday_0p65), 
            online_yday_0p7 = sum(online_yday_0p7), online_yday_0p75 = sum(online_yday_0p75), 
            online_yday_0p8 = sum(online_yday_0p8), online_yday_0p85 = sum(online_yday_0p85), 
            online_yday_0p9 = sum(online_yday_0p9), online_yday_0p95 = sum(online_yday_0p95)) %>%
  pivot_longer(everything()) %>%
  mutate(name = as.numeric(str_replace(str_remove(name, "online_yday_"), "p", "."))) %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = name, y = value))

## Effect with different similarity thresholds 
model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + period")), panel_online_df)
model2 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_max + ", base_controls, "| Code + period")), panel_online_df)
model3 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p6 + ", base_controls, "| Code + period")), panel_online_df)
model4 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p65 + ", base_controls, "| Code + period")), panel_online_df)
model5 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p7 + ", base_controls, "| Code + period")), panel_online_df)
model6 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p75 + ", base_controls, "| Code + period")), panel_online_df)
model7 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p8 + ", base_controls, "| Code + period")), panel_online_df)
model8 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p85 + ", base_controls, "| Code + period")), panel_online_df)
model9 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p9 + ", base_controls, "| Code + period")), panel_online_df)
model10 <- felm_DK_se(as.formula(str_c("highlow ~ online_yday_0p95 + ", base_controls, "| Code + period")), panel_online_df)
summary(model10)

online_coefs <- tibble(as.data.frame(rbind(c("Baseline",summary(model1)$coefficients["mention",]),
                      c("None",summary(model2)$coefficients["online_yday_max",]),
                      c("0.6",summary(model3)$coefficients["online_yday_0p6",]),
                      c("0.7",summary(model5)$coefficients["online_yday_0p7",]),
                      c("0.8",summary(model7)$coefficients["online_yday_0p8",]),
                      c("0.9",summary(model9)$coefficients["online_yday_0p9",]))))

online_coefs %>%
  mutate(coef = as.numeric(Estimate), se = as.numeric(`Std. Error`)) %>%
  mutate(model = factor(V1, ordered = T, levels = c("Baseline", "None", "0.6", "0.7", "0.8", "0.9"))) %>%
  ggplot() + theme_bw() + 
  geom_point(aes(x = model, y = coef)) + 
  geom_errorbar(aes(x = model, ymin = coef - 1.96*se, ymax = coef + 1.96*se), width=.3) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Similarity threshold", y = "Article effect")
ggsave("figures/online_article_effects.pdf", width = 3, height = 3.5)





"
Point 4: relate to theoretical literature
"




"
Point 5: Robustness to additional DK lags
"

DK_coefs <- data.frame(DK_lags = c(0:20), coef = 0, se = 0)
for (ii in 1:nrow(DK_coefs)){
  print(ii)
  if (ii == 0){
    model <- felm(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + period")), panel_df)
  } else {
    model <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + period")), 
                        panel_df, nlags = DK_coefs$DK_lags[ii])
  }
  
  DK_coefs$coef[ii] <- summary(model)$coefficients["mention","Estimate"]
  DK_coefs$se[ii] <- summary(model)$coefficients["mention","Std. Error"]
  
}

DK_coefs %>%
  ggplot(aes(x=DK_lags, y=coef)) + theme_bw() + 
  geom_point(shape=21, size=3, fill="white") +
  geom_errorbar(width=.3, aes(ymin=coef-1.96*se, ymax=coef+1.96*se)) + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  labs(x = "Driscoll-Kraay lags", y = "FT article effect")
ggsave("figures/effect_DK_lags.pdf", width = 6, height = 2.5)



"
Point 6 - market volatility rather than day fixed effects 
"
# Import FTSE VIX data 
vix_df <- read_csv("data/FTSE 100 VIX Historical Data.csv") %>%
  mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
  rename(VIX = Price) %>%
  dplyr::select(Date, VIX)

vix_df %>%
  ggplot(aes(x = Date)) + theme_bw() + 
  geom_line(aes(y = VIX))

reg_df <- panel_df %>%
  left_join(vix_df)


model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", 
                                      base_controls, "| Code + period")), reg_df, nlags = 3)
summary(model1)
model2 <- felm_DK_se(as.formula(str_c("highlow ~ mention + IndexHighLow + ", 
                                      base_controls, "| Code ")), reg_df, nlags = 3)
summary(model2)
model3 <- felm_DK_se(as.formula(str_c("highlow ~ mention + abs_overnight + IndexHighLow + Index_abs_Change + Index_Change + ", 
                                      base_controls, "| Code ")), reg_df, nlags = 3)
summary(model3)

stargazer(model1, model2, model3, table.placement = "H", df = F)



"
Point 7 - weight effects by market cap
"
# Import and clean sector lables for ONS tables 
matrix_labels <- read_csv("~/Documents/DPhil/Clean_Data/UK_macro/input-output/ONS_mat_labels.csv") %>%
  mutate(x = str_replace_all(x, " ", "_"), x = str_replace_all(x, "\\&", "and"), x = str_replace_all(x, "\\.", "_"),
         x = str_replace_all(x, "\\,", "_"), x = str_replace_all(x, "-", "to"), x = str_replace_all(x, "41__42__and_43", "41__42_and_43"))
# Import and clean up cons matrix
cons_matrix <- read.csv("~/Documents/DPhil/Clean_Data/UK_macro/input-output/average_supplier_matrix_intconsweighted.csv")
colnames(cons_matrix) <- matrix_labels$x
cons_matrix$shocked_Sector <- matrix_labels$x
cons_matrix <- cons_matrix %>% 
  pivot_longer(cols = -shocked_Sector, names_to = "this_Sector", values_to = "cons_weight")
# Import and clean up dem matrix
dem_matrix <- read.csv("~/Documents/DPhil/Clean_Data/UK_macro/input-output/average_consumer_matrix_intdemweighted.csv")
dem_matrix[is.na(dem_matrix)] <- 0
colnames(dem_matrix) <- matrix_labels$x
dem_matrix$this_Sector <- matrix_labels$x
dem_matrix <- dem_matrix %>% 
  pivot_longer(cols = -this_Sector, names_to = "shocked_Sector", values_to = "dem_weight")
# Import spillover variables from 
weighted_mentions_short <- read_csv("~/Documents/DPhil/Clean_Data//FT/matched/weighted_mention_avg.csv") %>%
  rename(own_sec_mention_notme_old = own_sec_mention_notme, 
         own_sec_mention_avg_notme_old = own_sec_mention_avg_notme, 
         cons_weighted_mention_avg_old = cons_weighted_mention_avg, 
         dem_weighted_mention_avg_old = dem_weighted_mention_avg,
         placebo_weighted_mention_avg_old = placebo_weighted_mention_avg,
         own_sec_highlow_notme_old = own_sec_highlow_notme) %>%
  dplyr::select(Code, Date, own_sec_mention_notme_old, own_sec_mention_avg_notme_old, cons_weighted_mention_avg_old,
                 dem_weighted_mention_avg_old, placebo_weighted_mention_avg_old, own_sec_highlow_notme_old)


panel_df %>% 
  group_by(Code) %>% 
  summarise(market_value = mean(market_value, na.rm = T),
            lmarket_value = mean(lmarket_value, na.rm = T)) %>%
  ggplot() + theme_bw() + 
  geom_density(aes(x = lmarket_value))


# Get within sector variables
sector_df <- panel_df %>%
  group_by(Date, Sector) %>%
  summarise(N_sector = n(),
            market_value_sector = sum(market_value, na.rm = T), mention_max = max(mention),  mention_sum = sum(mention),
            mention_sum_wt = sum(mention*market_value, na.rm = T), highlow_sum = sum(highlow),
            highlow_sum_wt = sum(highlow*market_value, na.rm = T)) %>% 
  ungroup() %>%
  mutate(mention_avg = mention_sum/N_sector, 
         mention_avg_wt = case_when(market_value_sector > 0 ~ mention_sum_wt/market_value_sector, TRUE ~ 0),
         highlow_avg = highlow_sum/N_sector, 
         highlow_avg_wt = case_when(market_value_sector > 0 ~ highlow_sum_wt/market_value_sector, TRUE ~ 0))
# Get Sector level shocks
shocks_df <- sector_df %>%
  right_join(crossing(Date = unique(panel_df$Date), Sector = matrix_labels$x)) %>%
  arrange(Date, Sector) %>%
  mutate(mention_max = replace_na(mention_avg, 0),
         mention_avg = replace_na(mention_avg, 0),
         mention_avg_wt = replace_na(mention_avg_wt, 0)) %>%
  rename(shocked_Sector = Sector) %>%
  select(Date, shocked_Sector, mention_max, mention_avg, mention_avg_wt)
# Combine shocks and weights
weights_shocks_df <- dem_matrix %>%
  full_join(cons_matrix) %>%
  full_join(shocks_df) %>%
  filter(mention_max != 0, mention_avg != 0 , mention_avg_wt != 0) %>%
  filter(dem_weight != 0 | cons_weight != 0 )
# Calcuate total shocks
network_shocks_df <- weights_shocks_df %>%
  group_by(this_Sector, Date) %>%
  summarise(cons_ntwk_mention_max = sum(cons_weight*mention_max),
            cons_ntwk_mention_avg = sum(cons_weight*mention_avg),
            cons_ntwk_mention_avg_wt = sum(cons_weight*mention_avg_wt),
            dem_ntwk_mention_max = sum(dem_weight*mention_max),
            dem_ntwk_mention_avg = sum(dem_weight*mention_avg),
            dem_ntwk_mention_avg_wt = sum(dem_weight*mention_avg_wt)) %>%
  rename(Sector = this_Sector)

## Combine various spillover variables. 
panel_spillover_df <- panel_df %>%
  left_join(weighted_mentions_short) %>%
  left_join(sector_df) %>%
  left_join(network_shocks_df) %>%
  mutate(own_sec_mention_avg_notme = (mention_sum - mention)/N_sector,
         own_sec_mention_avg_wt_notme = 
           case_when(market_value_sector > 0 ~ (mention_sum_wt - mention*market_value)/market_value_sector, 
                     TRUE ~ NA_real_),
         own_sec_mention_max_notme = as.numeric(own_sec_mention_avg_notme > 0),
         own_sec_highlow_avg_notme = (highlow_sum - highlow)/N_sector,
         own_sec_highlow_avg_wt_notme = highlow_avg_wt - (highlow*market_value)/market_value_sector)


# Baseline
model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention +", base_controls, "| Code + Date")), panel_spillover_df)
# Only cons
model2 <- felm_DK_se(as.formula(str_c("highlow ~ mention + cons_weighted_mention_avg_old + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Only dem
model3 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_weighted_mention_avg_old + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Both
model4 <- felm_DK_se(as.formula(str_c("highlow ~ mention +  dem_weighted_mention_avg_old + cons_weighted_mention_avg_old + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Both plus own sector
model5 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_weighted_mention_avg_old + cons_weighted_mention_avg_old + 
                                      own_sec_mention_avg_notme_old + ", base_controls, "| Code + Date")),  panel_spillover_df)
# Both, own sec mention and vol
model6 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_weighted_mention_avg_old + cons_weighted_mention_avg_old +
                 + own_sec_mention_avg_notme_old + own_sec_highlow_notme_old + ", base_controls, "| Code + Date")), panel_spillover_df)
# Both, own sec mention and vol
model7 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_ntwk_mention_avg_wt + cons_ntwk_mention_avg_wt +
                 + own_sec_mention_avg_wt_notme + own_sec_highlow_avg_wt_notme + ", base_controls, "| Code + Date")), panel_spillover_df)
summary(model7) 

stargazer(model1, model2, model3, model4, model5, model6, model7,
          table.placement = "H", df = F) 


# Baseline
model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention +", base_controls, "| Code + Date")), panel_spillover_df)
# Only cons
model2 <- felm_DK_se(as.formula(str_c("highlow ~ mention + cons_ntwk_mention_avg_wt + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Only dem
model3 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_ntwk_mention_avg_wt + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Both
model4 <- felm_DK_se(as.formula(str_c("highlow ~ mention +  dem_ntwk_mention_avg_wt + cons_ntwk_mention_avg_wt + ", 
                                      base_controls, "| Code + Date")), panel_spillover_df)
# Both plus own sector
model5 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_ntwk_mention_avg_wt + cons_ntwk_mention_avg_wt + 
                                      own_sec_mention_avg_wt_notme + ", base_controls, "| Code + Date")),  panel_spillover_df)
# Both, own sec mention and vol
model6 <- felm_DK_se(as.formula(str_c("highlow ~ mention + dem_ntwk_mention_avg_wt + cons_ntwk_mention_avg_wt +
                 + own_sec_mention_avg_wt_notme + own_sec_highlow_avg_wt_notme + ", base_controls, "| Code + Date")), panel_spillover_df)
summary(model6) 
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F) 



### Index level effect
index_data <- panel_df %>%
  group_by(Date) %>%
  summarise(mention_weighted = sum(market_value_1lag*mention, na.rm = T)/sum(market_value_1lag, na.rm = T),
            mention = mean(mention, na.rm = T), 
            index_val = sum(market_value, na.rm = T), 
            IndexHighLow = mean(IndexHighLow, na.rm = T)) %>%
  ungroup() %>%
  mutate(IndexHighLow_1lag = lag(IndexHighLow, order_by = Date))

cor.test(index_data$mention, index_data$mention_weighted)

ggplot(index_data) + 
  geom_line(aes(x = as.Date(Date), y = mention*100, color = "mention")) + 
  geom_line(aes(x = as.Date(Date), y = mention_weighted*100, color = "mention_wt")) + 
  geom_line(aes(x = as.Date(Date), y = IndexHighLow, color = "highlow"))

model1 <- lm(IndexHighLow ~ mention, index_data)
model2 <- lm(IndexHighLow ~ mention + IndexHighLow_1lag, index_data) 
model3 <- lm(IndexHighLow ~ mention_weighted + IndexHighLow_1lag, index_data) 
stargazer(model1, model2, model3, table.placement = "H", df = F)







"
Point 8 - typos
"


"
Point 9 - Descriptions for tables and figures
"

"
Point 10 - front page (and number of trades)
"
panel_ntrades_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/panel_intraday_df.rds") %>%
  rename(NumTrades = NumTrades_60min) %>%
  select(Code, Date, NumTrades) %>% 
  right_join(panel_df) %>%
  arrange(Code, Date) %>%
  mutate(otherpages = mention - frontpage,
         lNumTrades = log(NumTrades),
         AvTrade = Volume/NumTrades,
         lAvTrade = log(Volume/NumTrades))



## Baseline model
model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + period")), panel_ntrades_df)
model2 <- felm_DK_se(as.formula(str_c("lVolume ~ mention + ", base_controls, "| Code + period")), panel_ntrades_df)
model3 <- felm_DK_se(as.formula(str_c("abs_intra_day ~ mention + ", base_controls, "| Code + period")), panel_ntrades_df)
model4 <- felm_DK_se(as.formula(str_c("highlow ~ mention + abs_intra_day + ", base_controls, "|Code + period")), panel_ntrades_df)
model5 <- felm_DK_se(as.formula(str_c("highlow ~ mention + lVolume +  ", base_controls, "|Code + period")), panel_ntrades_df)
model6 <- felm_DK_se(as.formula(str_c("highlow ~ mention + abs_intra_day + lVolume +  ", base_controls, "|Code + period")), panel_ntrades_df)
stargazer(model1, model2, model3, model4, model5, model6,
          table.placement = "H", df = F)



## Baseline model
model1 <- felm_DK_se(as.formula(str_c("highlow ~ frontpage + otherpages + ", base_controls, "| Code + period")), panel_ntrades_df)
model2 <- felm_DK_se(as.formula(str_c("lVolume ~ frontpage + otherpages + ", base_controls, "| Code + period")), panel_ntrades_df)
model3 <- felm_DK_se(as.formula(str_c("abs_intra_day ~ frontpage + otherpages + ", base_controls, "| Code + period")), panel_ntrades_df)
model4 <- felm_DK_se(as.formula(str_c("highlow ~ frontpage + abs_intra_day + otherpages + ", base_controls, "|Code + period")), panel_ntrades_df)
model5 <- felm_DK_se(as.formula(str_c("highlow ~ frontpage + lVolume + otherpages + ", base_controls, "|Code + period")), panel_ntrades_df)
model6 <- felm_DK_se(as.formula(str_c("highlow ~ frontpage + abs_intra_day + lVolume + otherpages + ", base_controls, "|Code + period")), panel_ntrades_df)
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



panel_df %>%
  tabyl(mention, frontpage)

model1 <- felm_DK_se(as.formula(str_c("highlow ~ mention + ", base_controls, "| Code + Date")), panel_spillover_df)
summary(model1)

model <- felm_DK_se(highlow ~ mention + frontpage + highlow_1lag + abs_overnight + 
                      VI_put_1lag + VI_call_1lag| Code + period, new_data)
summary(model)


model <- felm_DK_se(highlow ~ mention + frontpage + abs_overnight + 
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
                    | Code + period, 
                    panel_df, nlags = 3)
summary(model)
model <- felm_DK_se(highlow ~ mention + log(1+page_no) + abs_overnight + 
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
                    | Code + period, 
                    panel_df, nlags = 3)
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




"
Additional exercise - number of trades
"
panel_intraday_df <- readRDS("/Users/julianashwin/Documents/DPhil/Firm_level_news/JFM_version/data/panel_intraday_df.rds")


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
