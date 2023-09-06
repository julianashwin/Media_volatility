setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")
rm(list = ls())

library(tidyverse)
library(striprtf)
library(svMisc)
library(progress)
library(tictoc)
library(beepr)
library(lubridate)


raw_dir <- "/Users/julianashwin/Documents/DPhil/Raw_Data/FT_com/"
clean_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/FT_com/"
meta_dir <- "/Users/julianashwin/Documents/GitHub/Media_volatility/selenium_factiva/save_times/"

all_files <- dir(raw_dir)

online_articles_df <- tibble()
files_df <- tibble(filename = all_files, narticles = NA)

pb <- progress_bar$new(total = nrow(files_df) - 991,
                       format = " cleaning [:bar] :elapsedfull and :percent done, so :eta remaining")
for (ii in 1:nrow(files_df)){
  pb$tick()
  filename <- files_df$filename[ii]
  file_text <- read_rtf(str_c(raw_dir, filename))
  
  breaks <- which(str_detect(tolower(file_text), "document ftcom[0-9]+") | 
                    str_detect(tolower(file_text), "document ftft"))
  
  files_df$narticles[ii] <- length(breaks)
  break_points <- c(0, breaks)
  for (jj in 1:length(breaks)){
    # Extract article and remove any empty lines
    article <- str_squish(file_text[(break_points[jj]+1):(break_points[jj+1])])
    article <- article[which(article != "")]
    # Identify some key points
    word_count <- which(str_detect(article, "^[0-9]+ words$|^[0-9]+,[0-9]+ words$"))
    copyright <- which(str_detect(article, "^\\(c\\) [0-9]+|^Copyright [0-9]+"))
    date_line <- which(str_detect(article, "^[0-9]+ [A-Z][a-z]+ [0-9]+$"))
    
    docid <- article[length(article)]
    headline <- article[1]
    word_count <- article[word_count]
    date <- article[date_line]
    main_text <- paste(article[(copyright+1):(length(article)-1)], collapse = " ")
    
    article_row <- tibble(file_id = filename, doc_id = docid, date = date, headline = headline, 
                          word_count = word_count, main_text = main_text)
    
    online_articles_df <- bind_rows(online_articles_df, article_row)
  }
  
}


online_articles_df$online_id <- paste("online", 1:nrow(online_articles_df), sep = "_")
online_articles_df$date_full <- online_articles_df$date
online_articles_df$date <- as.Date(online_articles_df$date_full, format = "%d %B %Y")

online_articles_df$headline <- str_squish(str_remove(online_articles_df$headline, "FT.com site :"))  

online_articles_df <- online_articles_df %>%
  select(online_id, doc_id, file_id, date, headline, main_text, word_count, date_full)


online_articles_df %>%
  write.csv(str_c(clean_dir,"FTcom_articles.csv"), row.names = F)
#online_articles_df <- read_csv(str_c(clean_dir,"FTcom_articles.csv"))

# Import the file metadat
meta_df <- tibble()
meta_files <- dir(meta_dir)
for (meta_file in meta_files){
  file_in <- read_csv(str_c(meta_dir, meta_file))
  meta_df <- rbind(meta_df, file_in)
}
min(meta_df$Date)

# Import the article info
article_info <- read_csv("selenium_factiva/article_info.csv") %>% 
  left_join(select(meta_df, article_id, save_time, online_found)) %>% 
  mutate(online_today = NA, online_yday = NA,
    closest_online_headline_today = NA, closest_online_headline_yday = NA,
    closest_online_text_today = NA, closest_online_text_yday = NA,
    closeness_online_headline_today = NA, closeness_online_headline_yday = NA, 
    closeness_online_text_today = NA, closeness_online_text_yday = NA, 
    nomatch = NA, online_found = replace_na(online_found, "No"))
  

### Loop through each article and see if there's an online equivalent
pb <- progress_bar$new(total = nrow(article_info))
print(paste("Started loop at" , Sys.time()))
for (ii in 1:nrow(article_info)){
  pb$tick()
  
  if (article_info$online_found[ii] == "Yes"){
    code <- article_info$Code[ii]
    company <- gsub("[^a-z0-9 ]", "", tolower(str_remove_all(article_info$Company[ii], ";")))
    today <- article_info$Date[ii]
    yday <- today - 1
    
    # Extract headline and all words
    headline <- tolower(article_info$headline[ii])
    headline_words <- str_split(gsub("[^a-z0-9 ]", "", headline), " ")[[1]]
    main_text <- tolower(article_info$main_text[ii])
    main_text_words <- str_split(gsub("[^a-z0-9 ]", "", main_text), " ")[[1]]
    
    
    savetime <- article_info$save_time[ii] - hours(1) # Adjust for timezone of LBS network and local files
    savetime_alt <- article_info$save_time[ii] - hours(1) - minutes(1) # Adjust for timezone of LBS network and local files
    
    predicted_filename <- str_c(year(savetime), str_pad(month(savetime), 2, pad = "0"), 
                                str_pad(day(savetime), 2, pad = "0"), "-", 
                                str_pad(hour(savetime), 2, pad = "0"), str_pad(minute(savetime), 2, pad = "0"))
    predicted_filename_alt <- str_c(year(savetime_alt), str_pad(month(savetime_alt), 2, pad = "0"), 
                                    str_pad(day(savetime_alt), 2, pad = "0"), "-", 
                                    str_pad(hour(savetime_alt), 2, pad = "0"), str_pad(minute(savetime_alt), 2, pad = "0"))
    mini_df <- online_articles_df %>%
      filter(date >= yday & date <= today) %>%
      filter(str_detect(file_id, predicted_filename) | str_detect(file_id, predicted_filename_alt)) %>%
      mutate(headline_clean = str_squish(gsub("[^a-z0-9 ]", "", tolower(headline))),
             main_text_clean = str_squish(gsub("[^a-z0-9 ]", "", tolower(main_text))),
             company_match = as.numeric(str_detect(headline_clean, company)), 
             closeness_headline = NA,
             closeness_main_text = NA)
    
    
    if (nrow(mini_df) > 0){
      article_info$nomatch[ii] <- 0
      for (jj in 1:nrow(mini_df)){
        # Headline
        headline_candidate <- str_split(mini_df$headline_clean[jj], " ")[[1]]
        mini_df$closeness_headline[jj] <- length(intersect(headline_words, headline_candidate))/length(headline_candidate)
        # Main text
        main_text_candidate <- str_split(mini_df$main_text_clean[jj], " ")[[1]]
        mini_df$closeness_main_text[jj] <- length(intersect(main_text_words, main_text_candidate))/length(main_text_candidate)
      }
      # Pull out today and yesterday
      mini_today <- mini_df[which(mini_df$date == today),]
      mini_yday <- mini_df[which(mini_df$date == yday),]
      # Is there an article matched by headline?
      article_info$online_today[ii] <- as.numeric(any(mini_today$company_match == 1))
      article_info$online_yday[ii] <- as.numeric(any(mini_yday$company_match == 1))
      # Today's match details
      if (nrow(mini_today) > 0){
        article_info$closest_online_headline_today[ii] <- paste(mini_today$headline[which(mini_today$closeness_headline == max(mini_today$closeness_headline) &
                                                                                            mini_today$closeness_headline > 0)], collapse = "; ")
        article_info$closeness_online_headline_today[ii] <-   max(mini_today$closeness_headline) 
        article_info$closest_online_text_today[ii] <- paste(mini_today$main_text[which(mini_today$closeness_main_text == max(mini_today$closeness_main_text) &
                                                                                            mini_today$closeness_main_text > 0)], collapse = "; ")
        article_info$closeness_online_text_today[ii] <-   max(mini_today$closeness_main_text) 
      }
      # Yesterday's match details
      if (nrow(mini_yday) > 0){
        article_info$closest_online_headline_yday[ii] <- paste(mini_yday$headline[which(mini_yday$closeness_headline == max(mini_yday$closeness_headline) &
                                                                                          mini_yday$closeness_headline > 0)], collapse = "; ")
        article_info$closeness_online_headline_yday[ii] <-   max(mini_yday$closeness_headline) 
        article_info$closest_online_text_yday[ii] <- paste(mini_yday$main_text[which(mini_yday$closeness_main_text == max(mini_yday$closeness_main_text) &
                                                                                       mini_yday$closeness_main_text > 0)], collapse = "; ")
        article_info$closeness_online_text_yday[ii] <-   max(mini_yday$closeness_main_text)
      }
    } else {
      article_info$nomatch[ii] <- 1
    }
  } else {
    article_info$online_today[ii] <- 0
    article_info$online_yday[ii] <- 0
  }
}

beep()
article_info <- article_info %>%
  distinct()

saveRDS(article_info, "selenium_factiva/article_info_matched.rds")
#article_info <- readRDS("selenium_factiva/article_info_matched.rds")