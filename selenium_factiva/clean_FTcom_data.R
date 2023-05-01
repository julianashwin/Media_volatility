setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")
rm(list = ls())

library(tidyverse)
library(striprtf)
library(svMisc)
library(progress)


raw_dir <- "/Users/julianashwin/Documents/DPhil/Raw_Data/FT_com/"
clean_dir <- "/Users/julianashwin/Documents/DPhil/Clean_Data/FT/FT_com/"
meta_dir <- "/Users/julianashwin/Documents/GitHub/Media_volatility/selenium_factiva/save_times/"

all_files <- dir(raw_dir)

articles_df <- tibble()
files_df <- tibble(filename = all_files, narticles = NA)

pb <- progress_bar$new(total = nrow(files_df) - 991,
                       format = " cleaning [:bar] :elapsedfull and :percent done, so :eta remaining")
for (ii in 991:nrow(files_df)){
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
    
    article_row <- tibble(file_id = filename, article_id = docid, date = date, headline = headline, 
                          word_count = word_count, main_text = main_text)
    
    articles_df <- bind_rows(articles_df, article_row)
  }
  
}

articles_df %>%
  write.csv(str_c(clean_dir,"FTcom_articles.csv"), row.names = F)


# Import the file metadat
meta_df <- tibble()
meta_files <- dir(meta_dir)
for (meta_file in meta_files){
  file_in <- read_csv(str_c(meta_dir, meta_file))
  meta_df <- rbind(meta_df, file_in)
}

# Import the article info
article_info <- read_csv("selenium_factiva/article_info.csv")






