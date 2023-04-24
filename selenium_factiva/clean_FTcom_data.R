setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")
rm(list = ls())

library(tidyverse)
library(striprtf)
library(svMisc)


raw_dir <- "/Users/julianashwin/Documents/DPhil/Raw_Data/FT_com/"
meta_dir <- "/Users/julianashwin/Documents/GitHub/Media_volatility/selenium_factiva/save_times/"

# Import the file metadat
meta_files <- dir(meta_dir)


all_files <- dir(raw_dir)

articles_df <- tibble()
files_df <- tibble(filename = all_files, narticles = NA)

for (ii in 101:nrow(files_df)){
  progress(ii, progress.bar = TRUE)
  
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
  
  articles_df %>%
    write.csv("selenium_factiva/FTcom_articles.csv", row.names = F)
  
}



