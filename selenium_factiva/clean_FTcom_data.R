setwd("/Users/julianashwin/Documents/GitHub/Media_volatility/")
rm(list = ls())

library(tidyverse)
library(striprtf)


raw_dir <- "/Users/julianashwin/Documents/DPhil/Raw_Data/FT_com/"
meta_dir <- "/Users/julianashwin/Documents/GitHub/Media_volatility/selenium_factiva/save_times/"

# Import the file metadat
meta_files <- dir(meta_dir)


all_files <- dir(raw_dir)

articles_df <- tibble()
files_df <- tibble(filename = all_files, narticles = NA)
filename <- all_files[3200]
pb = txtProgressBar(min = 1, max = nrow(files_df))
for (ii in 1:nrow(files_df)){
  setTxtProgressBar(pb,ii)
  
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
    word_count <- which(str_detect(article, "^[0-9]+ words$"))
    copyright <- which(str_detect(article, "^\\(c\\) [0-9]+"))
    
    article[1]
    article[word_count]
    article[copyright]
    
  }
  
}