setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())
require(tm)
require(wordcloud)
require(stringr)
require(readtext)
require(stargazer)
require(ggplot2)
require(reshape2)
require(plm)

# import the topic descriptions
topicnumber <- 30
wordnum <- 20

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_abs_slda_30topic_description.csv", sep = "/")
abs_topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)
abs_topics <- abs_topics[,2:(wordnum+1)]

clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_highlow_slda_30topic_description.csv", sep = "/")
highlow_topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)
highlow_topics <- highlow_topics[,2:(wordnum+1)]




#########################################################################
#create pdf wordclouds for each topic
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Absolute sLDA topic clouds
abs_text <- rep("None", topicnumber)
for (i in seq(1,topicnumber,1)){
  if(as.numeric(as.character(abs_topics[(2*i),(wordnum)])) > 0){
    for(j in seq(1,wordnum,1)){
      h <- 2*i
      count <- as.numeric(as.character(abs_topics[h,j]))
      word <- toString(abs_topics[h-1,j])
      words <- toString(rep(word,round((5000*count),0)))
      abs_text[i] <-  paste(abs_text[i], words)
    }
  }
}
for (i in seq(1,topicnumber)){
  x <- paste0(figure_location, "/30topic_slda/company_abs_slda_30topic", (i-1),"_cloud.pdf")
  pdf(x)
  wordcloud(abs_text[i], scale=c(6,1),max.words=wordnum, random.order=FALSE, 
            rot.per=0.4, colors = brewer.pal(8, "Blues"),vfont=c("serif","plain"))
  #line <- readline() # 
  dev.off()
}


# Relative sLDA topic clouds
highlow_text <- rep("None", topicnumber)
for (i in seq(1,topicnumber,1)){
  if(as.numeric(as.character(highlow_topics[(2*i),(wordnum)])) > 0){
    for(j in seq(1,wordnum,1)){
      h <- 2*i
      count <- as.numeric(as.character(highlow_topics[h,j]))
      word <- toString(highlow_topics[h-1,j])
      words <- toString(rep(word,round((5000*count),0)))
      highlow_text[i] <-  paste(highlow_text[i], words)
    }
  }
}
for (i in seq(1,topicnumber)){
  x <- paste0(figure_location, "/30topic_slda/company_highlow_slda_30topic", (i-1),"_cloud.pdf")
  pdf(x)
  wordcloud(highlow_text[i], scale=c(6,1),max.words=wordnum, random.order=FALSE, 
            rot.per=0.4, colors = brewer.pal(8, "Blues"),vfont=c("serif","plain"))
  #line <- readline() # 
  dev.off()
}




### Import the phi estimates at each EM step:
source_location <- "~/Documents/DPhil/Clean_Data/FT/matched"
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

# Temporary phi estimates
phi_filename <- paste0(source_location, "/company_phi_temp.csv")
phi <- read.csv(phi_filename)
phi_long <- melt(phi, id = "X")

ggplot(data=phi_long, aes(x=X, y=value, colour=variable)) +
  geom_line() + theme(legend.position="none") + labs(x = "EM iterations", 
                                                     y = expression(paste("Values of ", phi)))
#export_filename <- paste0(figure_location, "/company_temp_30phi_converge.png")
#ggsave(export_filename, width = 8, height = 4, dpi = 200)


# Highlow sLDA phi
phi_filename <- paste0(source_location, "/company_highlow_slda_30phi.csv")
phi <- read.csv(phi_filename)
cutoff <- which(phi$X0 == 0)[1]-1
phi <- phi[1:cutoff,] # Adjust here to cut out the unused iterations
phi_long <- melt(phi, id = "X")

ggplot(data=phi_long, aes(x=X, y=value, colour=variable)) +
  geom_line() + theme(legend.position="none") + labs(x = "EM iterations", 
    y = expression(paste("Values of ", phi)))
export_filename <- paste0(figure_location, "/company_highlow_30phi_converge.png")
ggsave(export_filename, width = 8, height = 4, dpi = 200)


# Absolute sLDA phi
phi_filename <- paste0(source_location, "/company_abs_slda_30phi.csv")
phi <- read.csv(phi_filename)
cutoff <- which(phi$X0 == 0)[1]-1
phi <- phi[1:cutoff,]
phi_long <- melt(phi, id = "X")

ggplot(data=phi_long, aes(x=X, y=value, colour=variable)) +
  geom_line() + theme(legend.position="none") + labs(x = "EM iterations", 
                                                     y = expression(paste("Values of ", phi)))
export_filename <- paste0(figure_location, "/company_abs_30phi_converge.png")
ggsave(export_filename, width = 8, height = 4, dpi = 200)




######################################################################
