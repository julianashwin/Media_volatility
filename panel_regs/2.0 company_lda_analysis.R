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
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_lda_30topic_description.csv", sep = "/")
topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)

topicnumber <- (length(topics[,1])/2)
wordnum <- 20


#########################################################################
#create pdf wordclouds for each topic
figure_location <- "~/Documents/DPhil/Firm_level_news/figures"

topics <- topics[,2:(wordnum+1)]
text <- rep("None", topicnumber)
for (i in seq(1,topicnumber,1)){
  if(as.numeric(as.character(topics[(2*i),(wordnum)])) > 0){
    for(j in seq(1,wordnum,1)){
      h <- 2*i
      count <- as.numeric(as.character(topics[h,j]))
      word <- toString(topics[h-1,j])
      words <- toString(rep(word,round((5000*count),0)))
      text[i] <-  paste(text[i], words)
    }
  }
}

for (i in seq(1,topicnumber)){
  x <- paste0(figure_location, "/30topic_lda/company_lda_30topic", (i-1),"_cloud.pdf")
  pdf(x)
  wordcloud(text[i], scale=c(6,1),max.words=wordnum, random.order=FALSE, 
            rot.per=0.4, colors = brewer.pal(8, "Blues"),vfont=c("serif","plain"))
  #line <- readline() # 
  dev.off()
}


#########################################################################



### Import the topic distribution data
topic_props <- read.csv("companyhead_lda_30proportions.csv", stringsAsFactors = FALSE)


####### Merge with the equity level data

# Import the data
ner_data <- read.csv("~/Documents/DPhil/sLDA/code/equity_prices_collapsedarticles.csv", stringsAsFactors = FALSE)
ner_data <- pdata.frame(ner_data, index = c("Code", "Date"))


total_data <- read.csv("~/Documents/DPhil/sLDA/code/equity_prices_collapsedheadlines.csv", stringsAsFactors = FALSE)
total_data <- pdata.frame(total_data, index = c("Code", "Date"))

all(ner_data$Date == total_data$Date)
all(ner_data$Code == total_data$Code)
total_data$ner_mention <- ner_data$mentioned
total_data$headline_mention <- total_data$mentioned

# Clean it up a bit
# Multiply the abs_Change variable by 100
total_data$abs_Change_1day <- total_data$abs_Change_1day*100
total_data$abs_Change_2day <- abs(total_data$Change_2day)*100

# Divide volume by 100000
total_data$Volume <- total_data$Volume/100000


all_data <- merge(total_data, topic_props, by = c("Date", "Code"), all.x = TRUE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))

no_out <- all_data[which(all_data$abs_Change_1day < 50),]


fixed = plm(abs_Change_1day ~ headline_mention, data = no_out,index = c("Code", "Date"),
            model = "within")
summary(fixed)

no_out[which(is.na(no_out$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                 "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                 "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                 "T29")] <- 0 

fixed1_both = plm(abs_Change_1day ~  ner_mention + mentioned, 
                  data = no_out, index = c("Code", "Date"), 
                  model = "within")
summary(fixed1_both)

fixed = plm(lag(Change_1day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
            + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
            +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
            +T27 + T28 + T29- 0 , data = no_out,
            index = c("Code", "Date"), model = "within")
summary(fixed)
fixed_abs = plm(lag(abs_Change_1day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29- 0 , data = no_out,
                index = c("Code", "Date"), model = "within")
summary(fixed_abs)




### Import the phi estimates at each EM step:
source_location <- "~/Documents/DPhil/Clean_Data/FT/matched"

phi_filename <- paste0(source_location, "/company_phi_temp.csv")
phi <- read.csv(phi_filename)
phi_long <- melt(phi, id = "X")

ggplot(data=phi_long, aes(x=X, y=value, colour=variable)) +
  geom_line() + theme(legend.position="none") + labs(x = "EM iterations", 
    y = expression(paste("Values of ", phi)))
ggsave("output/companyhead_15phi_converge.png", width = 8, height = 4, dpi = 200)




######################################################################
