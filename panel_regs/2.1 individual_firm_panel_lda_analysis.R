setwd("~/Documents/GitHub/Firm_level_news_analysis")
rm(list=ls())
require(plm)
require(stringr)
require(stargazer)
require(ggplot2)
require(reshape2)
require(urca)

# Import the panel data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/clean_equities_articles.csv", sep = "/")
total_data <- read.csv(import_filename, stringsAsFactors = FALSE)



#################### Bring in the Topics ##########################

### Import the topic distribution data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_lda_30proportions.csv", sep = "/")
topic_props <- read.csv(import_filename, stringsAsFactors = FALSE)

all_data <- merge(total_data, topic_props, by = c("Date", "Code"), all.x = TRUE)
all_data <- pdata.frame(all_data, index = c("Code", "Date"))

all_data[which(is.na(all_data$T0)),c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                         "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                         "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                         "T29")] <- 0

# Import the topic description data
clean_dir <- "~/Documents/DPhil/Clean_Data"
import_filename = paste(clean_dir, "FT/matched/company_lda_30topic_description.csv", sep = "/")
topics <- read.csv(import_filename, stringsAsFactors = FALSE, header = FALSE)


topicnumber <- (length(topics[,1])/2)
wordnum <- 20

topwords <- rep(":", topicnumber)
for (i in seq(1,topicnumber)){
  for(j in seq(2,6)){
    h <- 2*i
    word <- toString(topics[h-1,j])
    topwords[i] <-  paste(topwords[i], word, sep = " ")
  }
}

colnames(all_data[,37:(37+topicnumber -1)]) 
topiclabels <- paste(colnames(all_data[,37:(37+topicnumber -1)]), 
                     topwords, sep = "") 

colnames(all_data) 

fixed_baseline = plm(abs_intra_day ~  mention , data = all_data[which(all_data$Code != "BP.L"),],
            index = c("Code", "Date"), model = "within")
summary(fixed_baseline)



# Intra-day returns and the morning paper's LDA topics
fixed_rel = plm(intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                   + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                   +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                   +T27 + T28 + T29 - 0 , data = all_data,
                   index = c("Code", "Date"), model = "within")
summary(fixed_rel)

fixed_rel_controls = plm(intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                       + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                       +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                       +T27 + T28 + T29 - 0 + lag(open_open,1:4) 
                       + lag(Index_Change, 0:4)
                       + Monday + Tuesday + Wednesday + Thursday, data = all_data,
                       index = c("Code", "Date"), model = "within")
summary(fixed_rel_controls)

fixed_abs = plm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 , data = all_data,
                index = c("Code", "Date"), model = "within")
summary(fixed_abs)

fixed_abs_controls = plm(abs_intra_day ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                        + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                        +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                        +T27 + T28 + T29 - 0 + lag(abs_open_open,1:4) 
                        + lag(Index_abs_Change, 0:4)
                        + Monday + Tuesday + Wednesday + Thursday, data = all_data,
                        index = c("Code", "Date"), model = "within")
summary(fixed_abs_controls)



stargazer(fixed_rel, fixed_rel_controls, fixed_abs, 
          fixed_abs_controls,
          table.placement = "H", df = FALSE, column.sep.width = "2pt",
          covariate.labels = topiclabels, font.size = "tiny",
          dep.var.labels = c(rep("Relative daily change",1),
                             rep("Absolute daily change",1)),
          title = "Intra-day returns and morning news LDA topics")




# Intra-day returns and the next day's paper's LDA topics
fixed_rel = plm(plm::lag(intra_day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 , data = all_data,
                index = c("Code", "Date"), model = "within")
summary(fixed_rel)

fixed_rel_controls = plm(plm::lag(intra_day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                         + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                         +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                         +T27 + T28 + T29 - 0 + lag(open_open,1:5) 
                         + lag(Index_Change, 1:5)
                         + Monday + Tuesday + Wednesday + Thursday, data = all_data,
                         index = c("Code", "Date"), model = "within")
summary(fixed_rel_controls)

fixed_abs = plm(plm::lag(abs_intra_day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                +T27 + T28 + T29 - 0 , data = all_data,
                index = c("Code", "Date"), model = "within")
summary(fixed_abs)

fixed_abs_controls = plm(plm::lag(abs_intra_day,1) ~ T0 + T1 + T2 + T3 + T4 + T5 + T6 + T7
                        + T8 + T9 + T10 + T11 + T12 + T13 + T14 +T15 +T16
                        +T17 + T18 + T19 + T20 + T21 + T22 + T23 + T24 +T25 +T26
                        +T27 + T28 + T29 - 0 + lag(abs_open_open,1:5) 
                        + lag(Index_abs_Change, 1:5)
                        + Monday + Tuesday + Wednesday + Thursday, data = all_data,
                        index = c("Code", "Date"), model = "within")
summary(fixed_abs_controls)



stargazer(fixed_rel, fixed_rel_controls, fixed_abs, 
          fixed_abs_controls, table.placement = "H", df = FALSE, column.sep.width = "2pt",
          covariate.labels = topiclabels, font.size = "tiny",
          dep.var.labels = c(rep("Relative daily change",1),rep("Absolute daily change",1)),
          title = "Intra-day returns and next day news LDA topics")






# Aggregate mentions by firm
require(lattice)
firm_mentions <- aggregate(all_data$mention, list(all_data$Code), sum)



# Tabulate average topic proportion per firm
just_article_days <- all_data[which(all_data$T0 != 0 ),]

mean_props <- aggregate(x = just_article_days[c("T0", "T1","T2","T3","T4","T5","T6","T7","T8",              
                                                "T9","T10", "T11","T12","T13","T14","T15","T16","T17","T18",              
                                                "T19","T20","T21","T22","T23","T24","T25","T26","T27","T28",              
                                                "T29")],
                        FUN = mean, by = list(just_article_days$Company))

sum(mean_props[1,2:31])



top_companies_pertopic <- data.frame(matrix(NA,nrow = topicnumber,ncol = 6))
colnames(top_companies_pertopic) <- c("Topic", "Firm 1", "Firm 2", "Firm 3"
                                      , "Firm 4", "Firm 5")
top_companies_pertopic$Topic <- topiclabels


for (i in 1:topicnumber){
  topic_means <- mean_props[,c("Group.1",paste("T",(i-1), sep = ""))]
  colnames(topic_means) <- c("Company", "Prop")
  topic_means <- topic_means[order( - topic_means$Prop),]
  topic_means$Company <- as.character(topic_means$Company)
  top_companies <- topic_means[1:5,c("Company")]
  top_companies_pertopic[i,2:6] <- top_companies
}

stargazer(as.matrix(top_companies_pertopic), rownames = FALSE, column.sep.width = "2pt",
          table.placement = "H")


ggplot(data=subset(mean_props, T0 > 0.1) , aes(x=Group.1, y=T0)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(y = "Mean proportion", x = "") + 
  theme(text = element_text(size=15),
        axis.text.x = element_text(angle=90, hjust=1))
ggsave("~/Documents/DPhil/sLDA/code/output/unsup_T0_topfirms.png", width = 6, height = 6, dpi = 200)






