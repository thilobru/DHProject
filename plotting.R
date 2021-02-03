# Install
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes

#install.packages("plyr")
#install.packages("lubridate")
# Load
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

library(dplyr)
library(plyr)
library(ggplot2)
library(lubridate)
theme_set(theme_bw())


getwd()
setwd("C:/Users/thilo/OneDrive/Dokumente/UniversitätLeipzig/1.SemesterDataScience/DH/Projekt")


data <- read.delim("data/44444TweetsHydratedScored.tsv", header = TRUE, sep = "\t", quote = "\"",
                   dec = ".", fill = TRUE, comment.char = "")
data <- read.delim("data/500000TweetsHydratedScored.tsv", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "")
data$date <- as.Date(data$date, "%Y-%m-%d %H:%M:%S")
typeof(data$date)

#lst <- split(data,data$fulltext)
#ftextList <- Corpus(split(data,data$fulltext))
#lemmaList <- Corpus(data$lemma)

redata <- aggregate(data[, 2:9], list(data$date), mean)



ggplot(data, aes(date, fill = "Daily density")) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
    binwidth=.5,
    colour="grey", fill="white") +
  #geom_density(alpha = 0.2) +
  #geom_vline(max(count(data, vars = "date")$date)) +
  scale_x_date()
ggsave("plots/tweetsPerDay.png")

ggplot(redata, aes(x=Group.1)) +
  geom_line(aes(y=AfinnSentiment), color="red") +
  geom_line(aes(y=AfinnDefaultSentiment), color="blue") #+
  #geom_vline(aes(xintercept=subset(redata, redata$AfinnSentiment == min(redata$AfinnSentiment),
  #                                 select=c(Group.1))),   # Ignore NA values for mean
  #                      color="red", linetype="dashed", size=1)
ggsave("plots/AfinnsOverlayed.png")

#######################
#####VALUE RESULTS#####
#######################
res <- cor.test(data$AfinnSentiment, data$AfinnDefaultSentiment, 
                method = "pearson")
res
cor.test(data$AfinnSentiment, data$NRCValenceMean+data$NRCArousalMean, 
         method = "pearson")
cov(data$AfinnSentiment, data$NRCValenceMean+data$NRCArousalMean)

max(data$AfinnSentiment)
min(data$AfinnSentiment)
subset(data, data$AfinnSentiment == max(data$AfinnSentiment),
       select=c(fulltext))
subset(data, data$AfinnSentiment == min(data$AfinnSentiment),
       select=c(fulltext))

subset(data, data$NRCValenceMean == max(data$NRCValenceMean),
       select=c(fulltext))
subset(data, data$NRCValenceMean == min(data$NRCValenceMean) & data$NRCValenceMean > 0,
       select=c(fulltext))
subset(data, data$NRCArousalMean == max(data$NRCArousalMean),
       select=c(fulltext))
subset(data, data$NRCArousalMean == min(data$NRCArousalMean) & data$NRCArousalMean > 0,
       select=c(fulltext))

max(data$NRCArousalMean*data$NRCValenceMean)
subset(data, data$NRCArousalMean+data$NRCValenceMean == max(data$NRCArousalMean+data$NRCValenceMean),
       select=c(fulltext,lemma))

######################
########PLOTS#########
######################
ggplot(redata, aes(x = Group.1, group=month(Group.1),y=AfinnSentiment)) + 
  geom_boxplot()

#http://www.cookbook-r.com/Graphs/Plotting_distributions_(ggplot2)/
ggplot(data, aes(x=AfinnSentiment)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=2,
                 colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666",adjust = 3) +  
  geom_vline(aes(xintercept=mean(AfinnSentiment, na.rm=T)),
           color="red", linetype="dashed", size=1) +
  #geom_boxplot(alpha = .01)

ggplot(data, aes(x=AfinnSentiment)) +
  geom_density() +
  geom_vline(aes(xintercept=mean(AfinnSentiment, na.rm=T)),
    color="red",linetype="dashed", size=1)

ggplot(redata, aes(x = Group.1, y=AfinnSentiment)) + 
  geom_smooth(se = TRUE, method=loess, color="red") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")
  #geom_point(alpha = 0.5) + 
  #geom_quantile() +
  #scale_y_continuous(trans='log2')
  #labs(title='sentiment plot', x='Time', y='Sentiment') #+ # customizing labels
  #ylim(-1,1)
ggsave("plots/afinnOverTime.png")
ggplot(subset(data, grepl('Pelosi', data$fulltext, fixed = TRUE)), aes(x = date, y=AfinnSentiment)) + 
  geom_smooth(se = TRUE, method=loess, color="red") +
  scale_x_date(date_labels="%b %y",date_breaks  ="1 month")

theme_set(theme_bw())

# plot
g <- ggplot(data, aes(group=month(date), x = date, y= AfinnSentiment))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="AfinnSentiment",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

ggplot(data, aes(x=date, y=AfinnSentiment)) + 
  geom_point(alpha = 0.5) + 
  geom_smooth(method=loess, color="grey")
  #ylim(-10,10)

###### NRC VAD
ggplot(data, aes(x=NRCValence,y=NRCArousal)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method=lm)

ggplot(data, aes(x=NRCValenceMean,y=NRCArousalMean)) +
  geom_point(alpha = 0.1) +
  geom_smooth(method=lm) +
  geom_vline(aes(xintercept=mean(NRCValenceMean, na.rm=T)),   # Ignore NA values for mean
            color="red", linetype="dashed", size=1)+
  geom_hline(aes(yintercept=mean(NRCArousalMean, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)
