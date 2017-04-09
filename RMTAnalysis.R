##############################################################
#
# RMTAnalysis.R
#
# The purpose of this script is to analyze the clean data for RMT
# acquired from popular review and travel websites. 
# The script performs a time series analysis of the customer ratings, 
# and a text analysis of the customer reviews
#
# Author: Antoine Beauchamp
# Edited: April 7th, 2017
# Created: April 5th, 2017
#
##################################################################

rm(list=ls())

######################## FUNCTION DEFINITIONS ###############################


# Function: WordCloudAnalysis
#
# This function takes a dataset containing customer reviews and prepares the data for a word cloud
# analysis
# Input: Data frame containing Review variable that has customer reviews
# Output: List containing text analytics properties of reviews such as Corpus, Term Document Matrix, and word freqneucies.
#

TextAnalysis <- function(dataset, use.sentences=TRUE){
  
  #Remove "The" or "the"  from reviews, since the tm_map() function below doesn't do it. 
  dataset$Reviews <- gsub("[Tt]he", "", dataset$Reviews)
  
  #Options to split review text into individual sentences. This improves the use of the findAssocs() function later on.
  if(use.sentences==TRUE){
    dataCorpus <- get_sentences(dataset$Reviews) %>% VectorSource %>% Corpus()
  }
  else {
    #Create corpus from the full text data
    dataCorpus <- dataset$Reviews %>% VectorSource() %>% Corpus()
  }
  
  #Remove punctuation and english stopwords from text data
  dataCorpus <- dataCorpus %>% 
    tm_map(removePunctuation) %>% 
    tm_map(removeWords, stopwords("english")) %>% 
    tm_map(content_transformer(tolower))
  
  #Create term document matrix and convert to matrix class.
  TDM <- TermDocumentMatrix(dataCorpus)
  TDM_m <- TDM %>% as.matrix()
  
  #Compute word frequencies from TDM. 
  wFreqs = sort(rowSums(TDM_m), decreasing=TRUE)
  
  return(list("Corpus" = dataCorpus, "TDM"=TDM, "wordFreq"=wFreqs))
}

# Function: revCount
#
# This function takes a dataset containing customer reviews and prepares the data for a word cloud
# analysis
# Input: Vector of regex to get words for data, vector of customer reviews, word frequencies
# Output: Data frame with words, review counts and fractions
#
revCount <- function(grepVec, revVec,wordFreq){
  percVec <- numeric(length(grepVec))
  numVec <- numeric(length(grepVec))
  for(i in 1:length(grepVec)){
    numVec[i] <- length(revVec[grepl(grepVec[i],revVec)])
    percVec[i] <- length(revVec[grepl(grepVec[i], revVec)])/length(revVec)
  }
  df <- data_frame("word"=names(wordFreq[1:length(grepVec)]),
                   "reviewCount"=round(numVec,2),
                   "reviewFraction"=round(percVec,2))
  return(df)
}


######################## HEADER ###############################

#Set working directory
CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject/CapstoneDeliverables"
setwd(CapstoneDir)

#Load libraries
library(readr)
library(dplyr)
library(tidyr)
library(tm)
library(wordcloud)
library(ggplot2)
library(syuzhet)

#Read in clean data
CapstoneDF <- read_csv("./Data/CapstoneCleanData.csv")

#Summary of data frame
str(CapstoneDF)
summary(CapstoneDF)
dim(CapstoneDF)

#Theme to generate plots
mytheme <-   theme(panel.grid.major = element_line(size=2), 
                   panel.background = element_rect(fill="gray92", colour = "black", size = 2.0),
                   axis.line = element_line(colour = "black", size=1.0), 
                   axis.text = element_text(face="bold", size=28),
                   axis.title = element_text(size=36),
                   axis.title.y = element_text(margin = margin(0,20,0,0)),
                   axis.title.x = element_text(margin=margin(20,0,0,0)),
                   plot.margin = margin(t=1, r=1, b=1, l=1), 
                   plot.background = element_rect(fill="#F5F5F5"), 
                   legend.background = element_rect(fill="#F5F5F5"), 
                   legend.title = element_text(face="bold", size=28), 
                   legend.text = element_text(size=24))


########################## DATA WRANGLING #####################################

#Create Quarters variable
CapstoneDF <- CapstoneDF  %>% mutate(Quarters = quarters.Date(Dates))

#Separate date variable into Year, Month, Day
CapstoneDF <- CapstoneDF %>% separate(Dates, c("Year","Month","Day"))

#Create variable that describes month and year. 
tempdf <- CapstoneDF %>% unite("YearMonth", Year, Month, sep="-")
CapstoneDF$YearMonth <- tempdf$YearMonth

#Create variable that describes quarters and years
tempdf <- CapstoneDF %>% unite("YearQuarters", Year, Quarters, sep="-")
CapstoneDF$YearQuarters <- tempdf$YearQuarters

#Convert Website and Quarters to factor class
CapstoneDF$Website <- factor(CapstoneDF$Website)
CapstoneDF$Quarters <- factor(CapstoneDF$Quarters)

#Create new variables

#Review count by year
t1 <- CapstoneDF %>% group_by(Year) %>% summarise(countYear=n())
CapstoneDF <- left_join(CapstoneDF,t1, by="Year")

#Review count by quarters
t2 <- CapstoneDF %>% group_by(YearQuarters) %>% summarise(countQuarters=n())
CapstoneDF <- left_join(CapstoneDF,t2, by="YearQuarters")

#Review count by month
t3 <- CapstoneDF %>% group_by(YearMonth) %>% summarise(countMonth=n())
CapstoneDF <- left_join(CapstoneDF,t3, by="YearMonth")

head(CapstoneDF[-1])


########################## TIME SERIES ANALYSIS #####################################


#What is full mean of ratings? 
mean(CapstoneDF$Ratings)
CapstoneDF$RatingsAvg <- rep(mean(CapstoneDF$Ratings), length(CapstoneDF$Ratings))


ht=10
wd=20
#Ratings aggregated by Year
timeseries_year <- ggplot(CapstoneDF, aes(x=Year, y=Ratings)) + 
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed",
            colour="black", alpha=0.5, size=2) +
  stat_summary(aes(size=countYear), fun.y=mean, geom="point", col="firebrick", alpha=1.0) +
  scale_size_continuous(range=c(15,30), guide=guide_legend(title="Review Count")) +
  coord_cartesian(ylim=c(3.0,4.5)) + mytheme + ylab(label="Average Ratings")
timeseries_year
ggsave(filename="./Plots/global_YearRatings.png",timeseries_year, width=wd, height=ht)


#Ratings aggregated by Year and Quarters
timeseries_quarters <- ggplot(CapstoneDF, aes(x=YearQuarters, y=Ratings,col=Year)) + 
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed",
            colour="black", alpha=0.5, size=2) +
  stat_summary(aes(size=countQuarters), fun.y=mean, geom="point", alpha=0.9) + 
  coord_cartesian(ylim=c(3.0,4.5)) +
  scale_size_continuous(range=c(15,30), guide = guide_legend(title="Review Count")) +
  theme(axis.text.x =element_text(angle=90)) +
  labs(x="Yearly Quarters", y="Average Ratings") +
  mytheme + theme(legend.position = "right", legend.justification = "center")+
  guides(col = guide_legend(override.aes = list(size=15)))
timeseries_quarters
ggsave(filename="./Plots/global_YearQuartersRatings.png",timeseries_quarters,width=wd,height=ht)


#Ratings aggregated by Year and Month
timeseries_month <- ggplot(CapstoneDF, aes(x=YearMonth, y=Ratings, col=Year)) +
  geom_line(aes(y=RatingsAvg, group=1), linetype="dashed",
            colour="black", alpha=0.5, size=2) +
  stat_summary(aes(size=countMonth),fun.y=mean, geom="point", alpha=0.8) +
  coord_cartesian(ylim=c(2.5,4.5)) + 
  scale_size_continuous(range=c(15,25), guide = guide_legend(title="Review Count")) +
  theme(axis.text.x =element_text(angle=90, size=24)) + 
  mytheme + labs(x="Month", y="Average Ratings") + theme(legend.position = "right", legend.justification = "bottom")+
  guides(col = guide_legend(override.aes = list(size=10)))
timeseries_month
ggsave(filename="./Plots/global_YearMonthRatings.png",timeseries_month, width=wd, height=ht)



ht=10
wd=10
#Cumulative ratings distribution
t <- CapstoneDF %>% group_by(ceiling(Ratings)) %>% summarise(count = n())
t$count <- round(t$count/dim(CapstoneDF)[1],2)
names(t)[1] <- "Ratings"
revFractionHist <- ggplot(CapstoneDF, aes(x=ceiling(Ratings))) + 
  geom_bar(aes(y=..count../sum(..count..)), fill="firebrick", col="grey30", alpha=0.9, size=1.5) +
  mytheme + 
  labs(x="Ratings", y="Review Fraction") + 
  geom_text(data=t, aes(label=count, y=count), vjust=2, fontface="bold", size=10) +
  theme(panel.grid.minor=element_blank()) + theme(plot.margin = margin(10, 10, 10, 10))
revFractionHist
ggsave(filename="./Plots/global_RatingsHist.png", revFractionHist, width=wd, height=ht)



ht=10
wd=20
#Yearly ratings histogram
ratingYearHist <- ggplot(CapstoneDF, aes(ceiling(Ratings), y=..density.., col=Year)) +
  geom_histogram(binwidth=1, alpha=1.0, fill="grey60", size=2.0) +
  facet_grid(.~Year) + mytheme + labs(x="Ratings", y="Review Fraction") +
  mytheme +
  theme(strip.text = element_text(face = "bold", size=24),
        strip.background = element_rect(colour = "black"), 
        legend.position = "none", 
        panel.grid.minor=element_blank(), 
        plot.margin = margin(10, 10, 10, 10))
ratingYearHist
ggsave(filename="./Plots/global_RatingsYearHist_dens.png", ratingYearHist, width=wd, height=ht)



temp <- CapstoneDF %>% group_by(Website) %>% summarise(average = mean(Ratings))
temp

wd=10.0
ht=10.0
#Website ratings histogram
ratingWebsiteHist <- ggplot(temp, aes(x=Website, y=average, fill=Website)) + 
  geom_bar(stat = "identity", col="grey30", size=1.5, alpha=0.9) + 
  coord_cartesian(ylim=c(0,6)) + 
  geom_text(aes(label=round(average,2)), vjust=2.5, fontface="bold", size=10) +
  mytheme + theme(legend.position = "none", panel.grid.major.x=element_blank(), panel.grid.major.y=element_line(size = 2), panel.grid.minor.y=element_line(size = 2)) + labs(x="Websites", y="Ratings Average") +
  scale_fill_manual(values = brewer.pal(9, "Reds")[4:9])
ratingWebsiteHist
ggsave(filename="./Plots/RatingsWebsiteMean.png", ratingWebsiteHist, width=wd, height=ht)



#Build a plot that describes the time series of the percent of individual star ratings

#Vector of year values
yearVec <- sort(unique(as.numeric(CapstoneDF$Year)))

#Build temporary data frame and round up Ratings
CapstoneDF_t <- CapstoneDF
CapstoneDF_t$Ratings <- ceiling(CapstoneDF_t$Ratings)

#Build data frames for rating counts by year
d1 <- CapstoneDF_t %>% group_by(Year,Ratings) %>% summarise(countRatings=n())
d2 <- CapstoneDF_t %>% group_by(Year) %>% summarise(countYear=n())

#Join data frames for ratings count and year counts
percentData <- left_join(d1,d2, by="Year")
percentData$Ratings <- ceiling(percentData$Ratings)

#Create observations for instances when no ratings of that number of stars have been observed
vec2 <- NULL
for(i in 1:length(yearVec)){
  vec <- percentData$Ratings %>% subset(percentData$Year == yearVec[i])
  if(!(1 %in% vec)){
    yearCount <- subset(percentData$countYear, percentData$Year==yearVec[i])[1]
    vec2 <- data.frame(Year=as.character(yearVec[i]), Ratings=as.numeric(1), countRatings=as.integer(0), countYear=as.integer(yearCount))
    percentData <- (bind_rows(vec2, as.data.frame(percentData)))
  }
}

#Calculate ratings percentages
percentData <- percentData %>% arrange(Ratings) %>%  mutate(percentRatings = countRatings/countYear)
#Select columns
percentData <- percentData %>% select(Year,Ratings,percentRatings, countYear) %>% arrange(Year,Ratings)
#Treat Ratings as ordinal variables
percentData$Ratings <- factor(percentData$Ratings)


ht=10
wd=20
#Ratings fractions time series
ratingsFrac <- ggplot(percentData, aes(x=Year, y=percentRatings, group=Ratings, col=Ratings)) + 
  geom_smooth(data=subset(percentData, percentData$Ratings==1), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==2), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==3), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==4), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==5), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  geom_point(aes(col=Ratings,  size=countYear), alpha=0.9) + 
  scale_size_continuous(range=c(10,25), guide = guide_legend(title = "Review Count")) +
  mytheme + labs(y="Review Fraction") +
  theme(panel.grid.minor=element_blank(), 
        plot.margin = margin(10, 10, 10, 10)) +
  guides(col = guide_legend(override.aes = list(size=10))) +
  scale_color_brewer(palette = "Dark2")
ratingsFrac
ggsave(filename="./Plots/global_percentRatings.png", ratingsFrac, width=wd, height=ht)

#Make plots that highlight different ratings series
t <- ggplot(percentData, aes(x=Year, y=percentRatings, group=Ratings, col=Ratings)) + 
  geom_smooth(data=subset(percentData, percentData$Ratings==1), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==2), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  geom_smooth(data=subset(percentData, percentData$Ratings==3), method="lm", se=T, formula=y~poly(x,1, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==4), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") + 
  geom_smooth(data=subset(percentData, percentData$Ratings==5), method="lm", se=T, formula=y~poly(x,2, raw=TRUE), linetype="dashed", size=1.0, alpha=0.15, col="black") +
  scale_size_continuous(range=c(10,25), guide = guide_legend(title = "Review Count")) +
  mytheme + labs(y="Review Fraction") +
  theme(panel.grid.minor=element_blank(), 
        plot.margin = margin(10, 10, 10, 10)) +
  guides(col = guide_legend(override.aes = list(size=10))) +
  scale_color_brewer(palette = "Dark2")

#1 star
ratingsFrac_1 <- t + geom_point(data=subset(percentData, Ratings!="1"), aes(col=Ratings, size=countYear), alpha=0.2) +
  geom_point(data=subset(percentData, Ratings=="1"), aes(col=Ratings, size=countYear), alpha=0.9) 
ggsave(filename="./Plots/global_percentRatings_1.png", ratingsFrac_1, width=wd, height=ht)
  
#2 star
ratingsFrac_2 <- t + geom_point(data=subset(percentData, Ratings!="2"), aes(col=Ratings, size=countYear), alpha=0.2) +
  geom_point(data=subset(percentData, Ratings=="2"), aes(col=Ratings, size=countYear), alpha=0.9) 
ggsave(filename="./Plots/global_percentRatings_2.png", ratingsFrac_2, width=wd, height=ht)

#3 star
ratingsFrac_3 <- t + geom_point(data=subset(percentData, Ratings!="3"), aes(col=Ratings, size=countYear), alpha=0.2) +
  geom_point(data=subset(percentData, Ratings=="3"), aes(col=Ratings, size=countYear), alpha=0.9) 
ggsave(filename="./Plots/global_percentRatings_3.png", ratingsFrac_3, width=wd, height=ht)

#4 star
ratingsFrac_4 <- t + geom_point(data=subset(percentData, Ratings!="4"), aes(col=Ratings, size=countYear), alpha=0.2) +
  geom_point(data=subset(percentData, Ratings=="4"), aes(col=Ratings, size=countYear), alpha=0.9) 
ggsave(filename="./Plots/global_percentRatings_4.png", ratingsFrac_4, width=wd, height=ht)

#5 star
ratingsFrac_5 <- t + geom_point(data=subset(percentData, Ratings!="5"), aes(col=Ratings, size=countYear), alpha=0.2) +
  geom_point(data=subset(percentData, Ratings=="5"), aes(col=Ratings, size=countYear), alpha=0.9) 
ggsave(filename="./Plots/global_percentRatings_5.png", ratingsFrac_5, width=wd, height=ht)



#Let's check the numbers on our models to see if they're actually reasonable
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==1)))
summary(lm(percentRatings ~ poly(as.numeric(Year),1), data=subset(percentData, percentData$Ratings==2)))
summary(lm(percentRatings ~ poly(as.numeric(Year),1), data=subset(percentData, percentData$Ratings==3)))
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==4)))
summary(lm(percentRatings ~ poly(as.numeric(Year),2), data=subset(percentData, percentData$Ratings==5)))

#Most R-squared values are quite high, which is good. Notice that R-squared is low for Ratings==3. This is
# because the data has almost no discernible slope. This means that it can be approximated well by using
# the baseline model. R-squared is a measure of how well a function describes the data, compared to the baseline
# model. If the baseline model works fine, R-squared will be close to 0. This is what is happening with Ratings==3. 



########################## TEXT ANALYSIS #####################################


## ============================== GLOBAL ============================== 

#Word analysis
Capstone_wA <- TextAnalysis(CapstoneDF)

#Most common words
head(Capstone_wA$wordFreq, n=20)

#Generate wordcloud
png("globalwordcloud.png", width=4, height=4, units="in", res=300, bg="transparent")
wordcloud(Capstone_wA$Corpus, max.word=100, random.order=F, scale=c(2,0.5), colors=brewer.pal(9, "Blues")[5:9])
dev.off()

#regex for most common words
grepvec <- c("[Ff]ood", "[Gg]ood", "[Gg]reat", "[Ss]ervice", 
             "[Rr][Ee][Dd][Ss]", "[Pp]lace", "[Mm]enu", 
             "[Bb]ack", "[Nn]ice", "[Tt]ime")

#create data frame for common word histogram
df_global <- revCount(grepvec, CapstoneDF$Reviews, Capstone_wA$wordFreq)

#Plot histogram of review fractions for common words
ggplot(df_global, aes(x=word, y=reviewFraction)) + 
  geom_bar(fill="skyblue3", col="navy", stat="identity", alpha=0.9) + 
  geom_text(aes(label=reviewFraction), vjust=2, fontface="bold") + 
  theme(axis.text.x =element_text(angle=90)) + 
  mytheme + 
  labs(x="Word", y="Review Fraction")

#Word associations
findAssocs(Capstone_wA$TDM, "food", .18)
findAssocs(Capstone_wA$TDM, "service", 0.15)
findAssocs(Capstone_wA$TDM, c("good","great"), c(0.15, 0.10))
findAssocs(Capstone_wA$TDM, "reds", 0.20)
findAssocs(Capstone_wA$TDM, c("atmosphere", "decoration"), c(.11, 0.10))
findAssocs(Capstone_wA$TDM, "music", 0.20)
findAssocs(Capstone_wA$TDM, "decor", 0.15)
findAssocs(Capstone_wA$TDM, "menu", 0.15)


# Global sentiment analysis 
datasentiments <- get_nrc_sentiment(CapstoneDF$Reviews)
sentimentTotals <- data.frame(colSums(datasentiments))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ht=10
wd=17.5
#Sentiment analysis plot
sentiment <-ggplot(data = sentimentTotals[1:8,], aes(x = sentiment, y = count)) +
  #geom_bar(fill="skyblue3", col="navy", stat="identity", alpha=0.9) +
  geom_bar(aes(fill=sentiment), stat="identity", col="navy", alpha=0.9, size=1.5) + 
  geom_text(aes(label=count), vjust=2, size=10, fontface="bold") +
  theme(legend.position = "none", panel.grid.major.x=element_blank()) +
  xlab("Sentiment") + ylab("Total Count") + 
  mytheme +
  theme(panel.grid.minor=element_blank(), 
        plot.margin = margin(10, 10, 10, 10))
sentiment
ggsave(filename="./Plots/sentimentAnalysis.png", sentiment, width=wd, height=ht)




## ============================== POSITIVE/NEGATIVE REVIEWS ============================== 


#Break data into positive and negative reviews
CapstoneDF_pos <- CapstoneDF %>% subset(Ratings > 3)
CapstoneDF_neg <- CapstoneDF %>% subset(Ratings <= 3)

#Perform text analytics on pos and neg reviews
CapstonePos_wA <- TextAnalysis(CapstoneDF_pos)
CapstoneNeg_wA <- TextAnalysis(CapstoneDF_neg)


#Most common words in positive reviews
head(CapstonePos_wA$wordFreq, n=10)
#Most common words in negative reviews
head(CapstoneNeg_wA$wordFreq, n=10)



#Generate wordclouds
png("wordcloud_pos.png", width=4, height=4, units="in", res=300, bg="transparent")
wordcloud(CapstonePos_wA$Corpus, max.word=100, random.order=F, scale=c(2,0.5), colors=brewer.pal(9, "GnBu")[5:9])
dev.off()

png("wordcloud_neg.png", width=4, height=4, units="in", res=300, bg="transparent")
wordcloud(CapstoneNeg_wA$Corpus, max.word=100, random.order=F, scale=c(2,0.5), colors=brewer.pal(9, "OrRd")[3:9])
dev.off()

#Create data frame for common word review percentage, positive reviews
grepVec_pos <- c("[Ff]ood", "[Gg]reat", "[Gg]ood", "[Ss]ervice", 
                 "[Rr][Ee][Dd][Ss]", "[Mm]enu", "[Pp]lace", 
                 "[Rr]eally", "[Bb]ack", "[Dd]elicious", "[Ss]taff",
                 "[Ff]riendly", "[Nn]ice","[Tt]ime", "[Ee]xcellent")
df_pos <- revCount(grepVec_pos, CapstoneDF_pos$Reviews, CapstonePos_wA$wordFreq)

#Create data frame for common word review percentage, negative reviews
grepVec_neg <- c("[Ff]ood", "[Gg]ood", "[Ss]ervice", "[Pp]lace",
                 "[Jj]ust", "[Gg]reat","[Tt]able","[Rr]estaurant", 
                 "[Tt]ime", "[Nn]ice", "[Rr][Ee][Dd][Ss]", "[Bb]ack",
                 "[Mm]enu", "[Oo]rdered", "[Cc]ame")
df_neg <- revCount(grepVec_neg, CapstoneDF_neg$Reviews, CapstoneNeg_wA$wordFreq)

#Add categorical variables
df_pos$reviewClass <- rep("Positive", dim(df_pos)[1])
df_neg$reviewClass <- rep("Negative", dim(df_neg)[1])

#Create a data frame that only includes words that are in both positive and negative reviews
df1 <- df_pos[df_pos$word %in% df_neg$word,]
df2 <- df_neg[df_neg$word %in% df_pos$word,]
df_inBoth <- rbind(df1,df2)


#Plot histogram of review fractions for common words in positive and negative reviews
#Group aesthetic is needed to ensure that labels are placed on correct bars
ggplot(df_inBoth, aes(x=word, y=reviewFraction, group=reviewClass)) + 
  geom_bar(aes(fill=reviewClass),stat="identity", position="dodge", col="navy", alpha=0.8, size=0.6) +
  geom_text(aes(label=reviewFraction), position=position_dodge(0.9), angle=90,hjust=1.5, size=3, fontface="bold") +
  theme(axis.text.x =element_text(angle=90)) + mytheme +
  labs(x="Word", y="Review Fraction") + guides(fill=guide_legend(title="Review Class"))

#Create data frame that includes words that are exclusive to positive or negative reviews
df1 <- df_pos[!(df_pos$word %in% df_neg$word),]
df2 <- df_neg[!(df_neg$word %in% df_pos$word),]
df_ninBoth <- rbind(df1,df2)

#Plot histogram of review fractions for words exclusive to pos. or neg. reviews
ggplot(df_ninBoth, aes(x=word, y=reviewFraction)) + 
  geom_bar(aes(fill=reviewClass),stat="identity", position="dodge", col="navy", alpha=0.8, size=0.6) +
  geom_text(aes(label=reviewFraction), vjust=2, fontface="bold") +
  theme(axis.text.x =element_text(angle=90)) + mytheme +
  labs(x="Word", y="Review Fraction") + guides(fill=guide_legend(title="Review Class"))


## Word associations

#Positive reviews: "food"
findAssocs(CapstonePos_wA$TDM, "food", 0.15)
#Negative reviews: "food"
findAssocs(CapstoneNeg_wA$TDM, "food", 0.16)

#Positive reviews: "menu"
findAssocs(CapstonePos_wA$TDM, "menu", 0.15)
#Negative reviews: "menu"
findAssocs(CapstoneNeg_wA$TDM, "menu", 0.15)

#Positive reviews: "service", "staff", "server"
findAssocs(CapstonePos_wA$TDM, 
           c("service", "staff", "server"),
           c(0.15,0.19,0.16))
#Negative reviews: "service", "staff", "server"
findAssocs(CapstoneNeg_wA$TDM, 
           c("service", "staff", "server"), 
           c(0.19, 0.2, 0.2))

findAssocs(CapstoneNeg_wA$TDM, "time", 0.20)
findAssocs(CapstoneNeg_wA$TDM, "music", 0.15)


