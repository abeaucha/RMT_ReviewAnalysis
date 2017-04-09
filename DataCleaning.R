###########################HEADER####################################
#
# Capstone Project: Data Cleaning
# Springboard Foundations of Data Science
#
# The purpose of this program is to clean the raw data acquired
# from the web scraping process in DataGathering.R. 
# The cleaning process consists of some general data cleaning and
# proper formatting of the dates and numerical ratings.
#
# Author: Antoine Beauchamp. 
# Edited: April 6th, 2017
# Created: February 28th, 2017
#
#############################################################


################## INTRO #######################

CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject/CapstoneDeliverables"

setwd(CapstoneDir)

rm(list=ls())

#Load required libraries
library(rvest)
suppressMessages(library(dplyr))
library(tidyr)
suppressMessages(library(readr))

load("./Data/CapstoneRawData_032017.RData")

str(YelpData)
str(OpenTableData)
str(TripAdData)
str(ZomatoData)


################## PRELIMINARY CLEANING #######################
#
# This section will be focused consolidating the data from the different
# websites.
#
#

# ==================== Yelp Data =============================
#

#Remove extraneous previous reviews from Yelp data
NoPrevRev <- !grepl("has-previous-review",YelpData$PrevRev)
YelpData$Ratings <- YelpData$Ratings[NoPrevRev]
YelpData$Dates <- YelpData$Dates[NoPrevRev]

#Create a vector identifying Yelp data
YelpVec <- rep("Yelp",length(YelpData$Reviews))

#Combine to data frame
YelpDF <- data_frame(Reviews=YelpData$Reviews,
                     Ratings=YelpData$Ratings,
                     Dates=YelpData$Dates, 
                     Website=YelpVec)


# ================== OpenTable Data ==========================
#

# Vector, OpenTable identifier
OpenTableVec <- rep("OpenTable", length(OpenTableData$Reviews))

#Merge to data frame
OpenTableDF <- data_frame(Reviews=OpenTableData$Reviews, 
                          Ratings=OpenTableData$Ratings, 
                          Dates=OpenTableData$Dates,
                          Website=OpenTableVec)

# ==================== Zomato Data ===========================
#

#Remove NAs from ratings
ZomatoData$Ratings <- ZomatoData$Ratings[!is.na(ZomatoData$Ratings)]

#Remove duplicate reviews. These truncated duplicates can be
# identified with the regex "read more"
FullRev <- !grepl("read more",ZomatoData$Reviews)
ZomatoData$Ratings <- ZomatoData$Ratings[FullRev]
ZomatoData$Reviews <- ZomatoData$Reviews[FullRev]

#Vector, Zomato identifier
ZomatoVec <- rep("Zomato", length(ZomatoData$Reviews))

#Merge to df
ZomatoDF <- data_frame(Reviews=ZomatoData$Reviews,
                       Ratings=ZomatoData$Ratings, 
                       Dates=ZomatoData$Dates, 
                       Website=ZomatoVec)


# =================== TripAdvisor Data ===========================
#

#Replace dates of the form "Reviewed ## days ago" with proper dates
TripAdData$Dates2[grepl("ago|yesterday|today",TripAdData$Dates2)] <- TripAdData$Dates1

#Vector, TripAdvisor identifier
TripAdVec <- rep("TripAdvisor",length(TripAdData$Reviews))

#Merge to df
TripAdDF <- data_frame(Reviews=TripAdData$Reviews,
                       Ratings=TripAdData$Ratings,
                       Dates=TripAdData$Dates2,
                       Website=TripAdVec)



#Merge all data frames
d1 <-full_join(YelpDF,OpenTableDF)
d2 <- full_join(d1,ZomatoDF)
RawDF <- full_join(d2,TripAdDF) %>% group_by(Website)

str(RawDF)
summary(RawDF)



################## CLEANING UP DATES #######################
#
# This section will be focused on formatting the date data
#
#

#Look at Yelp dates
head(subset(RawDF$Dates, RawDF$Website=="Yelp"),n=10)

#Remove newline characters and spaces. Deals with Yelp dates.
RawDF$Dates <- gsub("\n *","",RawDF$Dates)
head(subset(RawDF$Dates, RawDF$Website=="Yelp"),n=10)

#Find data that doesn't fit the Yelp pattern
UncleanDates <- RawDF$Dates[!grepl("^[0-9].*[0-9]$",RawDF$Dates)]
head(UncleanDates, n=10)

#Remove "Updated review"
RawDF$Dates <- gsub("Updated review.*$","", RawDF$Dates)

#Remove "Dined on "
RawDF$Dates <- gsub("Dined on ","",RawDF$Dates)

#Look at OpenTable, TripAdvisor and Zomato dates
head(subset(RawDF$Dates, RawDF$Website=="OpenTable"),n=10)
head(subset(RawDF$Dates, RawDF$Website=="TripAdvisor"),n=10)
head(subset(RawDF$Dates, RawDF$Website=="Zomato"),n=10)

#OpenTable and Zomato already good.

#Remove "Reviewd " from TripAdvisor data
RawDF$Dates <- gsub("Reviewed ","",RawDF$Dates)
head(subset(RawDF$Dates, RawDF$Website=="TripAdvisor"),n=10)

#Start converting to proper date format

#Yelp date format
head(subset(RawDF$Dates, RawDF$Website=="Yelp"))
#Let's grep for the dates
YelpDateRegex <- grep("^[0-9]+/.*[0-9]$",RawDF$Dates)
#Express dates as date class
RawDF$Dates[YelpDateRegex] <- RawDF$Dates[YelpDateRegex] %>% as.Date(format="%m/%d/%Y")

#OpenTable date format
head(subset(RawDF$Dates, RawDF$Website=="OpenTable"))
#grep for OpenTable dates and convert to date
OpenTableDateRegex <- grep("^([Jj]|[Ff]|[Mm]|[Aa]|[Jj]|[Ss]|[Oo]|[Nn]|[Dd]).+[0-9]+$",RawDF$Dates)
RawDF$Dates[OpenTableDateRegex] <- RawDF$Dates[OpenTableDateRegex] %>% as.Date(format="%B %d, %Y")

#TripAdvisor date format:
head(subset(RawDF$Dates, RawDF$Website=="TripAdvisor"))
#grep for dates and convert to date
TripAdRegex <- grep("^[0-9]+ ([Jj]|[Ff]|[Mm]|[Aa]|[Jj]|[Ss]|[Oo]|[Nn]|[Dd]).+[0-9]+$",RawDF$Dates)
RawDF$Dates[TripAdRegex] <- RawDF$Dates[TripAdRegex] %>% as.Date(format="%d %B %Y")

#Zomate date format: 
head(subset(RawDF$Dates, RawDF$Website=="Zomato"))

#Already in proper format. Convert to date.
RawDF$Dates[which(RawDF$Website == "Zomato")] <- RawDF$Dates[which(RawDF$Website == "Zomato")] %>% as.Date()

#Enforce date class
class(RawDF$Dates) <- "Date"
str(RawDF)


################## CLEANING UP RATINGS #######################
#
# This section will be focused on formatting the ratings data
#
#

#Yelp ratings
head(subset(RawDF$Ratings, RawDF$Website=="Yelp"))
#Get rid of "star rating"
RawDF$Ratings <- gsub("star rating","",RawDF$Ratings)


#OpenTable ratings
head(subset(RawDF$Ratings, RawDF$Website=="OpenTable"))
#Looks good already

#TripAdvisor ratings
head(subset(RawDF$Ratings, RawDF$Website=="TripAdvisor"))
#Remove "of 5 bubbles"
RawDF$Ratings <- gsub("of [0-9] bubbles","",RawDF$Ratings)


#Zomato ratings
head(subset(RawDF$Ratings, RawDF$Website=="Zomato"))
#Remove "Rated "
RawDF$Ratings <- gsub("Rated ","",RawDF$Ratings)

#Enforce numeric class
class(RawDF$Ratings) <- "numeric"


################## FINAL TOUCHES #######################
#
# Last bits of cleaning to do.
#

#Convert Websites to factor class
RawDF$Website <- factor(RawDF$Website,order=FALSE,levels=c("Yelp","OpenTable","Zomato","TripAdvisor")) 
str(RawDF)


#Rewline characters from reviews
RawDF$Reviews <- gsub("\n","",RawDF$Reviews)

#Clean up the Zomato reviews a bit by removing the "Rated" at the beginning
subset(RawDF$Reviews,RawDF$Website=="Zomato")
RawDF$Reviews <- gsub(" +Rated *","",RawDF$Reviews)
subset(RawDF$Reviews,RawDF$Website=="Zomato")



#Write clean data to .csv file.
write_csv(RawDF, "./Data/CapstoneCleanData.csv")


#End.
