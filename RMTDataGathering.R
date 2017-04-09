############################################################
#
# Capstone Project: Data Gathering 
# Springboard Foundations of Data Science
#
# The purpose of this program is to scrape data from the web 
# for popular review and travel websites. 
# This script will scrape ratings, reviews, and dates.
#
# Author: Antoine Beauchamp. 
# Edited: April 6th, 2017
# Created: February 21st, 2017
#
#############################################################


CapstoneDir = "/Users/Antoine/Documents/Work/DataScience/Springboard/FoundationsofDataScience/CapstoneProject/CapstoneDeliverables"

setwd(CapstoneDir)

rm(list=ls())

#Load required libraries
library(rvest)
suppressMessages(library(dplyr))
library(tidyr)
suppressMessages(library(readr))


######################### Function: YelpScrape ###############################
#
# This function is used to scrape review data from Yelp.com
# Arguments: 
# BaseURL: URL to the first page of reviews that you want to scrape

YelpScrape <- function(BaseURL) {

  #Review counter. Yelp = 20 per page.
  ReviewCount <- 0 
  #Vector initialization
  Reviews <- character(0) 
  Ratings <- character(0)
  Dates <- character(0)
  PrevRev <- character(0)
  flag <- 1

  #Iterate over reviews pages and scrape data 
  while(flag==1){
  
    #Yelp URL for the given review page
    page_url <- paste(BaseURL,"?start=",as.character(ReviewCount),sep="")
  
    #Scrape reviews, ratings and dates from current URL
    ReviewsNew <- read_html(page_url) %>% html_nodes(".review-content p") %>% html_text
    RatingsNew <- read_html(page_url) %>% html_nodes(".rating-large") %>% html_attr("title")
    DatesNew <- read_html(page_url) %>% html_nodes(".biz-rating-large .rating-qualifier") %>% html_text()
    #Additional data identifying previous/updated reviews
    PrevRevNew <- read_html(page_url) %>% html_nodes(".biz-rating-large .rating-qualifier") %>% as.character()
    
    #Print iteration count identifier to std.out
    print(paste("Scraping Yelp page",ceiling(ReviewCount/20)))
    
    #Append new data to existing vectors
    Reviews <- c(Reviews,ReviewsNew)
    Ratings <- c(Ratings,RatingsNew)
    Dates <- c(Dates, DatesNew)
    PrevRev <- c(PrevRev,PrevRevNew)
    
    #Increment counter
    ReviewCount=ReviewCount +length(ReviewsNew)
    
    #Loop ending condition
    flag <- if(length(ReviewsNew)==0){0} else {1}
  }
 return(list("Reviews"=Reviews, "Ratings"=Ratings, "Dates"=Dates, "PrevRev"=PrevRev))
}


######################### Function: OpenTableScrape ###############################
#
# This function is used to scrape review data from opentable.com
# Arguments: 
# BaseURL: URL to the review page from open table. Note that the URL must end with &page= 
# without specify the page number. This is done in the function.


OpenTableScrape <- function(BaseURL) {

  # Parameters/vector initialization
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates <- character(0)
  flag <- 1
  
  #Iterate over review pages and scrape data
  while(flag==1) {
  
    #URL for given page
    page_url <- paste(BaseURL,as.character(ReviewCount),sep="")
    
    #Scrape reviews, ratings, dates from current URL
    ReviewsNew <- read_html(page_url) %>% html_nodes("#reviews-results .review-content") %>% html_text
    RatingsNew <- read_html(page_url) %>% html_nodes("#reviews-results .filled") %>% html_attr("title")
    DatesNew <- read_html(page_url) %>% html_nodes(".review-meta-separator+ .color-light") %>% html_text()
    
    #Append vectors
    Reviews <- c(Reviews,ReviewsNew)
    Ratings <- c(Ratings,RatingsNew)  
    Dates <- c(Dates,DatesNew)
    
    #Print iteration count identifier to std.out
    print(paste("Scraping OpenTable page",ReviewCount))
    
    #Increment counter
    ReviewCount <- ReviewCount+1
    
    #This condition checks whether we have reached the end of the reviews
    flag <- if(length(ReviewsNew)==0){0} else {1}
  }
  return(list("Reviews"=Reviews, "Ratings"=Ratings, "Dates"=Dates))
}


######################### Function: TripAdScrape ###############################
#
# This function is used to scrape review data from Trip Advisor
# Arguments:
# LandingURL: URL for the landing page of the restaurant to scrape 
# It will be used to link to the full review pages
TripAdScrape <- function(LandingURL) {

  #Get link to full review pages. These are embedded in review titles
  ReviewTitleLink <- read_html(LandingURL) %>% html_nodes(".quote a") %>% html_attr("href")
  
  #Base URL of first review page
  BaseURL <- paste("https://www.tripadvisor.ca",ReviewTitleLink[1],sep="")
  
  #Parameters/vector initialization 
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates1 <- character(0)
  Dates2 <- character(0)
  flag <- 1 
  
  #Iterate over review pages and scrape data
  while(flag==1){
  
    #Print iteration count identifier
    print(paste("Scraping Trip Advisor page",ReviewCount))
    
    #For the first page, the URL is just the base URL. 
    #For subsequent iterations, grab hyperlink to the new page
    # from the page links. 
    #E.g. page 1 carries a link to page 2 in its HTML, and so on. 
    if(ReviewCount == 1){
      page_url <- BaseURL
    } else {
      
      #Grab the page numbers for the links
      pagenum <- read_html(page_url) %>% 
        html_nodes(".pageNum") %>% 
        html_attr("data-page-number") %>% 
        as.numeric()
      #Get hyperlinks for subsequent pages
      hyperlink <- read_html(page_url) %>% 
        html_nodes(".pageNum") %>% 
        html_attr("href") %>% 
        as.character()
      #New URL
      page_url <- paste("https://www.tripadvisor.ca",hyperlink[pagenum==ReviewCount],sep="")
    }
    
    #Scrape reviews, ratings, dates from current URL
    ReviewsNew <- read_html(page_url) %>% html_nodes("#REVIEWS p") %>% html_text()
    RatingsNew <- read_html(page_url) %>% html_nodes("#REVIEWS .rating_s_fill") %>% html_attr("alt")
    DatesNew1 <- read_html(page_url) %>% html_nodes(".relativeDate") %>% html_attr("title",default=NA_character_)
    DatesNew2 <- read_html(page_url) %>% html_nodes(".ratingDate") %>% html_text()
    
    #Loop ending condition
    flag <- if(length(ReviewsNew)==0){0} else {1}
    
    #Append vectors
    Reviews <- c(Reviews, ReviewsNew)
    Ratings <- c(Ratings, RatingsNew)
    Dates1 <- c(Dates1, DatesNew1)
    Dates2 <- c(Dates2, DatesNew2)
    
    #Increment page count
    ReviewCount <- ReviewCount+1  
  }
  return(list("Reviews"=Reviews,"Ratings"=Ratings, "Dates1"=Dates1,"Dates2"=Dates2))
}

######################### Function: ZomatoScrape ###############################
# 
#This function is used to scrape review data from Zomato
# Arguments: 
# BaseURL: URL to the review page from Zomato. 
ZomatoScrape <- function(BaseURL) {

  #Parameters/vector initialization 
  ReviewCount <- 1
  Reviews <- character(0)
  Ratings <- character(0)
  Dates <- character(0)
  flag <- 1 
  
  #Scrape reviews, ratings, dates
  Reviews <- read_html(BaseURL) %>% html_nodes(".rev-text-expand , .rev-text") %>% html_text()
  Ratings <- read_html(BaseURL) %>% html_nodes(".rev-text-expand div , .rev-text div") %>% html_attr("aria-label")
  Dates <- read_html(BaseURL) %>% html_nodes("time") %>% html_attr("datetime")

  return(list("Reviews"=Reviews,"Ratings"=Ratings, "Dates"=Dates))
}

######################### MAIN ###############################

#######
#
# Data Acquisition
#

#Yelp main review page URL
BaseURL_Yelp <- "https://www.yelp.ca/biz/reds-midtown-tavern-toronto-2"
#OpenTable main review page URL
BaseURL_OpenTable <- "https://www.opentable.com/reds-midtown-tavern?covers=2&dateTime=2017-02-22+19%3A00%23reviews&page="
#Trip Advisor landing page
LandingURL_TripAd <- "https://www.tripadvisor.ca/Restaurant_Review-g155019-d5058760-Reviews-Reds_Midtown_Tavern-Toronto_Ontario.html"
#Zomato main review page URL
BaseURL_Zomato <- "https://www.zomato.com/toronto/reds-midtown-tavern-church-and-wellesley/reviews"

#Scrape data from websites
YelpData <- YelpScrape(BaseURL_Yelp)
OpenTableData <- OpenTableScrape(BaseURL_OpenTable)
TripAdData <- TripAdScrape(LandingURL_TripAd)
ZomatoData <- ZomatoScrape(BaseURL_Zomato)


## Notes: 
# The OpenTable dates in the form "Dined ## days ago" need to be clean on the
# day of data acquisition by using the system date and time.
#

#######
#
# Cleaning OpenTable Dates
#

#Find all instances of dates in the form "Dined ## days ago"
DatesLogic <- grepl("[0-9]+.*ago",OpenTableData$Dates)

#Subset date info to get the instances matching the above format
DatesTemp <- OpenTableData$Dates[DatesLogic]
#Empty char vector of proper length
DineDate <- character(length(DatesTemp))
#Extract the number of days ago that the review was posted
dineDay <- regmatches(DatesTemp,regexpr("[0-9]+",DatesTemp)) %>% as.numeric()
#Today's date
todayDate <- Sys.Date()
#Subtract the number of days from today's date
DineDate <- todayDate - dineDay

#Replace the date entries with the proper dates
OpenTableData$Dates[DatesLogic] <- DineDate

#Save raw data to file
save(YelpData,OpenTableData,TripAdData,ZomatoData, file="./Data/CapstoneRawData.RData")

