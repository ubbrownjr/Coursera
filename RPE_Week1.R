.libPaths()
getwd()
setwd("C:/Users/USER/OneDrive/VSTS/Coursera")


find.package("tidyr")
install.packages("tidyr", destdir = "C:/Program Files/Microsoft SQL Server/140/R_SERVER/library")


find.package("magrittr")
find.package("readr")
install.packages("readr", lib = "C:/Program Files/Microsoft SQL Server/140/R_SERVER/library")


library(tidyr)
library(dplyr)

VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths)) %>%
  gather(key, death_rate, - age) %>%
  separate(key, c("urban", "gender"), sep = " ") %>%
  mutate(age = factor(age), urban = factor(urban), gender = factor(gender))


library("readr")
teams <- read_csv("data/team_standings.csv")
logs <- read_csv("data/2016-07-19.csv.gz", n_max = 10)
logdates <- read_csv("data/2016-07-20.csv.gz",
                     col_types = cols_only(date = col_date()),
                     n_max = 10)

## GET FLAT FILE WEB DATA ... TXT CSV FILES VIA HTTP
ext_tracks_file <- paste0("http://rammb.cira.colostate.edu/research/",
                          "tropical_cyclones/tc_extended_best_track_dataset/",
                          "data/ebtrk_atlc_1988_2015.txt")


USCensusColCounties <- paste("https://www2.census.gov/geo/docs/reference/cenpop2010/county/CenPop2010_Mean_CO08.txt")
USCensusColCounties <- read.csv(USCensusColCounties)

USCensusColCounties %>%
 filter(STNAME == "Colorado") %>%
 select(STATEFP,COUNTYFP,COUNAME,POPULATION) %>%
 sample_n(4)

## API METHOD CALLS TO GET DATA
find.package("httr")
library(httr)
meso_url <- "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py/"
denver <- GET(url = meso_url,
                    query = list(station = "DEN",
                                 data = "sped",
                                 year1 = "2016",
                                 month1 = "6",
                                 day1 = "1",
                                 year2 = "2016",
                                 month2 = "6",
                                 day2 = "30",
                                 tz = "America/Denver",
                                 format = "comma")) %>%
  content() %>%
  read_csv(skip = 5, na = "M")

denver %>% slice(1:3)


##  R packages already exist for
##   many open data APIs. If an R package already exists for
##    an API, you can use functions from that package directly,
##  rather than writing your own code using the API protocols and httr functions.
##  Other examples of existing R packages to interact with open data APIs include:
##  twitteR:Twitter
##  rnoaa:National Oceanic and Atmospheric Administration
##  Quandl:Quandl(financial data)
##  RGoogleAnalytics:Google Analytics
##  censusr, acs:United States Census
##  WDI, wbstats:World Bank
##  GuardianR, rdian:The Guardian Media Group
##  blsAPI:Bureau of Labor Statistics
##  rtimes:New York Times
##  dataRetrieval, waterData:United States Geological Survey

ls()
rm(list = ls())

##########
library(swirl)

#Coursera ... 
#Course 1 in Mastering Software Development in R (John Hopkins University)
install_course("The R Programming Environment")

# TO RUN "SWIRL" TUTORIAL:
swirl()

