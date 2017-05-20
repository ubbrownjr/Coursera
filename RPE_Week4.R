#  daily_SPEC_2014.csv.bz2:
#  A compressed CSV file containing daily measurements of
#  particulate matter chemical constituents in the United States for
#  the year 2014. Note that you should NOT have to decompress this file.
#  The data are measured at a network of federal, state, and local monitors
#  and assembled by the EPA. In this dataset, the "Arithmetic Mean" column
#  provides the level of the indicated chemical constituent and
#  the "Parameter.Name" column provides the name of the chemical constituent.
#  The combination of a "State Code", a "County Code", and a "Site Num",
#  uniquely identifies a monitoring site(the location of which is provided
#  by the "Latitude" and "Longitude" columns) .
#  
#  aqs_sites.xlsx:
#  An excel spreadsheet containing metadata about each of the monitoring
#  sites in the United States where pollution measurements are made. In
#  particular, the "Land Use" and "Location Setting" variables contain
#  information about what kinds of areas the monitors are located in
#  (i.e. “residential” vs. “forest”).

.libPaths()
getwd()


find.package(c("dplyr", "tidyr", "readr", "readxl"))
installed.packages(lib.loc = "C:/Program Files/Microsoft SQL Server/130/R_SERVER/library") # MSFT R SERVER SQL 2016

# install.packages(c("dplyr", "tidyr", "readr", "readxl"))
# Warning in utils::install.packages(...):
# # 
#  'lib = "C:/Program Files/Microsoft SQL Server/130/R_SERVER/library"' is not writable
# Error in utils::install.packages(...):unable to install packages

# find.package(c("dplyr", "tidyr", "readr", "readxl"))
install.packages(c("dplyr", "tidyr", "readr", "readxl"), lib = "C:/R_Libraries")
installed.packages(lib.loc = "C:/R_Libraries")

# install.packages(c("devtools", "Rcpp"), lib = "C:/R_Libraries")
# library(devtools)
# library(Rcpp)
# install.packages(c("utils", "tools"), lib = "C:/R_Libraries")

? remove.packages
remove.packages("readxl", lib = "C:/R_Libraries")


################################################################################################################

.libPaths("C:/R_Libraries")
setwd("C:/Users/USER/OneDrive/VSTS/Coursera")

library(dplyr)
library(readr)
library(tidyr)


#dailyspec10 <- read_csv("data/daily_SPEC_2014.csv.bz2", n_max = 10)
#head(dailyspec10)

dailyspec <- read_csv("data/daily_SPEC_2014.csv.bz2")
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))


names(dailyspec)
head(dailyspec)

################################################################################################################

# [1] "StateCode" "CountyCode" "SiteNum" "ParameterCode" "POC" "Latitude" "Longitude" "Datum"
# [9] "ParameterName" "SampleDuration" "PollutantStandard" "DateLocal" "UnitsofMeasure" "EventType" "ObservationCount" "ObservationPercent"
# [17] "ArithmeticMean" "1stMaxValue" "1stMaxHour" "AQI" "MethodCode" "MethodName" "LocalSiteName" "Address"
# [25] "StateName" "CountyName" "CityName" "CBSAName" "DateofLastChange"

# For all of the questions below, you can ignore the missing values in the dataset
# , so when taking averages, just remove the missing values before taking the average
# (i.e. you can use na.rm = TRUE in the mean() function)
# 
# 1. What is average Arithmetic.Mean for “Bromine PM2.5 LC ” in the state of Wisconsin in this dataset ?
dailyspec %>%
 filter(StateName == "Wisconsin", ParameterName == "Bromine PM2.5 LC") %>%
 summarize(AvgVal = (mean(ArithmeticMean, trim = 0, na.rm = TRUE)))

dailyspec %>%
 filter(StateName == "Wisconsin", ParameterName == "Bromine PM2.5 LC") %>%
 select(StateName, ParameterName, ArithmeticMean) %>%
 arrange(desc(ArithmeticMean)) %>%
 head(4)


# Calculate the average of each chemical constituent across all states, monitoring sites and all time points.
# 2. Which constituent Parameter.Name has the highest average level ?
dailyspec %>%
 group_by(ParameterName) %>%
 summarize(AvgVal = (mean(ArithmeticMean, trim = 0, na.rm = TRUE))) %>%
 select(ParameterName, AvgVal) %>%
 arrange(desc(AvgVal)) %>%
 head(4)

dailyspec %>%
 group_by(ParameterName, SiteNum, DateLocal, DateofLastChange) %>%
 summarize(AvgVal = mean(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 select(ParameterName, AvgVal) %>%
 arrange(desc(AvgVal)) %>%
 head(4)

dailyspec %>%
 group_by(ParameterName) %>%
 summarize(MaxVal = max(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 select(ParameterName, MaxVal) %>%
 arrange(desc(MaxVal)) %>%
 head(4)

# [1] "StateCode" "CountyCode" "SiteNum" "ParameterCode" "POC" "Latitude" "Longitude" "Datum"
# [9] "ParameterName" "SampleDuration" "PollutantStandard" "DateLocal" "UnitsofMeasure" "EventType" "ObservationCount" "ObservationPercent"
# [17] "ArithmeticMean" "1stMaxValue" "1stMaxHour" "AQI" "MethodCode" "MethodName" "LocalSiteName" "Address"
# [25] "StateName" "CountyName" "CityName" "CBSAName" "DateofLastChange"

# 3. Which monitoring site has the highest average level of “Sulfate PM2.5 LC ” across all time ?
#    Indicate the state code, county code, and site number.
##  State 39 County 081 Site 0017   ##  State 48 County 203 Site 0002   ##  State 31 County 055 Site 0019   ##  State 10 County 001 Site 0003

dailyspec %>%
 filter(ParameterName == "Sulfate PM2.5 LC") %>%
 group_by(StateCode, CountyCode, SiteNum) %>%
 summarize(HighestAvgLevel = mean(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 filter(HighestAvgLevel > 3) %>%
 arrange(desc(HighestAvgLevel)) %>%
 head(2)

dailyspec %>%
 filter(ParameterName == "Sulfate PM2.5 LC") %>%
 group_by(StateCode, CountyCode, SiteNum) %>%
 summarize(SiteAvgLevel = mean(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 select(SiteAvgLevel, StateCode, CountyCode, SiteNum) %>%
 order(SiteAvgLevel) %>%
 head(15)

dailyspec %>%
 filter(ParameterName == "Sulfate PM2.5 LC") %>%
 select(StateCode, CountyCode, SiteNum, ArithmeticMean) %>%
 group_by(SiteNum) %>%
 arrange(desc(ArithmeticMean)) %>%
 head(4)



# [1] "StateCode" "CountyCode" "SiteNum" "ParameterCode" "POC" "Latitude" "Longitude" "Datum"
# [9] "ParameterName" "SampleDuration" "PollutantStandard" "DateLocal" "UnitsofMeasure" "EventType" "ObservationCount" "ObservationPercent"
# [17] "ArithmeticMean" "1stMaxValue" "1stMaxHour" "AQI" "MethodCode" "MethodName" "LocalSiteName" "Address"
# [25] "StateName" "CountyName" "CityName" "CBSAName" "DateofLastChange"

# 4. What is the absolute difference in the average levels of “EC PM2.5 LC TOR ”
#  between the states California and Arizona, across all time and all monitoring sites ?
##  0.17648069  ##  0.09476190  ##  0.018567  ##  0.06320775
x <- dailyspec %>%
 filter(ParameterName == "EC PM2.5 LC TOR" & StateName %in% c("California", "Arizona")) %>%
 select(StateName, ArithmeticMean) %>%
 group_by(StateName) %>%
 summarize(SiteAvgLevel = mean(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 arrange(desc(SiteAvgLevel))

x[1, 2] - x[2, 2]



# [1] "StateCode" "CountyCode" "SiteNum" "ParameterCode" "POC" "Latitude" "Longitude" "Datum"
# [9] "ParameterName" "SampleDuration" "PollutantStandard" "DateLocal" "UnitsofMeasure" "EventType" "ObservationCount" "ObservationPercent"
# [17] "ArithmeticMean" "1stMaxValue" "1stMaxHour" "AQI" "MethodCode" "MethodName" "LocalSiteName" "Address"
# [25] "StateName" "CountyName" "CityName" "CBSAName" "DateofLastChange"

# 5.What is the median level of “OC PM2.5 LC TOR ” in the western United States, across all time ?
# Define western as any monitoring location that has a Longitude LESS THAN - 100.
##  0.5175
##  0.0269
##  0.2600
##  0.4300

dailyspec %>%
 filter(ParameterName == "OC PM2.5 LC TOR" & Longitude < -100) %>%
 summarize(MedianLevel = median(ArithmeticMean, na.rm = TRUE))


########################################################################################################################################
# installed.packages(lib.loc = "C:/Program Files/Microsoft/R Server/R_SERVER/library") # MSFT R SERVER STANDALONE 9.1.0
# .libPaths()

# install.packages("readxl", repos = "http://cran.cnr.Berkeley.edu/", lib = "C:/R_Libraries")
# install.packages("xlsx", lib = "C:/R_Libraries")
# install.packages("rJava", repos = "http://cran.cnr.Berkeley.edu/", lib = "C:/R_Libraries")
install.packages("openxlsx", lib = "C:/R_Libraries")
install.packages("lubridate", lib = "C:/R_Libraries")

# find.package(readxl)
# find.package(xlsx)
find.package(openxlsx)

# library(help = readxl)
# library(help = xlsx)
library(help = openxlsx)

# library(readxl)
# library(xlsx)

setwd("C:/Users/USER/OneDrive/VSTS/Coursera")
.libPaths(c("C:/R_Libraries", "C:/Program Files/Microsoft/R Server/R_SERVER/library"))
library(openxlsx)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)


EPA_AQS <- readWorkbook("data/aqs_sites.xlsx")
dailyspec <- read_csv("data/daily_SPEC_2014.csv.bz2")
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))
names(dailyspec) <- sub(" ", "", names(dailyspec))

names(EPA_AQS)
names(dailyspec)



# Use the readxl package to read the file aqs_sites.xlsx into R
# This file contains metadata about each of the monitoring sites in the EPA ’s monitoring system.
# In particular, the "Land Use" and "Location Setting" variables contain information about what 
# kinds of areas the monitors are # located in (i.e. “residential” vs. “forest”).

#[1] "State.Code" "County.Code" "Site.Number" "Latitude" "Longitude" "Datum" "Elevation"
#[8] "Land.Use" "Location.Setting" "Site.Established.Date" "Site.Closed.Date" "Met.Site.State.Code" "Met.Site.County.Code" "Met.Site.Site.Number"
#[15] "Met.Site.Type" "Met.Site.Distance" "Met.Site.Direction" "GMT.Offset" "Owning.Agency" "Local.Site.Name" "Address"
#[22] "Zip.Code" "State.Name" "County.Name" "City.Name" "CBSA.Name" "Tribe.Name" "Extraction.Date"

# 6.How many monitoring sites are labelled as both RESIDENTIAL for "Land Use" and SUBURBAN for "Location Setting" ?
##  2233
##  3527
##  1610
##  1008

x <- EPA_AQS %>%
 filter(Location.Setting == "SUBURBAN" & Land.Use == "RESIDENTIAL") %>%
 select(State.Code)
sum(complete.cases(x))
dim(x)
nrow(na.omit(x))
NROW(x)



#[1] "State.Code" "County.Code" "Site.Number" "Latitude" "Longitude" "Datum" "Elevation"
#[8] "Land.Use" "Location.Setting" "Site.Established.Date" "Site.Closed.Date" "Met.Site.State.Code" "Met.Site.County.Code" "Met.Site.Site.Number"
#[15] "Met.Site.Type" "Met.Site.Distance" "Met.Site.Direction" "GMT.Offset" "Owning.Agency" "Local.Site.Name" "Address"
#[22] "Zip.Code" "State.Name" "County.Name" "City.Name" "CBSA.Name" "Tribe.Name" "Extraction.Date"

# 7.What is the median level of “EC PM2.5 LC TOR ” amongst monitoring sites that are labelled as both “RESIDENTIAL” and “SUBURBAN” in the
# eastern U.S., where eastern is defined as Longitude greater than or equal to - 100 ?
##  0.6100
##  0.1740
##  0.0460
##  0.5880

x <- EPA_AQS %>%
 filter(Location.Setting == "SUBURBAN" & Land.Use == "RESIDENTIAL" & Longitude >= - 100) %>%
 select(State.Code, County.Code, Site.Number)

names(x)
colnames(x) <- c("StateCode", "CountyCode", "SiteNum")
head(x)
summary(x)
class(x)
class(x$StateCode)
class(x$CountyCode)


y <- dailyspec %>%
 filter(ParameterName == "EC PM2.5 LC TOR" & Longitude >= -100) %>%
 select(StateCode, CountyCode, SiteNum, ArithmeticMean)

summary(y)

#y <- as.data.frame(lapply(y, as.numeric))

y <- data.frame(y, CC = as.numeric(y$CountyCode), SN = as.numeric(y$SiteNum))
y <- y %>%
 select(StateCode, CC, SN, ArithmeticMean)
colnames(y) <- c("StateCode", "CountyCode", "SiteNum", "ArithmeticMean")

names(y)
head(y)
class(y)
class(y$StateCode)

# rm(y)

y%>%
 inner_join(x, by = c("StateCode", "CountyCode", "SiteNum")) %>%
 summarize(MedianLevel = median(ArithmeticMean, na.rm = TRUE))

object.size(dailyspec) # 430,394,064 bytes
object.size(EPA_AQS) # 6,828,648 bytes





# 8.Amongst monitoring sites that are labeled as COMMERCIAL for "Land Use",
# which month of the year has the highest average levels of "Sulfate PM2.5 LC" ?
##  June
##  August
##  November
##  February

x <- EPA_AQS %>%
 filter(Land.Use == "COMMERCIAL") %>%
 select(State.Code, County.Code, Site.Number)

names(x)
colnames(x) <- c("StateCode", "CountyCode", "SiteNum")
head(x)
summary(x)
class(x$StateCode)
class(x$CountyCode)

y <- dailyspec %>%
 filter(ParameterName == "Sulfate PM2.5 LC") %>%
 select(StateCode, CountyCode, SiteNum, DateLocal, ArithmeticMean)

summary(y)

y <- data.frame(y, CC = as.numeric(y$CountyCode), SN = as.numeric(y$SiteNum))
y <- y %>%
 select(StateCode, CC, SN, DateLocal, ArithmeticMean)
colnames(y) <- c("StateCode", "CountyCode", "SiteNum",  "DateLocal" ,"ArithmeticMean")

names(y)
head(y)
class(y$StateCode)
class(y$DateLocal)

# rm(y)

y %>%
 inner_join(x, by = c("StateCode", "CountyCode", "SiteNum")) %>%
 mutate(Month = month(DateLocal)) %>%
 select(Month, ArithmeticMean) %>%
 group_by(Month) %>%
 summarize(SiteAvgLevel = mean(ArithmeticMean, trim = 0, na.rm = TRUE)) %>%
 filter(SiteAvgLevel > 1.9) %>%
 arrange(desc(SiteAvgLevel)) %>%
 head(2)



# 9.Take a look at the data for the monitoring site identified by
#  State Code 6, County Code 65, and Site Number 8001(this monitor is in California) .
#  At this monitor, for how many days is the sum of "Sulfate PM2.5 LC"
#  and "Total Nitrate PM2.5 LC" greater than 10 ?
#  For each of the chemical constituents, there will be some dates
#  that have multiple `Arithmetic Mean` values at this monitoring site.
#  When there are multiple values on a given date, take the average of
#  the constituent values for that date.
##  5
##  50
##  11
##  24

dailyspec %>%
 filter(ParameterName %in% c("Total Nitrate PM2.5 LC", "Sulfate PM2.5 LC") & StateCode == "06" & CountyCode == "065" & SiteNum == "8001") %>%
 select(DateLocal, ArithmeticMean) %>%
 group_by(DateLocal) %>%
 summarize(SiteAvgLevel = sum(ArithmeticMean)) %>%
 filter(SiteAvgLevel > 10)


dailyspec %>%
 filter(ParameterName == "Sulfate PM2.5 LC" & StateCode == "06" & CountyCode == "065" & SiteNum == "8001") %>%
 select(DateLocal, ArithmeticMean) %>%
 group_by(DateLocal) %>%
 filter(ArithmeticMean > 10) %>%
summarize()
%in% c("California", "Arizona")

# 10. Which monitoring site in the dataset has the highest correlation
# between "Sulfate PM2.5 LC" and "Total Nitrate PM2.5 LC" across all dates ? Identify the monitoring site by it 's State, County, and Site Number code.
# For each of the chemical constituents, there will be some dates that have multiple Sample.Value' s at a monitoring site. When there are multiple values on a given date, take the average of the constituent values for that date.
# Correlations between to variables can be computed with the cor() function.
##  State 42 County 45 Site 2
##  State 5 County 113 Site 3
##  State 2 County 90 Site 35
##  State 16 County 37 Site 2
