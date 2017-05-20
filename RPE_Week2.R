sessionInfo()


getwd()
setwd("C:/VSTS/Coursera")

.libPaths()
installed.packages()



install.packages(
 c("data.table", "devtools", "dlmn", "faraway", "forcats", 
"GGally", "ggmap", "ggthemes", "ghit", "GISTools", 
"gridExtra", "knitr", "leaflet", "lubridate","microbenchmark", 
"package", "pander", "plotly", "profvis", "pryr", "purrr", 
"rappdirs", "raster", "readr", "rmarkdown", "sp", 
"tidyr", "tidyverse", "tigris", "titanic", "viridis"), 
lib = "C:/Program Files/Microsoft SQL Server/140/R_SERVER/library")

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)

################################################################################

ext_tracks_file <- "data/ebtrk_atlc_1988_2015.txt"
ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                       4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                          "hour", "year", "latitude", "longitude",
                          "max_wind", "min_pressure", "rad_max_wind",
                          "eye_diameter", "pressure_1", "pressure_2",
                          paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                          "storm_type", "distance_to_land", "final")

ext_tracks <- read_fwf(ext_tracks_file,
                       fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                       na = "-99")

ext_tracks
names(ext_tracks)

# Perhaps the most useful and concise function for understanding the *str*ucture of your data is str().
str(ext_tracks)


# Without piping
# function(dataframe, argument_2, argument_3)

# With piping
# dataframe %>%
#   function(argument_2, argument_3)

# Without piping
katrina <- filter(ext_tracks, storm_name == "KATRINA")
katrina_reduced <- select(katrina, month, day, hour, max_wind)
head(katrina_reduced, 3)


# With piping
ext_tracks %>%
  filter(storm_name == "KATRINA") %>%
  select(month, day, hour, max_wind) %>%
  head(3)

ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))


knots_to_mph <- function(knots) {
 mph <- 1.152 * knots
}
ext_tracks %>%
  summarize(n_obs = n(),
            worst_wind = knots_to_mph(max(max_wind)),
            worst_pressure = min(min_pressure))

ext_tracks %>%
  group_by(storm_name, year) %>%
  head()

ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(n_obs = n(),
            worst_wind = max(max_wind),
            worst_pressure = min(min_pressure))

library(ggplot2)
ext_tracks %>%
  group_by(storm_name) %>%
  summarize(worst_wind = max(max_wind)) %>%
  ggplot(aes(x = worst_wind)) + geom_histogram()


ext_tracks %>%
 select(storm_name, month, day, hour, year, latitude, longitude, max_wind)
ext_tracks %>%
 select(storm_name, latitude, longitude, starts_with("radius_34"))

# Depending on its class. For numeric data, summary() displays the minimum, 1st quartile, median, mean, 3rd quartile, and maximum. 
# For categorical variables (called 'factor' variables in R), summary() displays the number of times each value (or 'level') occurs in the data. For example, each value of storm_name
# You can see that R truncated the summary by including a catch-all category called 'Other'
summary(ext_tracks)

# we can see how many times each value actually occurs in the data with table(dataset$variable) .
table(ext_tracks$longitude)


ext_tracks %>%
 select(storm_name, month, day, hour, year, latitude, longitude, max_wind) %>%
 filter(storm_name == "KATRINA") %>%
 head(3)

ext_tracks %>%
  group_by(storm_name, year) %>%
  summarize(worst_wind = max(max_wind)) %>%
  filter(worst_wind >= 160)

ext_tracks %>%
  select(storm_name, month, day, hour, latitude, longitude, max_wind) %>%
  filter(storm_name == "ANDREW" & max_wind >= 137)

ext_tracks %>%
  select(storm_name, month, day, hour, eye_diameter, max_wind) %>%
  filter(storm_name %in% c("KATRINA", "GILBERT"))


################################################################################

library(faraway)
library(knitr)
library(pander)
data(worldcup)

worldcup <- worldcup %>%
  mutate(player_name = rownames(worldcup))

worldcup
worldcup %>% slice(1:3)
worldcup %>% head(10)

worldcup <- worldcup %>%
  group_by(Position) %>%
  mutate(ave_shots = mean(Shots)) %>%
  ungroup()

worldcup <- worldcup %>%
  group_by(Position) %>%
  summarize(ave_shots = mean(Shots))

worldcup %>%
  rename(Player_Position = Position) %>%

worldcup


################################################################################

data("VADeaths")
head(VADeaths)
names(VaDeaths)

# Move age from row names into a column
VADeaths <- VADeaths %>%
  tbl_df() %>%
  mutate(age = row.names(VADeaths))
VADeaths

# Gather everything but age to tidy data
VADeaths %>%
  gather(key = key, value = death_rate, - age)



# For example, if you ’d like to plot the relationship between the time a player played in the World Cup and his number of saves, tackles, and shots, with a separate graph for
#  each position(Figure 1.2), you can use gather to pull all the numbers of saves, tackles, and shots into a single column(Number) and then use faceting to plot them as separate graphs:
worldcup %>%
  select(Position, Time, Shots, Tackles, Saves) %>%
  gather(Type, Number, - Position, - Time) %>%
  ggplot(aes(x = Time, y = Number)) +
  geom_point() +
  facet_grid(Type ~ Position)

#For example, if you wanted to print a table of the average number and range of passes by position for
# the top four teams in this World Cup(Spain, Netherlands, Uruguay, and Germany), you could run:
# Summarize the data to create the summary statistics you want
wc_table <- worldcup %>%
  filter(Team %in% c("Spain", "Netherlands", "Uruguay", "Germany")) %>%
  select(Team, Position, Passes) %>%
  group_by(Team, Position) %>%
  summarize(ave_passes = mean(Passes),
            min_passes = min(Passes),
            max_passes = max(Passes),
            pass_summary = paste0(round(ave_passes), " (",
                                  min_passes, ", ",
                                  max_passes, ")")) %>%
  select(Team, Position, pass_summary)
# What the data looks like before using `spread`
wc_table


# Use spread to create a prettier format for a table
wc_table %>%
  spread(Position, pass_summary) %>%
  kable()

wc_table %>%
  spread(Position, pass_summary) %>%
  pander()

################################################################################

library(readr)
team_standings <- read_csv("data/team_standings.csv")
team_standings %>% head(3)

left_join(worldcup, team_standings, by = "Team")

#As an example of merging, say you want to create a table of the top 5 players by shots on goal, as well as the final standing for
# each of these player ’s teams, using the worldcup and team_standings data. You can do this by running:
worldcup %>%
  mutate(Name = rownames(worldcup),
         Team = as.character(Team)) %>%
  select(Name, Position, Shots, Team) %>%
  arrange(desc(Shots)) %>%
  slice(1:5) %>%
  left_join(team_standings, by = "Team") %>% # Merge in team standings
rename("Team Standing" = Standing) %>%
  kable()

################################################################################

library(lubridate)
ymd("2006-03-12")
ymd("'06 March 12")
ymd_hm("06/3/12 6:30 pm")

andrew_tracks <- ext_tracks %>%
  filter(storm_name == "ANDREW" & year == "1992") %>%
  select(year, month, day, hour, max_wind, min_pressure) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime))

head(andrew_tracks, 3)

andrew_tracks %>%
  gather(measure, value, - datetime) %>%
  ggplot(aes(x = datetime, y = value)) +
  geom_point() + geom_line() +
  facet_wrap(~measure, ncol = 1, scales = "free_y")

class(andrew_tracks$datetime)
# Once an object is in a date or date - time class(POSIXlt orPOSIXct, respectively), there are other functions in thelubridate package
# you can use to pull certain elements out of it. For example, you can use the functions year, months, mday, wday, yday, weekdays, hour, minute,
# and secondto pull the year, month, month day, etc., of the date. The following code uses the datetime variable in the Hurricane Andrew
# track data to add new columns for the year, month, weekday, year day, and hour of each observation:

andrew_tracks %>%
  select(datetime) %>%
  mutate(year = year(datetime),
         month = months(datetime),
         weekday = weekdays(datetime),
         yday = yday(datetime),
         hour = hour(datetime))


check_tracks <- ext_tracks %>%
  select(month, day, hour, year, max_wind) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime),
         weekday = weekdays(datetime),
         weekday = factor(weekday, levels = c("Sunday", "Monday",
                                              "Tuesday", "Wednesday",
                                              "Thursday", "Friday",
                                              "Saturday")),
         month = months(datetime),
         month = factor(month, levels = c("April", "May", "June",
                                          "July", "August", "September",
                                          "October", "November",
                                          "December", "January")))

check_weekdays <- check_tracks %>%
  group_by(weekday) %>%
  summarize(ave_max_wind = mean(max_wind)) %>%
  rename(grouping = weekday)
check_months <- check_tracks %>%
  group_by(month) %>%
  summarize(ave_max_wind = mean(max_wind)) %>%
  rename(grouping = month)

a <- ggplot(check_weekdays, aes(x = grouping, y = ave_max_wind)) +
  geom_bar(stat = "identity") + xlab("")
b <- a %+% check_months

library(gridExtra)
grid.arrange(a, b, ncol = 1)


# Based on Figure 1.4, there ’s little pattern in storm intensity by day of the week, but there is a pattern by month,
#  with the highest average wind speed measurements in observations in September and neighboring months(and no storm observations in February or March) .
# 
# There are a few other interesting things to note about this code:
#  To get the weekday and month values in the right order, the code uses the factor function in conjunction with thelevels option,
#  to control the order in which R sets the factor levels. By specifying the order we want to use withlevels, the plot prints out
#  using this order, rather than alphabetical order(try the code without the factor calls for month and weekday and compare the resulting graphs to the ones shown here) .
# 
#  The grid.arrange function, from the gridExtra package, allows you to arrange different ggplot objects in the same plot area.
#   Here, I ’ve used it to put the bar charts for weekday(a) and for month(b) together in one column(ncol = 1) .
# 
#  If you ever have ggplot code that you would like to re - use for a new plot with a different data frame, you can save a lot of
#  copying and pasting by using the %+% function. This function takes a ggplot object(a in this case, which is the bar chart by weekday)
#   and substitutes a different data frame(check_months) for the original one(check_weekdays), but otherwise maintains all code.
#   Note that we used rename to give the x - variable the same name in both datasets so we could take advantage of the %+% function.



andrew_tracks <- ext_tracks %>%
  filter(storm_name == "ANDREW") %>%
  slice(23:47) %>%
  select(year, month, day, hour, latitude, longitude) %>%
  unite(datetime, year, month, day, hour) %>%
  mutate(datetime = ymd_h(datetime),
         date = format(datetime, "%b %d"))

### Load/Attach Google Maps API
library(ggmap)

miami <- get_map("miami", zoom = 5)
ggmap(miami) +
  geom_path(data = andrew_tracks, aes(x = -longitude, y = latitude),
            color = "gray", size = 1.1) +
  geom_point(data = andrew_tracks,
             aes(x = -longitude, y = latitude, color = date),
             size = 2)


# To create this plot using local time for Miami, FL, rather than UTC(Figure 1.6), you can use the with_tz function from lubridate to convert the datetime
#  variable in the track data from UTC to local time. This function inputs a date - time object in the POSIXct class, as well as a character string with the time zone of the location for
#   which you ’d like to get local time, and returns the corresponding local time for that location.

andrew_tracks <- andrew_tracks %>%
  mutate(datetime = with_tz(datetime, tzone = "America/New_York"),
         date = format(datetime, "%b %d"))

ggmap(miami) +
  geom_path(data = andrew_tracks, aes(x = -longitude, y = latitude),
            color = "gray", size = 1.1) +
  geom_point(data = andrew_tracks,
             aes(x = -longitude, y = latitude, color = date),
             size = 2)



#To find out how muchg memory size the object is taking use:
object.size(andrew_tracks)
object.size(ext_tracks)

################################################################################


ls()
rm(list = ls())

##########
library(swirl)

#Coursera ... Course 1 in Mastering Software Development in R (John Hopkins University)
install_course("The R Programming Environment")

# TO RUN "SWIRL" TUTORIAL:
swirl()


################################################################################


# Add a `dplyr` or `tidyr` function to the pipe chain in the code 
# at the bottom of this script to subset the `worldcup` dataset to 
# four columns, so that the first lines of the resulting data 
# frame (`wc_1`) look like this: 
#
##           Time   Passes  Tackles Saves
## Abdoun      16        6        0     0
## Abe        351      101       14     0
## Abidal     180       91        6     0
## Abou Diaby 270       111       5     0
#
# I have already loaded the `worldcup` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

wc_1 <- worldcup %>%
  select(Time, Passes, Tackles, Saves) %>%

head(wc_1, 4)
names(worldcup)

# After the previous question, you should have transformed the 
# `worldcup` data to look like this:
#
##           Time   Passes  Tackles Saves
## Abdoun      16        6        0     0
## Abe        351      101       14     0
## Abidal     180       91        6     0
## Abou Diaby 270       111       5     0
#
# Now add a `dplyr` or `tidyr` function to the pipe chain in the 
# code at the bottom of this script so that `wc_2` data frame looks 
# like this, with four columns, and a single observation (the 
# mean value of each variable): 
#
##       Time   Passes  Tackles     Saves
##   208.8639 84.52101 4.191597 0.6672269
#
# I have already loaded the `worldcup` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

wc_2 <- worldcup %>%
  select(Time, Passes, Tackles, Saves) %>%
  summarize(Time = mean(Time), Passes = mean(Passes), Tackles = mean(Tackles), Saves = mean(Saves))


# After the previous question, you should have transformed the 
# `worldcup` data to look like this:
#
##       Time   Passes  Tackles     Saves
##   208.8639 84.52101 4.191597 0.6672269
#
# Now add a `dplyr` or `tidyr` function to the pipe chain in the 
# code at the bottom of this script so that the `wc_3` data frame looks 
# like this, with variable names in one column and the mean value
# of each variable in another column: 
#
##      var           mean
##     Time    208.8638655
##   Passes     84.5210084
##  Tackles      4.1915966
##    Saves      0.6672269
#
# I have already loaded the `worldcup` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

wc_3 <- worldcup %>%
  select(Time, Passes, Tackles, Saves) %>%
  summarize(Time = mean(Time),
            Passes = mean(Passes),
            Tackles = mean(Tackles),
            Saves = mean(Saves)) %>%
  gather(var, mean)

# After the previous question, you should have transformed the `worldcup`
# data to look like this:
#
##      var           mean
##     Time    208.8638655
##   Passes     84.5210084
##  Tackles      4.1915966
##    Saves      0.6672269
#
# Now add a `dplyr` or `tidyr` function to the pipe chain in the 
# code at the bottom of this script so that the `wc_4` data frame looks 
# like this, with variable means rounded to one decimal place: 
#
##      var     mean
##     Time    208.9
##   Passes     84.5
##  Tackles      4.2
##    Saves      0.7
#
# I have already loaded the `worldcup` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

wc_4 <- worldcup %>%
  select(Time, Passes, Tackles, Saves) %>%
  summarize(Time = mean(Time),
            Passes = mean(Passes),
            Tackles = mean(Tackles),
            Saves = mean(Saves)) %>%
  gather(var, mean) %>%
mutate(mean = round(mean, 1))


# Add a `dplyr` or `tidyr` function to the pipe chain in the code 
# at the bottom of this script to subset the `titanic` dataset to 
# four columns, so that the first lines of the resulting data 
# frame (`titanic_1`) look like this: 
#
##  Survived  Pclass   Age      Sex
##         0       3    22     male
##         1       1    38   female
##         1       3    26   female
##         1       1    35   female
##         0       3    35     male
##         0       3    NA     male
#
# I have already loaded the `titanic` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

titanic_1 <- titanic %>%
select(Survived, Pclass, Age, Sex)


# Now add a `dplyr` or `tidyr` function to the pipe chain in the 
# code at the bottom of this script so that `titanic_2` data frame 
# looks like this, with observations where "Age" was missing removed: 
#
##  Survived  Pclass   Age      Sex
##         0       3    22     male
##         1       1    38   female
##         1       3    26   female
##         1       1    35   female
##         0       3    35     male
##         0       1    54     male
#
# I have already loaded the `titanic` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

titanic_2 <- titanic %>%
  select(Survived, Pclass, Age, Sex) %>%
filter(Age != "NA")


# Now add a `dplyr` or `tidyr` function to the pipe chain in the 
# code at the bottom of this script so that `titanic_2` data frame 
# looks like this, with a new column added that specifies whether 
# the person's age is under 15, from 15 to 50, or over 50
# (this column should have a factor class, with factor levels ordered
# by age-- "Under 15", "15 to 50", "Over 50"): 
#
##   Survived Pclass   Age     Sex      agecat
##          0      3    22    male    15 to 50
##          1      1    38  female    15 to 50
##          1      3    26  female    15 to 50
##          1      1    35  female    15 to 50
##          0      3    35    male    15 to 50
##          0      1    54    male     Over 50
#
# I have already loaded the `titanic` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

titanic_3 <- titanic %>%
  select(Survived, Pclass, Age, Sex) %>%
  filter(!is.na(Age)) %>%
mutate(agecat = cut(Age, breaks = c(0, 15, 51, Inf),
 labels = c("Under 15", "15 to 50", "Over 50"),
  right = FALSE))


# After the previous question, you should have transformed the `titanic`
# data to look like this:
#
##   Survived Pclass   Age     Sex      agecat
##          0      3    22    male    15 to 50
##          1      1    38  female    15 to 50
##          1      3    26  female    15 to 50
##          1      1    35  female    15 to 50
##          0      3    35    male    15 to 50
##          0      1    54    male     Over 50
#
# Add one or more `dplyr` or `tidyr` functions to the pipe chain in 
# the code at the bottom of the script to change the `titanic` 
# dataset. The first six lines of the final `titanic_4` dataset 
# should look like the following example, with the number of
# passengers, number of survivors, and percent survival stratified
# by passenger class, age category, and sex. Be sure to use the 
# same column names as shown in the example output. 
#
## Pclass   agecat    Sex      N     survivors   perc_survived
## <int>   <fctr>    <chr>   <int>     <int>         <dbl>
##   1    Under 15  female     2         1        50.000000
##   1    Under 15    male     3         3       100.000000
##   1    15 to 50  female    70        68        97.142857
##   1    15 to 50    male    72        32        44.444444
##   1    Over 50   female    13        13       100.000000
##   1    Over 50     male    26         5        19.230769
#
# I have already loaded the `titanic` data frame for you, so you 
# can explore it and test out your code in the console.
#
# When you are ready submit your answer, save the script and type 
# submit(), or type reset() to reset the script to its original 
# state. 

titanic_4 <- titanic %>%
  select(Survived, Pclass, Age, Sex) %>%
  filter(!is.na(Age)) %>%
  mutate(agecat = cut(Age, breaks = c(0, 14.99, 50, 150),
                      include.lowest = TRUE,
                      labels = c("Under 15", "15 to 50",
                                 "Over 50"))) %>%
select(Pclass, agecat, Sex, Survived) %>%
group_by(Pclass, agecat, Sex) %>%
summarize(N = n(), survivors = sum(Survived), perc_survived = (sum(Survived)/n()) * 100)


head(titanic_4)
1