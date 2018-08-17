#====================================== Hubway data preparation ======================================#

# Set the working directory:
setwd("C:/Users/priya/Dropbox/priyanka_shared")
# Load the dataset:
station_status = read.csv('stationstatus.csv',header=TRUE,sep=",")
trips = read.csv('hubway_trips.csv',header=TRUE,sep=",")
# Checking the structure and type of data:
View(station_status) # Variables : unique id, stn id, timestamp update, available bikes, empty docks, capacity
str(station_status) # 30546812 obs. of  6 variables
View(trips) # Variables : seq id, hubway id, status, start date, start stn, end date, end stn, bike nr, subsc type, zip code, dob, gender
str(trips) # 1578973 obs. of  13 variables

# Data Cleaning and Wrangling:
# checking for NA values
station_status[!complete.cases(station_status),] # 0 NA values
trips[!complete.cases(trips),] # NA values exist. We find distribution of NAs using below function:
find_nas_in_df <- function() {
  data.frame(sapply(trips, function(x) sum(is.na(x)))) %>%
    rownames_to_column("column") %>%
    rename("na_count" = "sapply.trips..function.x..sum.is.na.x...") %>%
    filter(na_count > 0) %>%
    arrange(desc(na_count)) %>%
    mutate(na_percentage = na_count * 100 / nrow(trips))
}
install.packages('dplyr')
install.packages('tidyverse')
library(dplyr)
library(tidyverse)
find_nas_in_df() # 77% of birth date values are NA. We can ignore this column.
# 45 values are NA in end_statn and 14 values are NA in strt_statn. We remove these rows.
trips <- trips[!is.na(trips$end_statn), ]
trips <- trips[!is.na(trips$strt_statn), ]

# Split timestamp into date and time for station_status data:
station_status$updateDate <- as.Date(station_status$update)
station_status$updateTime <- format(as.POSIXct(station_status$update) ,format = "%H:%M:%S") 

# Finding the 10 topmost popular station:
t <- table(trips$strt_statn)
t1 <- sort(t)
t2 <- tail(t1, 10) # Station 22 has the highest trips
class(t2) # table
t3 <- as.data.frame(t2)

# Subsetting the data wrt these 10 most popular stations:
station_status1 <- station_status # creating a dummy dataframe to analyse
station_status1 <- station_status1[station_status1$station_id %in% t3$Var1,]
station_status1$station_id <- as.factor(station_status1$station_id)

# Creating a new variable for weekday/weekend:
station_status1$week <- ifelse(weekdays(station_status1$updateDate) %in% c("Saturday", "Sunday"), "weekend", "weekday")

# Creating a new variable for season:
install.packages('hydroTSM')
library(hydroTSM)
station_status1$season <- time2season(station_status1$updateDate, out.fmt = "seasons", type="default")

station_status1$week <- as.factor(station_status1$week)
station_status1$season <- as.factor(station_status1$season)
str(station_status1)

# Data preparation for Auto Regressive modeling (order 2):
range(station_status1$updateDate) # Aug 22, 2011 to Sept 30, 2011
table(station_status1$updateDate) # data is not present on a uniform daily level

install.packages('datetime')
library(datetime)
station_status1$updateTime <- as.time(station_status1$updateTime) # converting updateTime from character to time data type
# Creating a new column called updateHour for extracting only the hourly details per station:
install.packages('lubridate')
library(lubridate)
station_status1$updateHour <- hour(station_status1$update)
station_status1$updateHour <- as.factor(station_status1$updateHour)

# Grouping the hourly data per station and assigning the no. of empty docks for the last minute of the hour as the empty dock for the whole hour:
station_status_df <- station_status1 %>% select('station_id','updateDate','updateHour','nbEmptyDocks') %>% group_by(station_id, updateDate, updateHour) %>% 
  summarise(nbEmptyDocks = last(nbEmptyDocks))
View(station_status_df)

st <- station_status_df
# station_status_df <- st

# Making dataframe for auto regressive modeling:

station_status_df$predictor1 = station_status_df$nbEmptyDocks
station_status_df$predictor2 = station_status_df$nbEmptyDocks
colnames(station_status_df)[colnames(station_status_df)=="nbEmptyDocks"] <- "response"

station_status_df <- station_status_df %>% mutate_at(c("predictor1"), funs(lag), n = 2)
station_status_df <- station_status_df %>% mutate_at(c("predictor2"), funs(lag), n = 1)


# Analyzing percentage of NAs:
find_nas_in_df <- function() {
  data.frame(sapply(station_status_df, function(x) sum(is.na(x)))) %>%
    rownames_to_column("column") %>%
    rename("na_count" = "sapply.station_status_df..function.x..sum.is.na.x...") %>%
    filter(na_count > 0) %>%
    arrange(desc(na_count)) %>%
    mutate(na_percentage = na_count * 100 / nrow(trips))
}
library(dplyr)
library(tidyverse)
find_nas_in_df() # 37% of predictor1 and 18% of predictor2 values are NAs
# Dropping rows where NAs are introduced:
station_status_df <- na.omit(station_status_df)
###

# Adding 2 new columns for weekday and season:
station_status_df$week <- ifelse(weekdays(station_status_df$updateDate) %in% c("Saturday", "Sunday"), "weekend", "weekday")
install.packages('hydroTSM')
library(hydroTSM)
station_status_df$season <- time2season(station_status_df$updateDate, out.fmt = "seasons", type="default")
str(df_2)
station_status_df$week <- as.factor(station_status_df$week)
station_status_df$season <- as.factor(station_status_df$season)

# Creating the predictors, respone and test data (X, Y and test data):
Z <- data.frame(predictor1 = station_status_df$predictor1, 
                predictor2 = station_status_df$predictor2, predictor3 = station_status_df$station_id, 
                predictor4 = station_status_df$week, predictor5 = station_status_df$season, response = station_status_df$response)
# Splitting the data:
set.seed(123)
nr1 = nrow(Z)
trnIndex = sample(1:nr1, size = 20, replace=FALSE) #get a random sample of 20s row-indices
Z_tst = Z[trnIndex,]   #test data with the randomly selected row-indices
Z_trn = Z[-trnIndex,]  #train data with the other row-indices
# Creating the predictors data (X):
X <- data.frame(predictor1 = Z_trn$predictor1, 
                predictor2 = Z_trn$predictor2, predictor3 = Z_trn$predictor3, 
                predictor4 = Z_trn$predictor4, predictor5 = Z_trn$predictor5)
# Creating the response data (Y):
Y <- data.frame(response = Z_trn$response)
# Creating the set of new feature vectors (test data):
X_new <- Z_tst

# Function Calls to calculate the intervals via the 3 methods:
source("methods_script.R")
predictions_by_method0 <- method.0(X, Y, X_new) # OLS method
intervals_by_method2 <- method.2(X, Y, X_new) # Conditional quantile method
intervals_by_method3 <- method.3(X, Y, X_new) # OLS + Wiggle room method


