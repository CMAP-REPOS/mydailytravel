# Load libraries

library("dplyr")
library("ggplot2")
library(lubridate)
library(tidyverse)

# Load data
setwd("C:/Users/Daniel/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

data_place18 <- read.csv("place.csv")
data_household18 <- read.csv("household.csv")
data_person18 <- read.csv("person.csv")

setwd("C:/Users/Daniel/Documents/Git/dlcomeaux/mydailytravel")


# Analyze data

# Code to determine daytypes - testing
#
# foo <- data_place18$arrtime[1]
# too <- strptime(foo,format = '%Y-%m-%d %H:%M:%S')
# too <- ymd_hms(foo)
# wday(foo)
#
# weekdays(too)

get_time <- function(time = now()) {
  time %>%
    str_split(" ") %>%
    map_chr(2) %>%
    hms()
}


trips_in_motion <- data_place18 %>%
  mutate(arrtime = ymd_hms(arrtime),
         deptime = ymd_hms(deptime),
         ) %>%
  mutate(day_of_week = wday(arrtime),
         deptime_hms = get_time(deptime),
         arrtime_hms = get_time(arrtime))

