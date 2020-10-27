# Load libraries

library(ggplot2)
library(lubridate)
library(tidyverse)
library(slider)

# Load data
setwd("C:/Users/dlcom/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

data_place18 <- read.csv("place.csv")
data_household18 <- read.csv("household.csv")
data_person18 <- read.csv("person.csv")
data_location18 <- read.csv("location.csv")

setwd("C:/Users/dlcom/Documents/Git/dlcomeaux/mydailytravel")


# Analyze data

# Code to determine daytypes - testing
#
# foo <- data_place18$arrtime[1]
# too <- strptime(foo,format = '%Y-%m-%d %H:%M:%S')
# too <- ymd_hms(foo)
# wday(foo)
#
# weekdays(too)

# Convert to datetime object and add day of week
trips_in_motion <- data_place18 %>%
  mutate(arrtime = ymd_hms(arrtime),
         deptime = ymd_hms(deptime),
         ) %>%
  mutate(day_of_week = wday(arrtime))

# Process data
trips_in_motion_wday_wip <-
  trips_in_motion %>%
  # Remove weekends
  filter(day_of_week != 1 & day_of_week != 7) %>%
  # Remove trips > 15 hours
  filter(travtime < 15 * 60) %>%
  # Group to establish single trips
  group_by(placeGroup,sampno,perno) %>%
  # Pull data for single trips
  summarize(perno = perno,
            sampno = sampno,
            placeGroup = placeGroup,
            mode = mode,
            travtime = sum(travtime, na.rm = TRUE), # sum multi-leg trip times
            trip_end = max(arrtime,na.rm = TRUE), # find latest arrival time, as that is the end of the multi-leg trip
            day_of_week = day_of_week) %>%
  # Make every trip on the same day (for graphing)
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",substr(trip_end,12,19))),tzone = "America/Chicago")) %>%
  # Make trips that end before 3am into trips on the next day (given survey timing)
  mutate(trip_end = case_when(
    trip_end < 1577869201 ~ trip_end + 60 * 60 * 24,
    TRUE ~ trip_end)
  ) %>%
  # Remove trips with no travel time (not in motion)
  filter(travtime > 0) %>%
  # Calculate trip start time as end time minus travel time
  mutate(trip_start = trip_end - 60*travtime) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Add weights
  left_join(.,data_person18[,c(1,2,116)],by = c("sampno","perno"))

# Extract possible modes
possible_modes <- tibble(mode = unique(trips_in_motion_wday_wip$mode))

# Calculate trips in motion by mode
trip_times <-
  # Establish sequence of times over the day (in one minute increments)
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "min")) %>%
  # Add all possible modes to each time
  full_join(.,possible_modes, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(trips_in_motion_wday_wip$wtperfin[which(
                                            x %within% trips_in_motion_wday_wip$trip_interval &
                                            y == trips_in_motion_wday_wip$mode)]),
                        time_band,
                        mode
                        )
  ) %>%
  group_by(mode) %>%
  arrange(time_band) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count,mean,.before = 12,.after = 12)) %>%
  ungroup()


trip_times %>% ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime()


#########

tnc_data <- data_person18 %>%
  select(sampno,
         perno,
         age,
         sex,
         hisp,
         race,
         smrtphn,
         wkstat,
         tnc_use,
         tnc_typ,
         tnc_cost,
         tnc_purp,
         disab,
         wtperfin) %>%
  left_join(.,data_location18 %>% filter(home == 1) %>% select(sampno,county_fips),by = "sampno")

# Breakdown by age
tnc_data <- tnc_data %>%
  mutate(age_bucket = case_when(
    age <= 24 & age >=18 ~ "18-24",
    age <= 34 & age >= 25 ~ "25-34",
    age <= 44 & age >= 35 ~ "35-44",
    age <= 54 & age >= 45 ~ "45-54",
    age <= 64 & age >= 55 ~ "55-64",
    age > 65 ~ "65 and above",
    TRUE ~ "Other"
  ))

tnc_data %>%
  filter(!(tnc_use %in% c(-9,-8,-1))) %>%
  group_by(age_bucket) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

tnc_data %>%
  filter(tnc_cost >0) %>%
  group_by(age_bucket) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

# Breakdown by race
tnc_data %>%
  filter(!(race %in% c(-8,-7,-1))) %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(county_fips != -9) %>%
  mutate(race_ethn = case_when(
    race == 1 & hisp != 1 ~ "White",
    hisp == 1 ~ "Hispa",
    race == 2 & hisp != 1 ~ "Black",
    race == 3 & hisp != 1 ~ "Asian",
    race == 4 & hisp != 1 ~ "Amer. Indian/AK Native",
    race == 5 & hisp != 1 ~ "HI/Pac. Islander",
    race == 6 & hisp != 1 ~ "Multiracial",
    TRUE ~ "Other"
  )) %>%
  group_by(race,county_fips) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()

# Breakdown by home location
tnc_data %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(county_fips != -9) %>%
  group_by(county_fips) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()

tnc_data %>%
  filter(tnc_cost>0) %>%
  filter(county_fips != -9) %>%
  group_by(county_fips) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()

