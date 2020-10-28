# Load libraries

library(ggplot2)
library(lubridate)
library(tidyverse)
library(slider)

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

# Convert to datetime object and add day of week
trips_in_motion <- data_place18 %>%
  mutate(arrtime = ymd_hms(arrtime),
         ) %>%
  mutate(day_of_week = wday(arrtime))

# Create helper values
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24
person_weights <- data_person18[,c(1,2,116)] %>%
  mutate(samp_per_no = paste0(sampno,perno)) %>%
  select(samp_per_no,
         wtperfin)


# Process data
trips_in_motion_wday_wip3 <-
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
  ungroup() %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",substr(trip_end,12,19))),tzone = "America/Chicago")) %>%
  # Make trips that end before 3am into trips on the next day (given survey timing)
  mutate(trip_end = case_when(
    trip_end <= threshold ~ trip_end + day_value,
    TRUE ~ trip_end)
  ) %>%
  # Remove trips with no travel time (not in motion)
  filter(travtime > 0) %>%
  # Calculate trip start time as end time minus travel time
  mutate(trip_start = trip_end - 60*travtime) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Create combined sample and person number
  mutate(samp_per_no = paste0(sampno,perno)) %>%
  # Add weights
  left_join(., person_weights, by = "samp_per_no")

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

# Graph output
trip_times %>%
  filter(mode != -1, mode != -9) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime(labels = scales::date_format("%H:%M"), timezone = "CDT") +
  scale_y_continuous(label = scales::comma)
