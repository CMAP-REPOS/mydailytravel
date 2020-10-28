# Load libraries

library(ggplot2)
library(lubridate)
library(tidyverse)
library(slider)
library(cmapplot)

# Load data
setwd("C:/Users/Daniel/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

data_place18 <- read.csv("place.csv")
data_household18 <- read.csv("household.csv")
data_person18 <- read.csv("person.csv")
data_location18 <- read.csv("location.csv")

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
            tpurp = tpurp,
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
  # Create combined mode and purpose
  mutate(mode_tpurp = paste(mode,tpurp,sep = "_")) %>%
  # Add weights
  left_join(., person_weights, by = "samp_per_no")

# Extract possible modes
possible_modes <- tibble(mode = unique(trips_in_motion_wday_wip$mode))
possible_tpurp <- tibble(tpurp = unique(trips_in_motion_wday_wip$tpurp))
possible_mode_tpurp <- tibble(mode_tpurp = unique(trips_in_motion_wday_wip$mode_tpurp))

# Calculate trips in motion by mode
trip_times <-
  # Establish sequence of times over the day (in one minute increments)
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "min"))

trip_times_mode_and_purp <- trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_mode_tpurp, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(trips_in_motion_wday_wip$wtperfin[which(
                                            x %within% trips_in_motion_wday_wip$trip_interval &
                                            y == trips_in_motion_wday_wip$mode_tpurp)]),
                        time_band,
                        mode_tpurp
  )) %>%
  separate(col = mode_tpurp, into = c("mode","tpurp"), sep = "_") %>%
  group_by(mode,tpurp) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 12, .after = 12)) %>%
  ungroup()

trip_times_mode <- trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_modes, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct purpose)
  mutate(mode_count = mapply(function(x,y) sum(trips_in_motion_wday_wip$wtperfin[which(
                                            x %within% trips_in_motion_wday_wip$trip_interval &
                                            y == trips_in_motion_wday_wip$mode)]),
    time_band,
    mode
  )) %>%
  group_by(mode) %>%
  # Calculate rolling average
  mutate(rolling_mode_count = slide_dbl(mode_count, mean, .before = 12, .after = 12)) %>%
  ungroup()

trip_times_tpurp <- trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_tpurp, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct purpose)
  mutate(trip_purp = mapply(function(x,y) sum(trips_in_motion_wday_wip$wtperfin[which(
    x %within% trips_in_motion_wday_wip$trip_interval &
      y == trips_in_motion_wday_wip$tpurp)]),
    time_band,
    tpurp
  )) %>%
  group_by(tpurp) %>%
  # Calculate rolling average
  mutate(rolling_tpurp_count = slide_dbl(trip_purp, mean, .before = 12, .after = 12)) %>%
  ungroup()


# Graph output of trips in motion by mode
trip_times_mode %>%
  filter(!(mode %in% c(-9,-8,-7,-1))) %>%
  ggplot(aes(x = time_band,y = rolling_mode_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago")) +
  scale_y_continuous(label = scales::comma) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap()

# Graph output of bike trips in motion
trip_times_mode %>%
  filter(mode %in% c(102,103,104)) %>%
  ggplot(aes(x = time_band,y = rolling_mode_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago")) +
  scale_y_continuous(label = scales::comma) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap()

# Graph output of divvy trips in motion by purpose
trip_times_mode_and_purp %>%
  filter(mode == 103) %>%
  filter(tpurp > 0) %>%
  mutate(tpurp_bucket = case_when(
    tpurp == 1 ~ "Home",
    tpurp %in% c(2,3,4,5) ~ "Work",
    tpurp == 6 ~ "School",
    tpurp %in% c(8,9,10,11,15) ~ "Shopping/Errands",
    tpurp %in% c(12,13,22,23) ~ "Healthcare/Fitness",
    tpurp %in% c(14,18,19,20,21,7) ~ "Friends/Family/Community",
    tpurp %in% c(16,17) ~ "Food",
    tpurp %in% c(26,27,28) ~ "Travel",
    tpurp %in% c(24,97) ~ "Other",
    TRUE ~ "Other"
    )) %>%
  group_by(tpurp_bucket,time_band) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_bucket)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago")) +
  scale_y_continuous(label = scales::comma)
  cmap_fill_discrete(palette = "mobility")
  theme_cmap()

# Graph output of trips in motion by purpose
trip_times_tpurp %>%
  filter(!(tpurp %in% c(-9,-8,-7))) %>%
  ggplot(aes(x = time_band,y = rolling_tpurp_count)) +
  geom_area(aes(fill = as.character(tpurp))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago")) +
  scale_y_continuous(label = scales::comma) +
  theme_cmap()

# Graph output of school trips in motion by mode
trip_times_mode_and_purp %>%
  filter(tpurp == 6) %>%
  filter(!(mode %in% c(-9,-8,-7,-1))) %>%
  ggplot(aes(x = time_band, y = rolling_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago")) +
  scale_y_continuous(label = scales::comma) +
  theme_cmap()


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
