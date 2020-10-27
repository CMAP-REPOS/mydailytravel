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

trips_in_motion <- data_place18[1:50000,] %>%
  mutate(arrtime = ymd_hms(arrtime),
         deptime = ymd_hms(deptime),
         ) %>%
  mutate(day_of_week = wday(arrtime))

trips_in_motion_wday_wip <-
  trips_in_motion %>%
  filter(day_of_week != 1 & day_of_week != 7) %>%
  group_by(placeGroup,sampno,perno) %>%
  summarize(perno = perno,
            sampno = sampno,
            placeGroup = placeGroup,
            mode = mode,
            travtime = sum(travtime, na.rm = TRUE),
            trip_end = min(arrtime,na.rm = TRUE),
            day_of_week = day_of_week) %>%
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",substr(trip_end,12,19))),tzone = "America/Chicago")) %>%
  mutate(trip_end = case_when(
    trip_end < 1577869201 ~ trip_end + 60 * 60 * 24,
    TRUE ~ trip_end)
  ) %>%
  filter(travtime > 0) %>%
  mutate(trip_start = trip_end - 60*travtime) %>%
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  left_join(.,data_person18[,c(1,2,116)],by = c("sampno","perno"))

possible_modes <- tibble(mode = unique(trips_in_motion_wday_wip$mode))

trip_times <-
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "min")) %>%
  full_join(.,possible_modes, by = character()) %>%
  mutate(trip_count = mapply(function(x,y) sum(trips_in_motion_wday_wip$wtperfin[which(
                                            x %within% trips_in_motion_wday_wip$trip_interval &
                                            y == trips_in_motion_wday_wip$mode)]),
                        time_band,
                        mode
                        )
  ) %>%
  group_by(mode) %>%
  arrange(time_band) %>%
  mutate(rolling_count = slide_dbl(trip_count,mean,.before = 12,.after = 12)) %>%
  ungroup()


trip_times %>% ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = as.character(mode))) +
  scale_x_datetime()
