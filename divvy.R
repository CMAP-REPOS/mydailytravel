# This script downloads and analyzes Q3 2019 Divvy ridership data to produce a
# trips-in-motion chart for Divvy trips over the course of an average weekday.

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(tidyverse)
library(ggplot2)
library(lubridate)
library(slider)
library(cmapplot)

source("helper_fns.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

divvy_zip <- tempfile()
download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q3.zip",divvy_zip)
divvy <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q3.csv"))
file.remove(divvy_zip)
file.remove("Divvy_Trips_2019_Q3.csv")
rm(divvy_zip)


#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

# Create helper values
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24
july4 <- interval(ymd_hms("2019-07-04 03:00:00",
                          tz = "America/Chicago"),
                  ymd_hms("2019-07-05 03:00:00",
                          tz = "America/Chicago"))
laborday <- interval(ymd_hms("2019-09-02 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-09-03 03:00:00",
                             tz = "America/Chicago"))
# Identify number of weekdays in the sample
number_of_weekdays <-
  sum(!weekdays(seq(ymd("2019-07-01"),
                    ymd("2019-09-30"),
                    by = "days")) %in% c("Saturday", "Sunday")) -
  1 - # July 4
  1   # Labor Day

# Clean and filter data for application of the TIM calculator function
divvy_wip <-
  divvy %>%
  # Convert to datetime object
  mutate(start_time = ymd_hms(start_time, tz = "America/Chicago"),
         end_time = ymd_hms(end_time, tz = "America/Chicago")) %>%
  mutate(trip_time = end_time - start_time) %>%
  # Exclude trips that are on weekends (with days running from 3am to 3am)
  mutate(wday = wday(start_time - 3 * 60 * 60)) %>%
  # Keep out trips that are either Saturday or Sunday
  filter(!(wday %in% c(1,7))) %>%
  # Remove July 4 and Labor Day
  filter(!(start_time %within% july4 | start_time %within% laborday)) %>%
  # Exclude trips > 3 hours
  filter(trip_time <= 60 * 60 * 3) %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_start = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(start_time,12,19))),
                             tzone = "America/Chicago")) %>%
  # Make trips that start before 3am into trips on the next day (given survey timing)
  mutate(trip_start = case_when(
    trip_start < threshold ~ trip_start + day_value,
    TRUE ~ trip_start)
  ) %>%
  # Add trip end based on trip duration
  mutate(trip_end = trip_start + trip_time) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Add weight of 1 divided by the number of weekdays for summing the average TIMs
  mutate(weight = 1/number_of_weekdays)

# Use function defined in trips_in_motion.R
trip_times_divvy_counts <-
  tim_calculator(
    base_weights = divvy_wip$weight,
    trip_interval = divvy_wip$trip_interval,
    criteria = divvy_wip$usertype)

# Define breaks
breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")
# Create chart
divvy_p1 <-
  trip_times_divvy_counts %>%
  # Relevel user type
  mutate(identifier = factor(identifier, levels = c("Subscriber","Customer"))) %>%
  mutate(identifier = recode_factor(identifier,
                                    "Subscriber" = "Subscriber",
                                    "Customer" = "One-time user")) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count, fill = identifier)) +
  # Reverse stacking position
  geom_area(position = position_stack(reverse = TRUE)) +
  # Reformat labels
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                    breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  # Add CMAP style
  cmap_fill_discrete(palette = "legislation") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

# Export plot
finalize_plot(divvy_p1,
              title = "Divvy trips in motion on an average Q3 weekday, by
              customer type.",
              caption = "Note: Trips in motion are 25-minute rolling averages.
              \"One-time user\" refers to Divvy cusomters that purchased a day
              pass or one ride. Analysis is based on total Divvy ridership data
              from Q3 2019, which had a total of 64 non-holiday weekdays.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of Divvy
              ridership data from July 1 - September 30, 2019.",
              filename = "divvy_p1",
              mode = "png",
              # height = 6.3,
              # width = 11.3,
              overwrite = T)
