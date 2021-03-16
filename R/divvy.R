#### TO BE REVIEWED

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

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")

# Download 2019 data (split into four quarters)
divvy_zip <- tempfile()
download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q1.zip",divvy_zip)
divvy_q1 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q1"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q2.zip",divvy_zip)
divvy_q2 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q2"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q3.zip",divvy_zip)
divvy_q3 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q3.csv"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q4.zip",divvy_zip)
divvy_q4 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q4.csv"))

file.remove(divvy_zip,"Divvy_Trips_2019_Q1","Divvy_Trips_2019_Q2",
            "Divvy_Trips_2019_Q3.csv","Divvy_Trips_2019_Q4.csv")
rm(divvy_zip)

# Fix names of q2, which are broken for unknown reasons on import
names(divvy_q2) <- names(divvy_q1)

# Combine all 2019 data
divvy <- rbind(divvy_q1,divvy_q2,divvy_q3,divvy_q4)


#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

# Create helper values

# 3am day threshold (since our day starts at 3am)
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
# The number of seconds in a day (used to add days)
day_value <- 60*60*24
# Holidays: New Year's Day, MLK Day, President's Day, Memorial Day, July 4th,
# Labor Day, Columbus Day, Veterans Day, Thanksgiving, the Day after
# Thanksgiving, and Christmas Day.
nyd <- interval(ymd_hms("2019-01-01 03:00:00",
                        tz = "America/Chicago"),
                ymd_hms("2019-01-02 02:59:59",
                        tz = "America/Chicago"))
mlk <- interval(ymd_hms("2019-01-21 03:00:00",
                        tz = "America/Chicago"),
                ymd_hms("2019-01-22 02:59:59",
                        tz = "America/Chicago"))
pres <- interval(ymd_hms("2019-02-18 03:00:00",
                        tz = "America/Chicago"),
                ymd_hms("2019-02-19 02:59:59",
                        tz = "America/Chicago"))
memorial <- interval(ymd_hms("2019-05-27 03:00:00",
                        tz = "America/Chicago"),
                ymd_hms("2019-05-28 02:59:59",
                        tz = "America/Chicago"))
july4 <- interval(ymd_hms("2019-07-04 03:00:00",
                          tz = "America/Chicago"),
                  ymd_hms("2019-07-05 02:59:59",
                          tz = "America/Chicago"))
labor <- interval(ymd_hms("2019-09-02 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-09-03 02:59:59",
                             tz = "America/Chicago"))
columbus <- interval(ymd_hms("2019-10-14 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-10-15 02:59:59",
                             tz = "America/Chicago"))
vets <- interval(ymd_hms("2019-11-11 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-11-12 02:59:59",
                             tz = "America/Chicago"))
xgiving <- interval(ymd_hms("2019-11-28 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-11-29 02:59:59",
                             tz = "America/Chicago"))
blackfri <- interval(ymd_hms("2019-11-29 03:00:00",
                            tz = "America/Chicago"),
                    ymd_hms("2019-11-30 02:59:59",
                            tz = "America/Chicago"))
xmas <- interval(ymd_hms("2019-12-25 03:00:00",
                             tz = "America/Chicago"),
                     ymd_hms("2019-12-26 02:59:59",
                             tz = "America/Chicago"))
# Join all holidays into one list
holidays <- c(nyd,mlk,pres,memorial,july4,labor,
              columbus,vets,xgiving,blackfri,xmas)

# Identify number of non-holiday weekdays in the sample
number_of_weekdays <-
  sum(!weekdays(seq(ymd("2019-01-01"),
                    ymd("2019-12-31"),
                    by = "days")) %in% c("Saturday", "Sunday")) -
  length(holidays)

# Clean and filter data for application of the TIM calculator function
divvy_wip <-
  divvy %>% # 3818004 records
  # Convert to datetime object
  mutate(start_time = ymd_hms(start_time, tz = "America/Chicago"),
         end_time = ymd_hms(end_time, tz = "America/Chicago")) %>%
  mutate(trip_time = end_time - start_time) %>%
  # Exclude trips > 3 hours as outliers
  filter(trip_time <= 60 * 60 * 3) %>% # 3817608 records
  
  # Identify the day of the week of the trip using the `wday` function from the
  # lubridate package. Note that we subtract 3 hours from the day to use this
  # function since our days "begin" at 3am, i.e., a trip that starts at 2:35am
  # on a Saturday will be evaluated at 11:35pm the prior day, making it a
  # Friday.
  mutate(wday = wday(start_time - 3 * 60 * 60)) %>%
  # Keep out trips that are either Saturday (7) or Sunday (1)
  filter(!(wday %in% c(1,7))) %>% # 2904294 records
  # Remove holidays (individually - note that %within% does not currently work
  # on a list of time intervals)
  filter(!(start_time %within% nyd | 
           start_time %within% mlk |
           start_time %within% pres |
           start_time %within% memorial |
           start_time %within% july4 |
           start_time %within% labor |
           start_time %within% columbus |
           start_time %within% vets |
           start_time %within% xgiving |
           start_time %within% blackfri |
           start_time %within% xmas)) %>% # 2833191 records
  
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_start = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(start_time,12,19))),
                             tzone = "America/Chicago")) %>%
  # Since we just forced all trips to start on the same day, but our days do not
  # begin at midnight, make trips that start before 3am into trips on the next
  # day.
  mutate(trip_start = case_when(
    trip_start < threshold ~ trip_start + day_value,
    TRUE ~ trip_start)
  ) %>%
  # Add trip end based on trip duration
  mutate(trip_end = trip_start + trip_time) %>%
  # Create trip interval using the lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Add weight of 1 divided by the number of weekdays for summing the average TIMs
  mutate(weight = 1/number_of_weekdays)

# Use function defined in helper_fns.R to create trips in motion graph
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
  # Get data
  trip_times_divvy_counts %>%
  # Relevel user type
  mutate(identifier = recode_factor(factor(identifier, levels = c("Subscriber",
                                                                  "Customer")),
                                    "Subscriber" = "Subscriber",
                                    "Customer" = "One-time user")) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count, fill = identifier)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  
  # Reformat axes
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                    breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +

  # Add CMAP style
  cmap_fill_discrete(palette = "legislation") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

# Export plot
finalize_plot(divvy_p1,
              title = "Divvy trips in motion on an average weekday, by
              customer type.",
              caption = "Note: Trips in motion are 25-minute rolling averages.
              \"One-time user\" refers to Divvy customers that purchased a day
              pass or one ride. Trips that were in motion as of 2:55 A.M. and 
              ended after 3:00 A.M. are captured on the right-hand-side of the 
              graph, and are not included in the totals of trips in motion as 
              of 3:00 A.M. on the left-hand-side of the graph.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of Divvy
              ridership data for the 250 non-holiday weekdays between January 1 
              and December 31, 2019.",
              filename = "divvy_p1",
              # sidebar_width = 0,
              # caption_align = 1,
              # mode = "png",
              # height = 6.3,
              # width = 11.3,
              overwrite = T)
