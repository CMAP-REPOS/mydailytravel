# This script downloads and analyzes Divvy ridership data from the same time
# period as the My Daily Travel survey to produce a trips-in-motion chart for
# Divvy trips over the course of an average weekday.

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

# Download 2018-2019 data (split into four quarters across the survey collection period)
divvy_zip <- tempfile()

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2018_Q3.zip",divvy_zip,quiet = TRUE)
divvy_18q3 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2018_Q3.csv"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2018_Q4.zip",divvy_zip,quiet = TRUE)
divvy_18q4 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2018_Q4.csv"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q1.zip",divvy_zip,quiet = TRUE)
divvy_19q1 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q1"))

download.file("https://divvy-tripdata.s3.amazonaws.com/Divvy_Trips_2019_Q2.zip",divvy_zip,quiet = TRUE)
divvy_19q2 <-read.csv(unzip(divvy_zip,files = "Divvy_Trips_2019_Q2"))


file.remove(divvy_zip,"Divvy_Trips_2019_Q1","Divvy_Trips_2019_Q2",
            "Divvy_Trips_2018_Q3.csv","Divvy_Trips_2018_Q4.csv")
rm(divvy_zip)

# Fix names of q2 2019, which are broken for unknown reasons on import
names(divvy_19q2) <- names(divvy_19q1)

# Combine all data
divvy <- rbind(divvy_18q3,divvy_18q4,divvy_19q1,divvy_19q2)


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


# We want to keep all weekdays from September 4th, 2018 to May 9th, 2019. This
# represents the time period during which the main phase of the travel survey
# was being actively completed. However, several weeks were excluded that need
# to be excluded here as well: the week of Thanksgiving 2018 (November 19th to
# November 23rd); the period between December 24th, 2018 and January 4th, 2019;
# and the week of Spring Break (April 15th to 19th, 2019).
mdt_int <-  interval(ymd_hms("2018-09-04 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-05-10 02:59:59",tz = "America/Chicago"))
                     
xgiving <-  interval(ymd_hms("2018-11-19 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-11-24 02:59:59",tz = "America/Chicago"))
xmas <-     interval(ymd_hms("2018-12-24 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-01-05 02:59:59",tz = "America/Chicago"))
springb <-  interval(ymd_hms("2019-04-15 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-04-20 02:59:59",tz = "America/Chicago"))

# We also exclude federal public holidays that fall within this time range that
# were not excluded through the date ranges above: Columbus Day, Veterans Day,
# MLK Day, and President's Day,.
columbus <- interval(ymd_hms("2018-10-08 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-10-09 02:59:59",tz = "America/Chicago"))
vets <-     interval(ymd_hms("2018-11-12 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2018-11-13 02:59:59",tz = "America/Chicago"))
mlk <-      interval(ymd_hms("2019-01-21 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-01-22 02:59:59",tz = "America/Chicago"))
pres <-     interval(ymd_hms("2019-02-18 03:00:00",tz = "America/Chicago"),
                     ymd_hms("2019-02-19 02:59:59",tz = "America/Chicago"))

# Join all holidays into one list
holidays <- c(mlk,pres,columbus,vets)

# Identify number of non-excluded weekdays in the sample
number_of_weekdays <-
  sum(!weekdays(seq(ymd("2018-09-04"),
                    ymd("2019-05-09"),
                    by = "days")) %in% c("Saturday", "Sunday")) -
  length(holidays) -
  5 - # for Thanksgiving week
  10 - # for Christmas/New Year's weeks
  5   # for Spring Break week

# Clean and filter data for application of the TIM calculator function
divvy_wip <-
  divvy %>% # 3629488 records
  # Convert to datetime object
  mutate(start_time = ymd_hms(start_time, tz = "America/Chicago"),
         end_time = ymd_hms(end_time, tz = "America/Chicago")) %>%
  mutate(trip_time = end_time - start_time) %>%
  # Exclude trips > 3 hours as outliers
  filter(trip_time <= 60 * 60 * 3) %>% # 3629175 records
  
  # Identify the day of the week of the trip using the `wday` function from the
  # lubridate package. Note that we subtract 3 hours from the day to use this
  # function since our days "begin" at 3am, i.e., a trip that starts at 2:35am
  # on a Saturday will be evaluated at 11:35pm the prior day, making it a
  # Friday.
  mutate(wday = wday(start_time - 3 * 60 * 60)) %>%
  # Create a day field for total trips by day
  mutate(day = floor_date(start_time - 3 * 60 * 60,"day")) %>% 
  # Keep out trips that are either Saturday (7) or Sunday (1)
  filter(!(wday %in% c(1,7))) %>% # 2771405 records
  # Keep only trips in the MDT interval
  filter(start_time %within% mdt_int) %>% # 1412399 records
  # Remove holidays (individually - note that %within% does not currently work
  # on a list of time intervals)
  filter(!(start_time %within% mlk |
           start_time %within% pres |
           start_time %within% columbus |
           start_time %within% vets |
           start_time %within% xgiving |
           start_time %within% xmas |
           start_time %within% springb)) %>% # 1285608 records
  
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

# Average Divvy trips per day over the time period
divvy_wip %>% count(day) %>% summarize(avg = mean(n))

# Use function defined in helper_fns.R to create trips in motion graph
trip_times_divvy_counts <-
  tim_calculator(
    data = divvy_wip,
    weights = "weight",
    criteria = "usertype")

# Define breaks
breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 06:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "6 hours")
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
  scale_y_continuous(limits = c(0,500),expand = expansion(mult = c(.05,.01))) +

  # Add CMAP style
  scale_fill_discrete(type = c("#475c66","#ac8c00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Divvy trips")

# Export plot
finalize_plot(divvy_p1,
              title = "Divvy ridership was significantly higher in the afternoon, 
              especially for non-subscribers.",
              caption = "Note: Trips in motion are 25-minute rolling averages.
              \"One-time user\" refers to Divvy customers that purchased a
              single ride or a day pass. Trips that were in motion as of 2:55
              A.M. and ended after 3:00 A.M. are captured on the right side
              of the graph, and are not included in the totals of trips in
              motion as of 3:00 A.M. on the left side of the graph.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of Divvy
              ridership data for weekdays during the My Daily Travel survey
              collection period, between September 4, 2018 and May 9, 2019
              (excluding all federal holidays and the weeks of November 19,
              December 24, December 31, and April 15).",
              filename = "divvy_p1",
              # sidebar_width = 0,
              # caption_align = 1,
              # mode = "png",
              # height = 2.25,
              # width = 8,
              # overrides = list(margin_plot_l = 30),
              overwrite = T)

# Combined plot of Divvy ridership and MDT bike ridership - this requires
# running the code for the MDT bike ridership plot in 'trips_in_motion.R'
bike_p1 <- ggpubr::ggarrange(trips_in_motion_p3,divvy_p1,
                     ncol = 2,nrow = 1)

finalize_plot(bike_p1,
              sidebar_width = 0,
              title = "Personal bike trips peaked in the morning, while Divvy 
              trips peaked in the afternoon.",
              paste0("Note: Trips in motion are 55-minute rolling averages for
              personal bikes and 25-minute rolling averages for Divvy.
              'One-time user' refers to Divvy customers that purchased a
              single ride or a day pass. Personal bike trips anlyzed include
              trips by residents of the CMAP seven county region (Cook, DuPage,
              Kane, Kendall, Lake, McHenry, and Will), as well as Grundy and DeKalb. 
              Includes only trips that were within, to, and/or from one of those 
              counties. Divvy trips that were in motion as of 2:55
              A.M. and ended after 3:00 A.M. are captured on the right side
              of the graph, and are not included in the totals of trips in
              motion as of 3:00 A.M. on the left side of the graph.
              <br><br>
              Sample size: Personal bike trips are based on ",
                     format(nrow(tim_mdt_bike),big.mark = ","),
                     " records. Divvy trips are actual ridership numbers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel survey data and Divvy ridership data for weekdays 
              between September 4, 2018 and May 9, 2019 (excluding all federal 
              holidays and the weeks of November 19, December 24, December 31, 
              and April 15)."),
              filename = "bike_p1",
              mode = "png",
              overwrite = T)

# Count of ridership by day
divvy_wip %>% 
  mutate(day = floor_date(start_time - 3* 60*60, unit = "day")) %>% 
  count(day) %>% 
  summarize(average = mean(n))

# Count of trips by subscriber gender
divvy_wip %>% 
  filter(gender != "") %>% 
  count(gender) %>% 
  mutate(pct = n/sum(n))

####### ARCHIVE OF 2019 HOLIDAYS (FOR FUTURE ANALYSIS)

# mlk <-      interval(ymd_hms("2019-01-21 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-01-22 02:59:59",tz = "America/Chicago"))
# pres <-     interval(ymd_hms("2019-02-18 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-02-19 02:59:59",tz = "America/Chicago"))
# memorial <- interval(ymd_hms("2019-05-27 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-05-28 02:59:59",tz = "America/Chicago"))
# july4 <-    interval(ymd_hms("2019-07-04 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-07-05 02:59:59",tz = "America/Chicago"))
# labor <-    interval(ymd_hms("2019-09-02 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-09-03 02:59:59",tz = "America/Chicago"))
# columbus <- interval(ymd_hms("2019-10-14 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-10-15 02:59:59",tz = "America/Chicago"))
# vets <-     interval(ymd_hms("2019-11-11 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-12 02:59:59",tz = "America/Chicago"))
# xgiving <-  interval(ymd_hms("2019-11-28 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-29 02:59:59",tz = "America/Chicago"))
# blackfri <- interval(ymd_hms("2019-11-29 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-11-30 02:59:59",tz = "America/Chicago"))
# xmas <-     interval(ymd_hms("2019-12-25 03:00:00",tz = "America/Chicago"),
#                      ymd_hms("2019-12-26 02:59:59",tz = "America/Chicago"))
