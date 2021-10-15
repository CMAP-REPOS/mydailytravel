# This script downloads and analyzes Divvy ridership data from the same time
# period as the My Daily Travel survey to produce a trips-in-motion chart for
# Divvy trips over the course of an average weekday. It is referenced in Policy
# Brief #4.

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(tidyverse)
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
source("R/mdt_dates.R")

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


# Create helper values

# 3am day threshold (since our day starts at 3am)
threshold_divvy <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
# The number of seconds in a day (used to add days)
day_value <- 60*60*24


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
    trip_start < threshold_divvy ~ trip_start + day_value,
    TRUE ~ trip_start)
  ) %>%
  # Add trip end based on trip duration
  mutate(trip_end = trip_start + trip_time) %>%
  # Create trip interval using the lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Add weight of 1 divided by the number of weekdays for summing the average TIMs
  mutate(weight = 1/number_of_weekdays)

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
# Plot of Divvy ridership
################################################################################

# Use function defined in helper_fns.R to create trips in motion graph
trip_times_divvy_counts <-
  tim_calculator(
    data = divvy_wip,
    weights = "weight",
    criteria = "usertype")

# Define breaks
divvy_breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours"
                     )
# Create chart
figure4_6 <-
  # Get data
  trip_times_divvy_counts %>%
  # Relevel user type
  mutate(usertype = recode_factor(factor(usertype, levels = c("Subscriber",
                                                                  "Customer")),
                                    "Subscriber" = "Subscriber",
                                    "Customer" = "One-time user")) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count, fill = usertype)) +
  geom_area(position = position_stack(reverse = TRUE)) +
  
  # Reformat axes
  scale_x_datetime(labels = scales::date_format("%l%P", # Time without leading zero
                                                tz = "America/Chicago"),
                    breaks = divvy_breaks) +

  # Add CMAP style
  scale_fill_discrete(type = c("#38B2D8","#D88134")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Time of day",
             ylab = "Weekday Divvy trips in motion"
             )

# Export plot
finalize_plot(figure4_6,
              title = "Divvy ridership was significantly higher in the afternoon
              peak, especially for non-subscribers.",
              caption = "Note: Trips in motion are 25-minute rolling averages.
              \"One-time user\" refers to Divvy customers that purchased a
              single ride or a day pass. Trips that were in motion as of 2:55 a.m. 
              and ended after 3:00 a.m. are captured only on the right side
              of the graph.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of Divvy
              ridership data for weekdays between Sept. 4, 2018, and May 9, 2019,
              excluding all federal holidays and the weeks of Nov. 19,
              Dec. 24, Dec. 31, and Apr. 15.",
              filename = "figure4_6",
              height = 4,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Backup
################################################################################

# Backup - Count of ridership by day
divvy_wip %>%
  count(day) %>%
  summarize(average = mean(n))

