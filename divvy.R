library(ggplot2)
library(lubridate)
library(tidyverse)
library(slider)
library(cmapplot)




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
breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")


divvy_wip <-
  divvy %>%
  # Convert to datetime object
  mutate(start_time = ymd_hms(start_time),
         end_time = ymd_hms(end_time)) %>%
  mutate(trip_time = end_time - start_time) %>%
  # Exclude trips that are on weekends (with days running from 3am to 3am)
  mutate(wday = wday(start_time - 3 * 60 * 60)) %>%
  # Keep out trips that are either Saturday or Sunday
  filter(!(wday %in% c(1,7))) %>%
  # Exclude trips > 3 hours
  filter(trip_time <= 60 * 60 * 3) %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_start = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(start_time,12,19))),
                             tzone = "America/Chicago")) %>%
  # Make trips that start before 3am into trips on the next day (given survey timing)
  mutate(trip_start = case_when(
    trip_start <= threshold ~ trip_start + day_value,
    TRUE ~ trip_start)
  ) %>%
  # Add trip end based on trip duration
  mutate(trip_end = trip_start + trip_time) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago"))


# Identify number of weekdays in the sample
number_of_weekdays <-
  sum(!weekdays(seq(ymd("2019-07-01"),
                    ymd("2019-09-30"),
                    by = "days")) %in% c("Saturday", "Sunday")) -
  1 - # July 4
  1   # Labor Day


# Calculate trips in motion by mode
trip_times_divvy <-
  # Establish sequence of times over the day (in one minute increments)
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "5 min"))

# Calculate rolling averages by time band
trip_times_divvy_counts <-
  trip_times_divvy %>%
  # Join trip times to the two different types of users
  full_join(tibble(usertype = c("Subscriber","Customer")),by = character()) %>%
  # Calculate the number of trips that meet the criteria
  mutate(trip_count = mapply(function(x, y)
    nrow(divvy_wip[which(
      # Is it in the time interval?
      x %within% divvy_wip$trip_interval &
        # Is it the correct user type?
        y == divvy_wip$usertype),]),
    time_band,
    usertype
  )) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 2, .after = 2)) %>%
  # Divide rolling average by number of weekdays in the sample
  mutate(rolling_count_avg = rolling_count / number_of_weekdays)

# Create chart
divvy_p1 <-
  trip_times_divvy_counts %>%
  # Relevel user type
  mutate(usertype = factor(usertype, levels = c("Subscriber","Customer"))) %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count_avg, fill = usertype)) +
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
              title = "Divvy trips in motion on an average weekday, by customer type.",
              caption = "Note: Trips in motion are 25-minute rolling averages.
              \"Customer\" means non-subsciber. Analysis is based on total Divvy
              ridership data from Q3 2019, which had a total of 64 non-holiday
              weekdays.
              <br><br>
              Source: CMAP analysis of Q3 2019 Divvy ridership data.",
              filename = "divvy_p1",
              mode = "png",
              height = 6.3,
              width = 11.3,
              overwrite = T)
