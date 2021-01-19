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

source("data_cleaning.R")

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

# Create helper values
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24


# Process data
tim_mdt_wip <-
  mdt %>%                      # 125,103 records
  # Convert to datetime object
  mutate(arrtime = ymd_hms(arrtime)) %>%
  # Filter
  filter(
    # Remove trips > 15 hours
    travtime < 15 * 60,        # 125,096 records
    # Remove trips with no travel time
    travtime_pg > 0            # 96,775 records
    ) %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(arrtime,12,19))),
                             tzone = "America/Chicago")) %>%
  # Make trips that end before 3am into trips on the next day (given survey timing)
  mutate(trip_end = case_when(
    trip_end <= threshold ~ trip_end + day_value,
    TRUE ~ trip_end)
  ) %>%
  # Calculate trip start time as end time minus travel time
  mutate(trip_start = trip_end - 60*travtime) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Create combined sample and person number
  mutate(samp_per_no = paste0(sampno,perno)) %>%
  # Create combined mode and purpose
  mutate(mode_tpurp = paste(mode,tpurp,sep = "_")) %>%
  # Create combined mode and purpose category
  mutate(mode_tpurp_c = paste(mode,tpurp_c,sep = "_")) %>%
  # Create combined mode and chain category
  mutate(mode_chain = paste(mode,chain_bucket,sep = "_"))



# Extract possible modes
possible_modes <- tibble(mode_c = unique(tim_mdt_wip$mode_c))
possible_modes_detailed <- tibble(mode = unique(tim_mdt_wip$mode))
possible_tpurp <- tibble(tpurp = unique(tim_mdt_wip$tpurp))
possible_mode_tpurp <- tibble(mode_tpurp = unique(tim_mdt_wip$mode_tpurp))
possible_mode_tpurp_c <- tibble(mode_tpurp_c = unique(tim_mdt_wip$mode_tpurp_c))
possible_chains <- tibble(chain_bucket = unique(tim_mdt_wip$chain_bucket))
possible_mode_chains <- tibble(mode_chain_bucket = unique(tim_mdt_wip$mode_chain))

# Calculate trips in motion by mode
trip_times <-
  # Establish sequence of times over the day (in one minute increments)
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "5 min"))

trip_times_mode_c_mdt <-
  trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_modes, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode category)
  mutate(mode_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_c)]),
    time_band,
    mode_c
  )) %>%
  group_by(mode_c) %>%
  # Calculate rolling average
  mutate(rolling_mode_count = slide_dbl(mode_count, mean, .before = 2, .after = 2)) %>%
  ungroup()


### Mode and purpose
trip_times_mode_and_purp_c_mdt_55 <-
  trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_mode_tpurp_c, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_tpurp_c)]),
    time_band,
    mode_tpurp_c
  )) %>%
  separate(col = mode_tpurp_c, into = c("mode","tpurp_c"), sep = "_") %>%
  group_by(mode,tpurp_c) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 5, .after = 5)) %>%
  ungroup()


trip_times_mode_and_purp_c_mdt_25 <-
  trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_mode_tpurp_c, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_tpurp_c)]),
    time_band,
    mode_tpurp_c
  )) %>%
  separate(col = mode_tpurp_c, into = c("mode","tpurp_c"), sep = "_") %>%
  group_by(mode,tpurp_c) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 2, .after = 2)) %>%
  ungroup()


#### Group by trip chain buckets
trip_times_chains_mdt <-
  trip_times %>%
  full_join(possible_chains, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct bucket)
  mutate(bucket_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$chain_bucket)]),
    time_band,
    chain_bucket
  )) %>%
  group_by(chain_bucket) %>%
  # Calculate rolling average
  mutate(rolling_bucket_count = slide_dbl(bucket_count, mean, .before = 2, .after = 2)) %>%
  ungroup()

# Chain status and mode category
trip_times_mode_and_chain_mdt_25 <-
  trip_times %>%
  # Add all possible modes to each time
  full_join(possible_mode_chains, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_chain)]),
    time_band,
    mode_chain_bucket
  )) %>%
  separate(col = mode_chain_bucket, into = c("mode","chain_bucket"), sep = "_") %>%
  group_by(mode,chain_bucket) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 2, .after = 2)) %>%
  ungroup()

source("recoding.R")

trip_times_mode_c_and_chain_mdt_25 <-
  trip_times_mode_and_chain_mdt_25 %>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt)) %>%
  group_by(mode_c,time_band,chain_bucket) %>%
  summarize(rolling_count = sum(rolling_count))



#################################################################
#                                                               #
#                        Charts                                 #
#                                                               #
#################################################################

breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")

# Graph output of trips in motion by mode
trips_in_motion_p1 <-
  trip_times_mode_c_mdt %>%
  filter(mode_c != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_mode_count)) +
  geom_area(aes(fill = mode_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p1,
              "Trips in motion in the CMAP region by mode on weekdays.",
              "Source: CMAP analysis of My Daily Travel survey.")



# Graph output of trips in motion by purpose for bike trips
trips_in_motion_p2 <-
  trip_times_mode_and_purp_c_mdt_55 %>%
  filter(mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p2,
              "Bike share trips in motion by travel purpose.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")

# Graph output of trips in motion by purpose for bike trips
trips_in_motion_p3 <-
  trip_times_mode_and_purp_c_mdt_55 %>%
  filter(mode == "rideshare" | mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks[c(2,4,6,8)]) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_grid(~mode) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p3,
              "TNC and bike share trips in motion by travel purpose.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")


# Graph output of trips in motion by purpose for bike trips
trips_in_motion_p4 <-
  trip_times_mode_and_purp_c_mdt_55 %>%
  filter(mode == "personal bike" | mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks[c(2,4,6,8)]) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_grid(~mode) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p4,
              "Bicycling trips in motion by travel purpose.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")


# Graph output of trips in motion by purpose for transit trips
trips_in_motion_p5 <-
  trip_times_mode_and_purp_c_mdt_25 %>%
  filter(mode %in% c("rail and bus", "bus", "train", "local transit", "transit")) %>%
  group_by(time_band,tpurp_c) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p5,
              "Transit trips in motion by travel purpose.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")



# Graph trips in motion by trip chains
trips_in_motion_p6 <-
  trip_times_chains_mdt %>%
  ggplot(aes(x = time_band,y = rolling_bucket_count)) +
  geom_area(aes(fill = reorder(chain_bucket,desc(chain_bucket)))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "friday") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p6,
              "Trips in motion in the CMAP region by trip chain type on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")

# Graph trips in motion by mode category for work chains
trips_in_motion_p7 <-
  trip_times_mode_c_and_chain_mdt_25 %>%
  filter(chain_bucket %in% c("Work trip","Return home (work)")) %>%
  group_by(time_band,mode_c) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "friday") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p7,
              "Trips in motion in the CMAP region by trip chain type on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")


#### Investigation of "drive thru / takeout dining"

tim_mdt_drivethru <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp == "Drive thru / take-out dining")

trip_times_drivethru_and_mode_mdt <-
  trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_modes, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(tim_mdt_drivethru$wtperfin[which(
    x %within% tim_mdt_drivethru$trip_interval &
      y == tim_mdt_drivethru$mode_c)]),
    time_band,
    mode_c
  )) %>%
  group_by(mode_c) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 5, .after = 5)) %>%
  ungroup()



trips_in_motion_p8 <-
  trip_times_drivethru_and_mode_mdt %>%
  filter(mode_c != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = reorder(mode_c,desc(mode_c)))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "friday") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p8,
              "Drive-thru / take-out trips in motion by time of day on weekdays.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")

