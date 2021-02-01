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
  mutate(arrtime_pg = ymd_hms(arrtime_pg)) %>%
  # Filter
  filter(
    # Remove trips > 15 hours
    travtime < 15 * 60,        # 125,096 records
    # Remove trips with no travel time
    travtime_pg > 0            # 96,775 records
    ) %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(arrtime_pg,12,19))),
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
  # Mutate to character to allow case_when modification
  mutate(mode_c_school = as.character(mode_c)) %>%
  # Make school bus into its own category
  mutate(mode_c_school = case_when(
    mode == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
  )) %>%
  # Reconvert to factor
  mutate(mode_c_school = factor(mode_c_school)) %>%
  # Create combined mode and purpose
  mutate(mode_tpurp = paste(mode,tpurp,sep = "-")) %>%
  # Create combined mode and purpose category
  mutate(mode_tpurp_c = paste(mode,tpurp_c,sep = "-")) %>%
  # Create combined mode and chain category
  mutate(mode_chain = paste(mode,chain_bucket,sep = "-")) %>%
  # Create combined mode category and chain category
  mutate(mode_c_chain = paste(mode_c,chain_bucket,sep = "-"))



# Extract possible modes and/or purposes
possible_modes <- tibble(identifier = unique(tim_mdt_wip$mode_c))
possible_modes_detailed <- tibble(identifier = unique(tim_mdt_wip$mode))
possible_modes_school <- tibble(identifier = unique(tim_mdt_wip$mode_c_school))
possible_tpurp <- tibble(identifier = unique(tim_mdt_wip$tpurp))
possible_mode_tpurp <- tibble(identifier = unique(tim_mdt_wip$mode_tpurp))
possible_mode_tpurp_c <- tibble(identifier = unique(tim_mdt_wip$mode_tpurp_c))
possible_chains <- tibble(identifier = unique(tim_mdt_wip$chain_bucket))
possible_mode_chains <- tibble(identifier = unique(tim_mdt_wip$mode_chain))
possible_mode_c_chains <- tibble(identifier = unique(tim_mdt_wip$mode_c_chain))

# Helper function to calculate trip times within the intervals that meet given criteria
tim_calculator <- function(possibilities,
                           base_weights = tim_mdt_wip$wtperfin,
                           criteria1 = tim_mdt_wip$trip_interval,
                           criteria2,
                           rolling_interval = 5,
                           rolling_n = 25,
                           crosstab = F,
                           crosstab1 = NULL,
                           crosstab2 = NULL) {

  # Create tibble of all possible trip intervals
  trip_times <-
    # Establish sequence of times over the day (in one minute increments)
    tibble(time_band = seq.POSIXt(
      from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
      to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
      # Interval using input above, defaults to 5
      by = paste0(rolling_interval," min")))

  # Calculate number of intervals on either side of the interval needed for rolling average
  rolling_n_calc <- ((rolling_n / rolling_interval) - 1) / 2

  # Calculate trips in motion within the intervals meeting criteria
  raw_count <-
    # Start with all trip intervals
    trip_times %>%
    # Add all possible modes and/or purposes and/or chains
    full_join(possibilities, by = character()) %>%
    # Sum the weights that meet both criteria
    mutate(count = mapply(function(x,y) sum(base_weights[which(
      x %within% criteria1 &
        y == criteria2)]),
      time_band,
      identifier
    ))

  # If crosstab, need to split identifier and group by both columns. Otherwise, group by identifier.
  if (!crosstab) {
    grouped_count <-
      raw_count %>%
      group_by(identifier)
  } else {
    grouped_count <-
      raw_count %>%
      separate(identifier,into = c(crosstab1,crosstab2) , sep = "-") %>%
      group_by(.data[[crosstab1]],.data[[crosstab2]])
  }

  # Calculate rolling averages
  output <-
    grouped_count %>%
    # Calculate rolling average
    mutate(rolling_count = slide_dbl(count, mean,
                                     .before = rolling_n_calc,
                                     .after = rolling_n_calc)) %>%
    ungroup()

  return(output)

}


trip_times_mode_c_mdt <-
  tim_calculator(possibilities = possible_modes,
                 criteria2 = tim_mdt_wip$mode_c) %>%
  mutate(identifier = factor(identifier,
                             levels = c("driver","passenger","transit","walk",
                                        "bike","other","missing")))


trip_times_mode_c_school_mdt <-
  tim_calculator(possibilities = possible_modes_school,
                 criteria2 = tim_mdt_wip$mode_c_school) %>%
  mutate(identifier = factor(identifier,
                             levels = c("driver","passenger","transit","walk",
                                        "school bus","bike","other","missing")))

trip_times_mode_and_purp_c_mdt_55 <-
  tim_calculator(possibilities = possible_mode_tpurp_c,
                 criteria2 = tim_mdt_wip$mode_tpurp_c,
                 rolling_n = 55,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "tpurp_c")

trip_times_mode_and_purp_c_mdt_25 <-
  tim_calculator(possibilities = possible_mode_tpurp_c,
                 criteria2 = tim_mdt_wip$mode_tpurp_c,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "tpurp_c")

trip_times_chains_mdt <-
  tim_calcuclator(possibilities = possible_chains,
                  criteria2 = tim_mdt_wip$chain_bucket) %>%
  mutate(identifier = factor(identifier,
                             levels = c("Work trip","Return home (work)",
                                        "Shopping trip","Return home (shopping)",
                                        "Other trip")))

trip_times_mode_c_and_chain_mdt_25 <-
  tim_calculator(possibilities = possible_mode_c_chains,
                 criteria2 = tim_mdt_wip$mode_chain,
                 crosstab = T,crosstab1 = "mode_c",crosstab2 = "chain")

trip_times_mode_and_chain_mdt_55 <-
  tim_calculator(possibilities = possible_mode_chains,
                 criteria2 = tim_mdt_wip$mode_chain,
                 rolling_n = 55,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "chain")

#################################################################
#                                                               #
#                        Charts                                 #
#                                                               #
#################################################################

breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")

breaks_less <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "6 hours")

# Graph output of trips in motion by mode
trips_in_motion_p1 <-
  trip_times_mode_c_mdt %>%
  filter(identifier != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = TRUE)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p1,
              "Trips in motion in the CMAP region by mode on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p1",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

# Graph output of trips in motion by mode, with school buses
trips_in_motion_p1a <-
  trip_times_mode_c_school_mdt %>%
  filter(identifier != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = TRUE)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p1a,
              "Trips in motion in the CMAP region by mode on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p1a",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3,
              layout_style = "h")

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
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p2",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

# Graph output of trips in motion by purpose for taxi vs. ride share trips
trips_in_motion_p3 <-
  trip_times_mode_and_purp_c_mdt_55 %>%
  filter(mode == "rideshare" | mode == "shared rideshare" | mode == "taxi") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_wrap(~mode, nrow = 3) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p3,
              "Ride-share and shared ride-share trips in motion by travel
              purpose.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p3",
              # mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


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
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p4",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


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
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p5",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)



# Graph trips in motion by trip chains
trips_in_motion_p6 <-
  trip_times_chains_mdt %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200","#e5b172","#3d6600")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p6,
              "Trips in motion in the CMAP region by trip chain type on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p6",
              # mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

# Graph trips in motion by mode category for work chains
trips_in_motion_p7 <-
  trip_times_mode_c_and_chain_mdt_25 %>%
  filter(mode_c != "missing") %>%
  mutate(category = fct_collapse(chain,
    Work = c("Work trip","Return home (work)"),
    Other = "Other trip",
    Shopping = c("Shopping trip","Return home (shopping)"))) %>%
  mutate(category = factor(category, levels = c("Work","Shopping","Other"))) %>%
  group_by(time_band,mode_c,category) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c), position = position_stack(reverse = TRUE)) +
  facet_wrap(~category, ncol = 1) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 10)

finalize_plot(trips_in_motion_p7,
              "Trips in motion in the CMAP region by trip chain type on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p7",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


##### Ride share and taxis, by chain

# Graph trips in motion by mode category for work chains
trips_in_motion_p8 <-
  trip_times_mode_and_chain_mdt_55 %>%
  filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
  # mutate(category = fct_collapse(chain_bucket,
  #                                Work = c("Work trip","Return home (work)"),
  #                                Other = "Other trip",
  #                                Shopping = c("Shopping trip","Return home (shopping)"))) %>%
  # mutate(category = factor(category, levels = c("Work","Shopping","Other"))) %>%
  group_by(time_band,mode,chain_bucket) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain_bucket), position = position_stack(reverse = TRUE)) +
  facet_wrap(~mode, ncol = 1) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200","#e5b172","#3d6600")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 4)

finalize_plot(trips_in_motion_p8,
              "Trips in motion in TNCs and taxis in the CMAP region by trip
              chain type on weekdays.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p8",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


#### Investigation of "drive thru / takeout dining"

tim_mdt_drivethru <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp == "Drive thru / take-out dining")

trip_times_drivethru_and_mode_mdt <-
  tim_calculator(possibilities = possible_modes,
                 base_weights = tim_mdt_drivethru$wtperfin,
                 criteria1 = tim_mdt_drivethru$trip_interval,
                 criteria2 = tim_mdt_drivethru$mode_c)

trips_in_motion_p9 <-
  trip_times_drivethru_and_mode_mdt %>%
  filter(identifier != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = reorder(identifier,desc(identifier)))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "friday") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p9,
              "Drive-thru / take-out trips in motion by time of day on weekdays.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.")

