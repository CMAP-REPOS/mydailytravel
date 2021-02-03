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
                 criteria2 = tim_mdt_wip$mode_c)

trip_times_mode_and_purp_c_mdt_25 <-
  tim_calculator(possibilities = possible_mode_tpurp_c,
                 criteria2 = tim_mdt_wip$mode_tpurp_c,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "tpurp_c")

trip_times_mode_and_purp_c_mdt_55 <-
  tim_calculator(possibilities = possible_mode_tpurp_c,
                 criteria2 = tim_mdt_wip$mode_tpurp_c,
                 rolling_n = 55,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "tpurp_c")

trip_times_chains_mdt <-
  tim_calculator(possibilities = possible_chains,
                  criteria2 = tim_mdt_wip$chain_bucket)

trip_times_mode_c_and_chain_mdt_25 <-
  tim_calculator(possibilities = possible_mode_c_chains,
                 criteria2 = tim_mdt_wip$mode_c_chain,
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

#################################################################
#                                                               #
#                        Overall                                #
#                                                               #
#################################################################

# Graph output of trips in motion by mode
trips_in_motion_p1 <-
  trip_times_mode_c_mdt %>%
  filter(identifier != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = TRUE)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#a7efe8","#6d8692","#0084ac","#efa7a7","#67ac00")) +
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
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

# Graph trips in motion by mode category, faceting for different chain types
trips_in_motion_p7 <-
  trip_times_mode_c_and_chain_mdt_25 %>%
  mutate(mode_c = factor(mode_c, levels = mode_c_levels)) %>%
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
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#a7efe8","#6d8692",
                               "#0084ac","#efa7a7","#67ac00")) +
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

#################################################################
#                                                               #
#                        Bikes                                  #
#                                                               #
#################################################################

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

# Graph output of trips in motion by purpose for bike trips (personal bike only)
trips_in_motion_p4 <-
  trip_times_mode_and_chain_mdt_55 %>%
  mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
                                          "Shopping trip","Return home (shopping)",
                                          "Other trip"))) %>%
  filter(mode == "personal bike") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200","#e5b172","#3d6600")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p4,
              "Bicycling trips in motion by travel purpose.",
              "Note: Excludes bike share due to limited sample size. Trips in motion
              are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p4",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


# Split into two groups for comparison
tim_mdt_bike <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(mode %in% c("personal bike")) %>%
  group_by(sampno,perno,placeno,placeGroup) %>%
  mutate(n = 1,
         random = sample(1:2,size = 1))

tim_mdt_bike_set1 <-
  tim_mdt_bike %>%
  filter(random == 1)

tim_mdt_bike_set2 <-
  tim_mdt_bike %>%
  filter(random == 2)

trip_times_bike_chain_mdt_split <-
  tim_calculator(possibilities = possible_mode_chains,
                 base_weights = tim_mdt_bike_set1$wtperfin,
                 criteria1 = tim_mdt_bike_set1$trip_interval,
                 criteria2 = tim_mdt_bike_set1$mode_chain,
                 rolling_n = 55,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
  mutate(set = 1) %>%
  rbind(tim_calculator(possibilities = possible_mode_chains,
                       base_weights = tim_mdt_bike_set2$wtperfin,
                       criteria1 = tim_mdt_bike_set2$trip_interval,
                       criteria2 = tim_mdt_bike_set2$mode_chain,
                       rolling_n = 55,
                       crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
          mutate(set = 2))


trips_in_motion_p4_split <-
  trip_times_bike_chain_mdt_split %>%
  mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
                                          "Shopping trip","Return home (shopping)",
                                          "Other trip"))) %>%
  filter(mode == "personal bike") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_wrap(~set) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200","#e5b172","#3d6600")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p4_split,
              "Bicycling trips in motion by travel purpose.",
              "Note: Excludes bike share due to limited sample size. Trips in motion
              are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p4_split",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

#################################################################
#                                                               #
#                        TNC/taxi                               #
#                                                               #
#################################################################

# Graph output of trips in motion by purpose for taxi vs. ride share trips
trips_in_motion_p3 <-
  trip_times_mode_and_purp_c_mdt_55 %>%
  filter(mode %in% c("rideshare","shared rideshare","taxi"),
         tpurp_c != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c), position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_wrap(~mode, nrow = 3) +
  scale_fill_discrete(type = c("#00becc","#cc5f00","#3f0e00","#cca600","#003f8c",
                               "#67ac00","#006b8c","#efa7a7","#8c4100","#00303f",
                               "#a7efe8")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p3,
              "Ride-share and shared ride-share trips in motion by travel
              purpose.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p3",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)


##### Ride share and taxis, by chain

# Graph trips in motion by mode category for work chains
trips_in_motion_p8 <-
  trip_times_mode_and_chain_mdt_55 %>%
  mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
                                          "Shopping trip","Return home (shopping)",
                                          "Other trip"))) %>%
  filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
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

# Unweighted
tim_mdt_tnc_taxi <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
  group_by(sampno,perno,placeno,placeGroup) %>%
  mutate(n = 1,
         random = sample(1:2,size = 1))

tim_mdt_tnc_taxi_set1 <-
  tim_mdt_tnc_taxi %>%
  filter(random == 1)

tim_mdt_tnc_taxi_set2 <-
  tim_mdt_tnc_taxi %>%
  filter(random == 2)

trip_times_tnc_taxi_and_chain_mdt_unweighted <-
  tim_calculator(possibilities = possible_mode_chains,
                 base_weights = tim_mdt_tnc_taxi_set1$wtperfin,
                 criteria1 = tim_mdt_tnc_taxi_set1$trip_interval,
                 criteria2 = tim_mdt_tnc_taxi_set1$mode_chain,
                 rolling_n = 55,
                 crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
  mutate(set = 1) %>%
  rbind(tim_calculator(possibilities = possible_mode_chains,
                       base_weights = tim_mdt_tnc_taxi_set2$wtperfin,
                       criteria1 = tim_mdt_tnc_taxi_set2$trip_interval,
                       criteria2 = tim_mdt_tnc_taxi_set2$mode_chain,
                       rolling_n = 55,
                       crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
          mutate(set = 2))

# Graph trips in motion by mode category for work chains
trips_in_motion_p8_unweighted <-
  trip_times_tnc_taxi_and_chain_mdt_unweighted %>%
  mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
                                          "Shopping trip","Return home (shopping)",
                                          "Other trip"))) %>%
  filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
  facet_wrap(set~mode) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks[c(2,4,6,8,10)]) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200","#e5b172","#3d6600")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 4)

finalize_plot(trips_in_motion_p8_unweighted,
              title_width = 0,
              filename = "trips_in_motion_p8_unweighted",
              mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 8.3)

#################################################################
#                                                               #
#                        Transit                                #
#                                                               #
#################################################################


# Graph output of trips in motion by purpose for transit trips
trips_in_motion_p5 <-
  trip_times_mode_and_purp_c_mdt_25 %>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels)) %>%
  filter(mode %in% c("rail and bus", "bus", "train", "local transit", "transit"),
         tpurp_c != "missing") %>%
  group_by(time_band,tpurp_c) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c),position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  scale_fill_discrete(type = c("#00becc","#cc5f00","#3f0e00","#cca600","#003f8c",
                               "#67ac00","#006b8c","#efa7a7","#8c4100","#00303f",
                               "#a7efe8")) +
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


#################################################################
#                                                               #
#                        Drive thru                             #
#                                                               #
#################################################################

tim_mdt_drivethru <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp == "Drive thru / take-out dining")

trip_times_drivethru_and_mode_mdt <-
  tim_calculator(possibilities = possible_modes,
                 base_weights = tim_mdt_drivethru$wtperfin,
                 criteria1 = tim_mdt_drivethru$trip_interval,
                 criteria2 = tim_mdt_drivethru$mode_c,
                 rolling_n = 55)

trips_in_motion_p9 <-
  trip_times_drivethru_and_mode_mdt %>%
  # Collapse school buses into other and sum
  mutate(mode_c = as.character(identifier)) %>%
  mutate(mode_c = case_when(
    identifier == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  group_by(mode_c,time_band) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c),position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#a7efe8","#6d8692",
                               "#0084ac","#efa7a7","#67ac00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 5)

finalize_plot(trips_in_motion_p9,
              "Drive-thru / take-out trips in motion by time of day on weekdays.",
              "Note: Trips in motion are 55-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              height = 6.3,
              width = 11.3,
              mode = "png",
              filename = "trips_in_motion_p9",
              overwrite = T)
