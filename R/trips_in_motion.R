# This script allows for the creation of "trips in motion" analyses of the MDT
# trip diary survey data.

#################################################
#                                               #
#                 Library loading               #
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
source("R/data_cleaning.R")
source("R/helper_fns.R")

# Create helper values for date identification and modification
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24

# Filter and process data into working file for calculations
tim_mdt_wip <-
  mdt %>%        # 125463 records
  # Remove beginning trips
  filter(mode != "beginning") %>% # 97374 records
  # Manually calculate trip beginning times for remaining NA trips with nonzero
  # travel time (36 records)
  mutate(start_time_calc = arrtime_pg - 60 * travtime_pg) %>%
  mutate(start_times_pg = case_when(
    is.na(start_times_pg) ~ start_time_calc,
    TRUE ~ start_times_pg)) %>%
  select(-start_time_calc) %>%
  # Remove trip with departure time in 1900
  filter(start_times_pg > ymd_hms("2017-01-01 00:00:00") # 97373 records
  ) %>%
  # Remove all trips where the arrival time comes before the start time (there
  # are 10 records where this is a problem)
  filter(start_times_pg <= arrtime_pg) %>% # 97363 records
  # Add calculated travel time for validation
  mutate(travtime_calc = (arrtime_pg - start_times_pg)/60) %>%
  # Filter out unwanted trips
  filter(
    # Remove trips > 15 hours
    travtime_calc < 15 * 60,     # 97355 records
    # Remove trips with zero travel time
    travtime_calc > 0,           # 97331 records
    # Remove trips with 0 place group distance
    distance_pg > 0              # 97273 records
  ) %>%
  # Exclude improbable walk trips
  filter(improbable_walk == 0) %>% 
  
  # Make every trip on the same day (for analysis and graphing). I used January
  # 1, 2020 (arbitrarily). The code below extracts the time element of the
  # date-time object and replaces the date with 1/1/20.
  mutate(
    # Trips end at the arrival time of the record
    trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",
                                       substr(arrtime_pg,12,19))),
                        tzone = "America/Chicago"),
    # Trips begin at the departure time of the previous record, which has been
    # added here as start_times_pg
    trip_start = force_tz(ymd_hms(paste0("2020-01-01 ",
                                         substr(start_times_pg,12,19))),
                          tzone = "America/Chicago")) %>%
  
  # Since we have made every trip onto the same day, but the survey window was
  # actually from 3am to 3am, we need to make times between midnight and 3am
  # into trips on the next day.
  mutate(
    trip_end = case_when(
      trip_end < threshold ~ trip_end + day_value,
      TRUE ~ trip_end
    ),
    trip_start = case_when(
      trip_start < threshold ~ trip_start + day_value,
      TRUE ~ trip_start
    )) %>%
  # Create trip interval - this can be used to identify whether a trip is within
  # a given time interval
  mutate(trip_interval = lubridate::interval(trip_start,trip_end,
                                             tz = "America/Chicago"))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################


################################################################################
# Use the function we loaded to calculate the rolling averages as needed
################################################################################

# Trips in motion by mode category (25 minute rolling average)
trip_times_mode_c_mdt <-
  tim_calculator(data = tim_mdt_wip,
                 criteria = "mode_c",
                 weights = "wtperfin")

# Trips in motion by trip chain and mode category (25 minute rolling average)
trip_times_mode_c_and_chain_c_mdt_25 <-
  tim_calculator(data = tim_mdt_wip,
                 criteria = c("mode_c","chain_c"),
                 weights = "wtperfin")

#################################################################
#                                                               #
#                        Charts                                 #
#                                                               #
#################################################################

# Set breaks for charts (3 hour intervals)
tim_breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")

# Create 6-hour interval option
tim_breaks_less <- tim_breaks[c(2,4,6,8)]

################################################################################
# Overall
################################################################################

# Graph output of trips in motion by mode
trips_in_motion_p1 <-
  # Get data
  trip_times_mode_c_mdt %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Capitalize
  mutate(mode_c = recode_factor(mode_c,
                                    "driver" = "Driver",
                                    "passenger" = "Passenger",
                                    "walk" = "Walk",
                                    "transit" = "Transit",
                                    "bike" = "Bike",
                                    "schoolbus" = "School bus",
                                    "other" = "Other")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c), position = position_stack(reverse = TRUE)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#36d8ca","#6d8692",
                               "#efa7a7","#3d6600","#0084ac")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 7,
             xlab = "Weekday trips in motion by time of day")

finalize_plot(trips_in_motion_p1,
              "The morning and evening peaks in travel demand were very 
              pronounced, although the COVID-19 pandemic's impact on these 
              travel patterns remains uncertain.",
              paste0(
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include weekday trips by residents age 5 and older 
              of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              Excludes trips longer than 100 miles or greater than 15 hours long.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(tim_mdt_wip),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries."),
              filename = "trips_in_motion_p1",
              mode = c("png","pdf"),
              # sidebar_width = 2.3,
              overwrite = TRUE,
              # height = 4.5,
              # width = 8
              )

# Backup - trips in motion by mode, faceted (for prose)

# Graph output of trips in motion by mode
trip_times_mode_c_mdt %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Capitalize
  mutate(mode_c = recode_factor(mode_c,
                                    "driver" = "Driver",
                                    "passenger" = "Passenger",
                                    "walk" = "Walk",
                                    "transit" = "Transit",
                                    "bike" = "Bike",
                                    "schoolbus" = "School bus",
                                    "other" = "Other")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver()) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#36d8ca","#6d8692",
                               "#efa7a7","#3d6600","#0084ac")) +
  
  # Add faceting 
  facet_wrap(~mode_c,ncol = 2,scales = "free_y") +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 7,
             xlab = "Weekday trips in motion by time of day")

################################################################################
# Trips in motion by trip chains and mode
################################################################################

trips_in_motion_p2 <-
  # Get data
  trip_times_mode_c_and_chain_c_mdt_25 %>%
  # Factor for ordering
  mutate(mode_c = recode_factor(factor(mode_c, levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "walk" = "Walk",
                                "transit" = "Transit",
                                "bike" = "Bike",
                                "schoolbus" = "School bus",
                                "other" = "Other")) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Capitalize for presentation
  mutate(chain_c = recode_factor(chain_c,
                                 "work" = "Work",
                                 "shop" = "Shopping",
                                 "other" = "Other")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c), position = position_stack(reverse = TRUE)) +
  
  # Add facets
  facet_wrap(~chain_c, ncol = 1) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#36d8ca","#6d8692",
                               "#efa7a7","#3d6600","#0084ac")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray",),
             legend.max.columns = 7,
             strip.text = element_text(hjust = 0.5,face = "bold",vjust = 1),
             xlab = "Weekday trips in motion by time of day and trip chain type")

trips_in_motion_p2_samplesize <-
  tim_mdt_wip %>% 
  ungroup() %>% 
  count(chain_c)

finalize_plot(trips_in_motion_p2,
              "Travelers relied on substantially different modes for trips to and 
              from work vs. other trip purposes.",
              paste0(
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include weekday trips by residents age 5 and older 
              of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              Excludes trips longer than 100 miles or greater than 15 hours long.
              <br><br>
              Sample size: 
              <br>- Work (",
                     trips_in_motion_p2_samplesize %>% filter(chain_c == "work") %>% select(n),
                     "); 
              <br>- Shopping (",
                     trips_in_motion_p2_samplesize %>% filter(chain_c == "shop") %>% select(n),
                     "); 
              <br>- Other (",
                     trips_in_motion_p2_samplesize %>% filter(chain_c == "other") %>% select(n),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries."),
              filename = "trips_in_motion_p2",
              # mode = c("png","pdf"),
              overwrite = TRUE)

################################################################################
# Bikes (personal)
################################################################################

tim_mdt_bike <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(mode == "personal bike") %>% 
  # Recode chains to just be work and other
  mutate(chain = recode_factor(chain,
                               "Work trip" = "Work trip",
                               "Return home (work)" = "Work trip",
                               "Shopping trip" = "Other trip",
                               "Return home (shopping)" = "Other trip",
                               "Other trip" = "Other trip")) 

trip_times_bike_and_chain_mdt <-
  tim_calculator(data = tim_mdt_bike,
                 weights = "wtperfin",
                 criteria = "chain",
                 rolling_window = 55)

# Graph output of trips in motion by purpose for bike trips (personal bike only)
trips_in_motion_p3 <-
  # Get data
  trip_times_bike_and_chain_mdt %>%
  # Sort chains for ordering
  # mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
  #                                         "Shopping trip",
  #                                         "Return home (shopping)",
  #                                         "Other trip"))) %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = tim_breaks_less) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Add colors
  scale_fill_discrete(type = c("#72cae5","#3d6600")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",legend.max.columns = 3,
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Personal bike trips")

finalize_plot(trips_in_motion_p3,
              "Personal bike usage has a strong morning peak, with PM usage spread more evenly across the afternoon and evening.",
              "Note: Trips in motion are 55-minute rolling averages. Trips 
              analyzed include weekday trips by residents age 5 and older 
              of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              Excludes trips longer than 100 miles or greater than 15 hours long.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p3",
              # mode = c("png","pdf"),
              overwrite = TRUE,
              # height = 2.25,
              # width = 8
              )