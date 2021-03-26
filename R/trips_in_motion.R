# This script allows for the creation of "trips in motion" analyses of the MDT
# trip diary survey data. It includes a helper function that is also applied to
# Divvy data to generate analogous charts.

#################################################
#                                               #
#                 Library loading               #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")

library(tidyverse) # A package that enables new data manipulation functionality (and a broader "grammar" for R)
library(ggplot2) # A graphing package
library(lubridate) # A date handling package
library(slider) # A package that allows rolling average calculation
library(cmapplot) # CMAP's custom style package (an extension of ggplot)

source("R/helper_fns.R")

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

################################################################################
# Use the function we loaded to calculate the rolling averages as needed
################################################################################

# Trips in motion by mode category (25 minute rolling average)
trip_times_mode_c_mdt <-
  tim_calculator(data = tim_mdt_wip,
                 criteria = "mode_c",
                 weights = "wtperfin")

# # Trips in motion by detailed mode and trip purpose category (25 minute rolling
# # average)
# trip_times_mode_and_purp_c_mdt_25 <-
#   tim_calculator(data = tim_mdt_wip,
#                  criteria = "mode",
#                  criteria2 = "tpurp_c",
#                  weights = "wtperfin")
# 
# # Trips in motion by detailed mode and trip purpose category (55 minute rolling
# # average)
# trip_times_mode_and_purp_c_mdt_55 <-
#   tim_calculator(data = tim_mdt_wip,
#                  criteria = "mode",
#                  criteria2 = "tpurp_c",
#                  rolling_window = 55,
#                  weights = "wtperfin")

# Trips in motion by trip chain (25 minute rolling average)
trip_times_chains_mdt <-
  tim_calculator(data = tim_mdt_wip,
                 criteria = "chain",
                 weights = "wtperfin")

# Trips in motion by trip chain and mode category (25 minute rolling average)
trip_times_mode_c_and_chain_mdt_25 <-
  tim_calculator(data = tim_mdt_wip,
                 criteria = "mode_c",
                 criteria2 = "chain",
                 weights = "wtperfin")

# # Trips in motion by trip chain and mode category (55 minute rolling average)
# trip_times_mode_and_chain_mdt_55 <-
#   tim_calculator(data = tim_mdt_wip,
#                  criteria = "mode",
#                  criteria2 = "chain",
#                  rolling_window = 55,
#                  weights = "wtperfin")

#################################################################
#                                                               #
#                        Charts                                 #
#                                                               #
#################################################################

# Set breaks for charts (3 hour intervals)
breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")

# Create 6-hour interval option
breaks_less <- breaks[c(2,4,6,8)]

################################################################################
# Overall
################################################################################

# Graph output of trips in motion by mode
trips_in_motion_p1 <-
  # Get data
  trip_times_mode_c_mdt %>%
  # Remove missing modes
  filter(identifier != "missing") %>%
  # Capitalize
  mutate(identifier = recode_factor(identifier,
                                    "driver" = "Driver",
                                    "passenger" = "Passenger",
                                    "transit" = "Transit",
                                    "walk" = "Walk",
                                    "bike" = "Bike",
                                    "schoolbus" = "School bus",
                                    "other" = "Other")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = TRUE)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#3d6600","#efa7a7")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Weekday trips in motion by time of day")

finalize_plot(trips_in_motion_p1,
              "The morning and evening peaks in travel demand are very 
              pronounced, although the COVID-19 pandemic's impact on these 
              travel patterns remains uncertain.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all weekday trips by residents of the region that
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p1",
              mode = "png",
              overwrite = TRUE,
              # height = 6.3,
              # width = 11.3
              )

# Graph trips in motion by trip chains
trips_in_motion_p2 <-
  # Get data
  trip_times_chains_mdt %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
                               "#e5b172","#3d6600")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(trips_in_motion_p2,
              "Trips in motion in northeastern Illinois by trip chain type on weekdays, 
              2019.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p2",
              # mode = "png",
              overwrite = TRUE,
              # height = 6.3,
              # width = 11.3
              )

# Graph trips in motion by mode category, faceting for different chain types
trips_in_motion_p3 <-
  # Get data
  trip_times_mode_c_and_chain_mdt_25 %>%
  # Factor for ordering
  mutate(mode_c = recode_factor(factor(mode_c, levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bike",
                                "schoolbus" = "School bus",
                                "other" = "Other")) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Simplify categories for presentation
  mutate(category = fct_collapse(chain,
                                 Work = c("Work trip","Return home (work)"),
                                 Other = "Other trip",
                                 Shopping = c("Shopping trip",
                                              "Return home (shopping)"))) %>%
  # Factor for ordering
  mutate(category = factor(category, levels = c("Work","Shopping","Other"))) %>%
  # Summarize by new buckets
  group_by(time_band,mode_c,category) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c), position = position_stack(reverse = TRUE)) +
  
  # Add facets
  facet_wrap(~category, ncol = 1) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#3d6600","#efa7a7")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 7,
             xlab = "Weekday trips in motion by time of day and trip chain type")

finalize_plot(trips_in_motion_p3,
              "Travelers rely on substantially different modes for trips to and 
              from work vs. other trip purposes.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p3",
              mode = "png",
              overwrite = TRUE
              # height = 6.3,
              # width = 11.3
              )

################################################################################
# Drive-thru
################################################################################

tim_mdt_drivethru <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp == "Drive thru / take-out dining")

trip_times_drivethru_and_mode_mdt <-
  tim_calculator(tim_mdt_drivethru,
                 weights = "wtperfin",
                 criteria = "mode_c",
                 rolling_window = 55)

trips_in_motion_p4 <-
  trip_times_drivethru_and_mode_mdt %>%
  # Collapse school buses into other and sum
  mutate(mode_c = as.character(identifier)) %>%
  mutate(mode_c = case_when(
    identifier == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = recode_factor(factor(mode_c,levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bicycle",
                                "other" = "Other")) %>%  
  group_by(mode_c,time_band) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c),position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#efa7a7")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 6)

finalize_plot(trips_in_motion_p4,
              "Drive-thru / take-out trips in motion in northeastern Illinois by mode on weekdays, 
              2019.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              # height = 6.3,
              # width = 11.3,
              # mode = "png",
              filename = "trips_in_motion_p4",
              overwrite = T)

################################################################################
# Health
################################################################################

tim_mdt_health <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp_c == "health")

trip_times_health_and_mode_mdt <-
  tim_calculator(data = tim_mdt_health,
                 weights = "wtperfin",
                 criteria = "mode_c",
                 rolling_window = 55)

trips_in_motion_p5 <-
  trip_times_health_and_mode_mdt %>%
  # Collapse school buses into other and sum
  mutate(mode_c = as.character(identifier)) %>%
  mutate(mode_c = case_when(
    identifier == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = recode_factor(factor(mode_c,levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bicycle",
                                "other" = "Other")) %>%
  group_by(mode_c,time_band) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c),position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#efa7a7")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 6,
             xlab = "Weekday trips in motion by time of day")

finalize_plot(trips_in_motion_p5,
              "Health care trips also have a morning and afternoon peak, but these are much more concentrated around (but not during) the lunch hour.",
              "Note: Trips in motion are 55-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              # height = 6.3,
              # width = 11.3,
              mode = "png",
              filename = "trips_in_motion_p5",
              overwrite = T)

################################################################################
# Friends/family
################################################################################

tim_mdt_social <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"))

trip_times_social_and_mode_mdt <-
  tim_calculator(data = tim_mdt_social,
                 weights = "wtperfin",
                 criteria = "mode_c",
                 criteria2 = "tpurp",
                 rolling_window = 55)

trips_in_motion_p6 <-
  trip_times_social_and_mode_mdt %>%
  # Collapse school buses into other and sum
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = recode_factor(factor(mode_c,levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bike",
                                "other" = "Other")) %>%
  group_by(mode_c,tpurp,time_band) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c),position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#efa7a7")) +
  
  # Add faceting
  facet_wrap(~tpurp,ncol = 1) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 5)

finalize_plot(trips_in_motion_p6,
              "Socializing trips in motion in northeastern Illinois by mode on weekdays, 
              2019.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              # height = 6.3,
              # width = 11.3,
              # mode = "png",
              filename = "trips_in_motion_p6",
              overwrite = T)


################################################################################
# School
################################################################################

tim_mdt_school <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(tpurp_c == "school")

trip_times_school_and_mode_c_mdt <-
  tim_calculator(tim_mdt_school,
                 weights = "wtperfin",
                 criteria = "mode_c")

trips_in_motion_p7 <-
  trip_times_school_and_mode_c_mdt %>%
  rename(mode_c = identifier) %>%
  # Remove missing modes
  filter(mode_c != "missing") %>%
  # Factor for ordering
  mutate(mode_c = recode_factor(factor(mode_c, levels = mode_c_levels),
                                "driver" = "Driver",
                                "passenger" = "Passenger",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bike",
                                "schoolbus" = "School bus",
                                "other" = "Other")) %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c),position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#a7efe8",
                               "#0084ac","#efa7a7")) +

  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 5)

finalize_plot(trips_in_motion_p7,
              "School trips in motion in northeastern Illinois by mode on weekdays, 
              2019.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              # height = 6.3,
              # width = 11.3,
              # mode = "png",
              filename = "trips_in_motion_p7",
              overwrite = T)

################################################################################
# Bikes (personal)
################################################################################

tim_mdt_bike <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(mode == "personal bike")

trip_times_bike_and_chain_mdt <-
  tim_calculator(data = tim_mdt_bike,
                 weights = "wtperfin",
                 criteria = "chain",
                 rolling_window = 55)

# Graph output of trips in motion by purpose for bike trips (personal bike only)
trips_in_motion_p8 <-
  # Get data
  trip_times_bike_and_chain_mdt %>%
  # Sort chains for ordering
  mutate(chain = factor(identifier, levels = c("Work trip","Return home (work)",
                                          "Shopping trip",
                                          "Return home (shopping)",
                                          "Other trip"))) %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Add colors
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
                               "#e5b172","#3d6600")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",legend.max.columns = 3,
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p8,
              "Bicycling trips in motion in northeastern Illinois by purpose on weekdays, 
              2019.",
              "Note: Trips in motion are 55-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p8",
              mode = "png",
              overwrite = TRUE,
              # height = 6.3,
              # width = 11.3
              )



################################################################################
# Walking
################################################################################

tim_mdt_walking <-
  tim_mdt_wip %>%
  # Filter to just the purpose in question
  filter(mode_c == "walk",
         tpurp_c != "missing")

trip_times_walking_and_tpurp_mdt <-
  tim_calculator(data = tim_mdt_walking,
                 weights = "wtperfin",
                 criteria = "tpurp_c",
                 rolling_window = 55)

# Graph output of trips in motion by purpose for walking trips
trips_in_motion_p9 <-
  # Get data
  trip_times_walking_and_tpurp_mdt %>%
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",legend.max.columns = 3,
             panel.grid.major.x = element_line(color = "light gray")) +
  
  # Add CMAP colors
  cmap_fill_discrete(palette = "mobility")

finalize_plot(trips_in_motion_p9,
              "Walking trips in motion in northeastern Illinois by trip purpose on weekdays, 
              2019.",
              "Note: Trips in motion are 25-minute rolling averages. Trips 
              analyzed include all trips by residents of the region that start 
              and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Trips greater than 
              100 miles or lasting longer than 15 hours are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p9",
              # mode = "png",
              overwrite = TRUE,
              # height = 6.3,
              # width = 11.3
)
#
# # Graph output of trips in motion by purpose for bike trips
# trips_in_motion_p6 <-
#   trip_times_mode_and_purp_c_mdt_55 %>%
#   filter(mode == "bike share") %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = tpurp_c)) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
#   cmap_fill_discrete(palette = "mobility") +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"))
#
# finalize_plot(trips_in_motion_p6,
#               "Bike share trips in motion by travel purpose.",
#               "Note: Trips in motion are 55-minute rolling averages.
#               <br><br>
#               Source: CMAP analysis of My Daily Travel survey.",
#               filename = "trips_in_motion_p6",
#               mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 11.3)
#
# # Split into two groups for comparison
# tim_mdt_bike <-
#   tim_mdt_wip %>%
#   # Filter to just the purpose in question
#   filter(mode %in% c("personal bike")) %>%
#   group_by(sampno,perno,placeno,placeGroup) %>%
#   mutate(n = 1,
#          random = sample(1:2,size = 1))
#
# tim_mdt_bike_set1 <-
#   tim_mdt_bike %>%
#   filter(random == 1)
#
# tim_mdt_bike_set2 <-
#   tim_mdt_bike %>%
#   filter(random == 2)
#
# trip_times_bike_chain_mdt_split <-
#   tim_calculator(possibilities = possible_mode_chain,
#                  base_weights = tim_mdt_bike_set1$wtperfin,
#                  criteria1 = tim_mdt_bike_set1$trip_interval,
#                  criteria2 = tim_mdt_bike_set1$mode_chain,
#                  rolling_window = 55,
#                  crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
#   mutate(set = 1) %>%
#   rbind(tim_calculator(possibilities = possible_mode_chain,
#                        base_weights = tim_mdt_bike_set2$wtperfin,
#                        criteria1 = tim_mdt_bike_set2$trip_interval,
#                        criteria2 = tim_mdt_bike_set2$mode_chain,
#                        rolling_window = 55,
#                        crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
#           mutate(set = 2))
#
#
# trips_in_motion_p5_split <-
#   trip_times_bike_chain_mdt_split %>%
#   mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
#                                           "Shopping trip",
#                                           "Return home (shopping)",
#                                           "Other trip"))) %>%
#   filter(mode == "personal bike") %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
#   facet_wrap(~set) +
#   scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
#                                "#e5b172","#3d6600")) +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"))
#
# finalize_plot(trips_in_motion_p5_split,
#               "Bicycling trips in motion by travel purpose.",
#               "Note: Excludes bike share due to limited sample size. Trips in
#               motion are 55-minute rolling averages.
#               <br><br>
#               Source: CMAP analysis of My Daily Travel survey.",
#               filename = "trips_in_motion_p5_split",
#               mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 11.3)

################################################################################
# Archive - Transit
################################################################################
#
# # Graph output of trips in motion by purpose for transit trips
# trips_in_motion_p7 <-
#   trip_times_mode_and_purp_c_mdt_25 %>%
#   mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels)) %>%
#   filter(mode %in% c("rail and bus", "bus", "train",
#                      "local transit", "transit"),
#          tpurp_c != "missing") %>%
#   group_by(time_band,tpurp_c) %>%
#   summarize(rolling_count = sum(rolling_count)) %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = tpurp_c),position = position_stack(reverse = T)) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
#   scale_fill_discrete(type = c("#00becc","#cc5f00","#3f0e00","#cca600",
#                                "#003f8c","#67ac00","#006b8c","#efa7a7",
#                                "#8c4100","#00303f","#a7efe8")) +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"))
#
# finalize_plot(trips_in_motion_p7,
#               "Transit trips in motion by travel purpose.",
#               "Note: Trips in motion are 25-minute rolling averages.
#               <br><br>
#               Source: CMAP analysis of My Daily Travel survey.",
#               filename = "trips_in_motion_p7",
#               # mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 11.3)

################################################################################
# ARCHIVE - TNC/Taxi
################################################################################

# This analysis is not robust to weighting changes
#
# # Graph output of trips in motion by purpose for taxi vs. ride share trips
# trips_in_motion_p8 <-
#   trip_times_mode_and_purp_c_mdt_55 %>%
#   filter(mode %in% c("rideshare","shared rideshare","taxi"),
#          tpurp_c != "missing") %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = tpurp_c), position = position_stack(reverse = T)) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
#   facet_wrap(~mode, nrow = 3) +
#   scale_fill_discrete(type = c("#00becc","#cc5f00","#3f0e00","#cca600",
#                                "#003f8c","#67ac00","#006b8c","#efa7a7",
#                                "#8c4100","#00303f","#a7efe8")) +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"))
#
# finalize_plot(trips_in_motion_p8,
#               "Ride-share and shared ride-share trips in motion by travel
#               purpose.",
#               "Note: Trips in motion are 55-minute rolling averages.
#               <br><br>
#               Source: CMAP analysis of My Daily Travel survey.",
#               filename = "trips_in_motion_p8",
#               mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 11.3)
#
#
# ##### Ride share and taxis, by chain
#
# # Graph trips in motion by mode category for work chains
# trips_in_motion_p9 <-
#   trip_times_mode_and_chain_mdt_55 %>%
#   mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
#                                           "Shopping trip",
#                                           "Return home (shopping)",
#                                           "Other trip"))) %>%
#   filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
#   facet_wrap(~mode, ncol = 1) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
#   scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
#                                "#e5b172","#3d6600")) +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"),
#              legend.max.columns = 4)
#
# finalize_plot(trips_in_motion_p9,
#               "Trips in motion in TNCs and taxis in the CMAP region by trip
#               chain type on weekdays.",
#               "Note: Trips in motion are 55-minute rolling averages.
#               <br><br>
#               Source: CMAP analysis of My Daily Travel survey.",
#               filename = "trips_in_motion_p9",
#               mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 11.3)
#
# # Unweighted
# tim_mdt_tnc_taxi <-
#   tim_mdt_wip %>%
#   # Filter to just the purpose in question
#   filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
#   group_by(sampno,perno,placeno,placeGroup) %>%
#   mutate(n = 1,
#          random = sample(1:2,size = 1))
#
# tim_mdt_tnc_taxi_set1 <-
#   tim_mdt_tnc_taxi %>%
#   filter(random == 1)
#
# tim_mdt_tnc_taxi_set2 <-
#   tim_mdt_tnc_taxi %>%
#   filter(random == 2)
#
# trip_times_tnc_taxi_and_chain_mdt_unweighted <-
#   tim_calculator(possibilities = possible_mode_chain,
#                  base_weights = tim_mdt_tnc_taxi_set1$wtperfin,
#                  criteria1 = tim_mdt_tnc_taxi_set1$trip_interval,
#                  criteria2 = tim_mdt_tnc_taxi_set1$mode_chain,
#                  rolling_window = 55,
#                  crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
#   mutate(set = 1) %>%
#   rbind(tim_calculator(possibilities = possible_mode_chain,
#                        base_weights = tim_mdt_tnc_taxi_set2$wtperfin,
#                        criteria1 = tim_mdt_tnc_taxi_set2$trip_interval,
#                        criteria2 = tim_mdt_tnc_taxi_set2$mode_chain,
#                        rolling_window = 55,
#                        crosstab = T,crosstab1 = "mode",crosstab2 = "chain") %>%
#           mutate(set = 2))
#
# # Graph trips in motion by mode category for work chains
# trips_in_motion_p9_unweighted <-
#   trip_times_tnc_taxi_and_chain_mdt_unweighted %>%
#   mutate(chain = factor(chain, levels = c("Work trip","Return home (work)",
#                                           "Shopping trip",
#                                           "Return home (shopping)",
#                                           "Other trip"))) %>%
#   filter(mode %in% c("taxi","rideshare","shared rideshare")) %>%
#   ggplot(aes(x = time_band,y = rolling_count)) +
#   geom_area(aes(fill = chain), position = position_stack(reverse = T)) +
#   facet_wrap(set~mode) +
#   scale_x_datetime(labels = scales::date_format("%H:%M",
#                                                 tz = "America/Chicago"),
#                    breaks = breaks[c(2,4,6,8,10)]) +
#   scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
#   scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
#                                "#e5b172","#3d6600")) +
#   theme_cmap(gridlines = "hv",
#              panel.grid.major.x = element_line(color = "light gray"),
#              legend.max.columns = 4)
#
# finalize_plot(trips_in_motion_p9_unweighted,
#               title_width = 0,
#               filename = "trips_in_motion_p9_unweighted",
#               mode = "png",
#               overwrite = TRUE,
#               height = 6.3,
#               width = 8.3)


