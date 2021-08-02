# This script allows for the creation of "trips in motion" analyses of the MDT
# trip diary survey data.

#################################################
#                                               #
#                 Library loading               #
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
source("R/data_cleaning.R")
source("R/helper_fns.R")

# Create helper values for date identification and modification
threshold_tim <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24

# Filter and process data into working file for calculations
tim_wip_mdt <-
  mdt %>%        # 125463 records
  # Keep only travelers >= 5 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 5 |                                  
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8))) %>% # 125447 records
  # Remove beginning trips
  filter(mode != "beginning") %>% # 97365 records
  # Remove all trips where the arrival time comes before the start time (there
  # are 10 records where this is a problem)
  mutate(ends_before_begins = case_when(
    is.na(start_times_pg) ~ 0,
    start_times_pg <= arrtime_pg ~ 0,
    TRUE ~ 1)) %>%
  filter(ends_before_begins == 0) %>% # 97355 records
  select(-ends_before_begins) %>%  
  # Filter out unwanted trips
  filter(
    # Remove trips > 15 hours
    travtime_pg_calc < 15 * 60,     # 97270 records
    # Remove trips with zero travel time
    travtime_pg_calc > 0,           # 97322 records
    # Remove trips with 0 place group distance
    distance_pg > 0                 # 97264 records
  ) %>%
  # Exclude improbable walk trips
  filter(improbable_walk == 0) %>%  # 97224 records
  
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
      trip_end < threshold_tim ~ trip_end + day_value,
      TRUE ~ trip_end
    ),
    trip_start = case_when(
      trip_start < threshold_tim ~ trip_start + day_value,
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
  tim_calculator(data = tim_wip_mdt,
                 criteria = "mode_c",
                 weights = "weight")

# Trips in motion by trip chain and mode category (25 minute rolling average)
trip_times_chain_c_mdt <-
  tim_calculator(data = tim_wip_mdt,
                 criteria = "chain_c",
                 weights = "weight")

# Trips in motion by trip chain and mode category (25 minute rolling average)
trip_times_mode_c_and_chain_c_mdt <-
  tim_calculator(data = tim_wip_mdt,
                 criteria = c("mode_c","chain_c"),
                 weights = "weight")

# Trips in motion by trip chain and purpose (25 minute rolling average)
trip_times_tpurp_c_chain_c_mdt <-
  tim_calculator(data = tim_wip_mdt %>%
                   filter(tpurp_c != "missing"),
                 criteria = c("tpurp_c","chain_c"),
                 weights = "weight")

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
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
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
             xlab = "Time of day",
             ylab = "Trips in motion on an average weekday by mode")

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
                     format(nrow(tim_wip_mdt),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries."),
              filename = "trips_in_motion_p1",
              # mode = c("png","pdf"),
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
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 10) +
  
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
  trip_times_chain_c_mdt %>%
  # Capitalize for presentation
  mutate(chain_c = recode_factor(chain_c,
                                 "work" = "Work",
                                 "shop" = "Shopping",
                                 "other" = "Other")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_line(aes(color = chain_c)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray",),
             legend.max.columns = 7,
             xlab = "Time of day",
             ylab = "Trips in motion on an average weekday by trip chain type") +
  
  cmap_color_discrete(palette = "friday")

trips_in_motion_p2_samplesize <-
  tim_wip_mdt %>% 
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
              height = 5,
              mode = c("png","pdf"),
              overwrite = TRUE)

# Understand how mode share varies within these trip chain peaks

am_peak <- interval(start = as.POSIXct("2020-01-01 06:00", tz = "America/Chicago"),
                    end = as.POSIXct("2020-01-01 09:00", tz = "America/Chicago"))

pm_peak <- interval(start = as.POSIXct("2020-01-01 15:00", tz = "America/Chicago"),
                    end = as.POSIXct("2020-01-01 19:00", tz = "America/Chicago"))

chain_peak_breakdown <-
  tim_wip_mdt %>% 
  mutate(peak = case_when(
    int_overlaps(trip_interval,am_peak) ~ "Peak",
    int_overlaps(trip_interval,pm_peak) ~ "Peak",
    TRUE ~ "Off-peak"
  )) %>% 
  filter(mode_c != "missing")
  
  
chain_peak_mode_share <-
  pct_calculator(
    chain_peak_breakdown,
    breakdown_by = "mode_c",
    second_breakdown = "chain_c",
    third_breakdown = "peak",
    weight = "weight") %>% 
  # Recode chain
  mutate(chain_c = recode(chain_c,
                          "work" = "Work",
                          "shop" = "Shopping",
                          "other" = "Other"),
         pct = round(pct, 4)) %>% 
  ungroup()

trips_in_motion_p2a <-
  chain_peak_mode_share %>% 
  mutate(mode_c = recode_factor(factor(mode_c,levels = mode_c_levels),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "transit" = "Transit",
                           "walk" = "Walk",
                           "schoolbus" = "School bus",
                           "bike" = "Bicycle",
                           "other" = "Other")) %>% 
  # Create ggplot object
  ggplot(aes(x = pct, y = peak,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             legend.max.columns = 7,
             vline = 0,
             xlab = "Mode share by trip chain type",
             axis.title.x = element_text(hjust = 0.5),
             strip.text = element_text(hjust = 0.5,vjust = 1,
                                       family = "Whitney Semibold")) +
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#36d8ca","#6d8692",
                               "#efa7a7","#3d6600","#0084ac")) +
  
  
  # Add faceting
  facet_wrap(~chain_c,ncol = 1)

trips_in_motion_p2a_samplesize <-
  chain_peak_mode_share %>% 
  group_by(chain_c,peak) %>% 
  summarize(n = sum(breakdown_n)) %>% 
  ungroup()

# Export finalized graphic
finalize_plot(trips_in_motion_p2a,
              "Travel choices differ significantly between peak and off-peak trips.",
              paste0("Note: Trips 
              analyzed include weekday trips by residents age 5 and older 
              of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              Excludes trips longer than 100 miles or greater than 15 hours long.
              Peak trips include all trips that were in motion between 6:00am 
              and 9:00am or between 3:00pm and 7:00pm.
              <br><br>
              Sample size (Work/Shopping/ Other):
              <br>- Peak (",
                     paste0(trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Work", 
                                    peak == "Peak") %>% 
                             select(n) %>% 
                             as.numeric(),
                            "/",
                           trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Shopping", 
                                    peak == "Peak") %>% 
                             select(n) %>% 
                             as.numeric(),
                           "/ ",
                           trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Other", 
                                    peak == "Peak") %>% 
                             select(n) %>% 
                             as.numeric()),
                     ");
              <br>- Off-peak (",
                     paste0(trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Work", 
                                    peak == "Off-peak") %>% 
                             select(n) %>% 
                             as.numeric(),
                            "/ ",
                           trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Shopping", 
                                    peak == "Off-peak") %>% 
                             select(n) %>% 
                             as.numeric(),
                           "/",
                           trips_in_motion_p2a_samplesize %>% 
                             filter(chain_c == "Other", 
                                    peak == "Off-peak") %>% 
                             select(n) %>% 
                             as.numeric()),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries."),
              filename = "trips_in_motion_p2a",
              # sidebar_width = 2.7,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Health
################################################################################

tim_health_mdt <-
  tim_wip_mdt %>% # 97224 records
  # Filter to just the purpose in question
  filter(tpurp == "Health care visit for self") # 1440 records

trip_times_health_and_mode_mdt <-
  tim_calculator(data = tim_health_mdt,
                 weights = "weight",
                 criteria = "mode_c",
                 rolling_window = 55)

trips_in_motion_p3 <-
  trip_times_health_and_mode_mdt %>%
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
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  # Manually add colors
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#6d8692","#36d8ca",
                               "#efa7a7","#0084ac")) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 6,
             xlab = "Time of day",
             ylab = "Trips in motion for personal health care on an average weekday")

finalize_plot(trips_in_motion_p3,
              "Personal health care trips also have a morning and afternoon peak, but
              these are much more concentrated around, but not during, the
              lunch hour.",
              paste0("Note: Trips in motion are 55-minute rolling averages. Trips 
              analyzed include weekday personal health care trips by residents age 5 and older 
              of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              Excludes trips longer than 100 miles or greater than 15 hours long.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(tim_health_mdt),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries."),
              # height = 6.3,
              # width = 11.3,
              mode = c("pdf","png"),
              filename = "trips_in_motion_p3",
              overwrite = T)

################################################################################
# Bikes (personal)
################################################################################

tim_bike_mdt <-
  tim_wip_mdt %>% # 97224 records
  # Filter to just the purpose in question
  filter(mode == "personal bike") %>% # 1513 records
  # Recode chains to just be work and other
  mutate(chain = recode_factor(chain,
                               "Work trip" = "Work trip",
                               "Return home (work)" = "Work trip",
                               "Shopping trip" = "Other trip",
                               "Return home (shopping)" = "Other trip",
                               "Other trip" = "Other trip")) 

trip_times_bike_and_chain_mdt <-
  tim_calculator(data = tim_bike_mdt,
                 weights = "weight",
                 criteria = "chain",
                 rolling_window = 85)

# Graph output of trips in motion by purpose for bike trips (personal bike only)
trips_in_motion_p4 <-
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
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = c(0,3000,6000,9000,12000,15000),
                     limits = c(0,15000),expand = expansion(mult = c(.05,.01))) +
  
  # Add colors
  scale_fill_discrete(type = c("#72cae5","#3d6600")) +
  
  # # # Archived - facet by chain
  # facet_wrap(~chain,ncol = 1) +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",legend.max.columns = 3,
             panel.grid.major.x = element_line(color = "light gray"),
             xlab = "Time of day",
             ylab = "Personal bike trips in motion on an average weekday")

finalize_plot(trips_in_motion_p4,
              "Overall personal bike usage was highest during the AM peak, although work 
              trips have similar AM and PM peak profiles.",
              # "Note: Trips in motion are 85-minute rolling averages. Trips 
              # analyzed include weekday trips by residents age 5 and older 
              # of the CMAP seven county region (Cook, DuPage, Kane, Kendall, 
              # Lake, McHenry, and Will), as well as Grundy and DeKalb. Includes
              # only trips that were within, to, and/or from one of those counties.
              # Excludes trips longer than 100 miles or greater than 15 hours long.
              "Note: Trips in motion are 85-minute rolling averages. Trips 
              analyzed include weekday trips by residents age 5 and older 
              of the CMAP seven county region, Grundy, and DeKalb. Includes
              only trips that were within, to, and/or from one of those counties.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel trip diaries.",
              filename = "trips_in_motion_p4",
              height = 3.5,
              mode = c("png","pdf"),
              overwrite = TRUE,
              # height = 2.25,
              # width = 8
              )


################################################################################
# Backup for prose - Other chains by trip purpose
################################################################################

trips_in_motion_p5 <-
  # Get data
  trip_times_tpurp_c_chain_c_mdt %>%
  # Capitalize for presentation
  mutate(chain_c = recode_factor(chain_c,
                                 "work" = "Work",
                                 "shop" = "Shopping",
                                 "other" = "Other")) %>% 
  # Capitalize and group low-number trips into "Other" bucket
  mutate(tpurp_c = recode_factor(tpurp_c,
                                 "school" = "School",
                                 "transport" = "Transporting others",
                                 "dining" = "Dining",
                                 "health" = "Health care",
                                 "community" = "Community and socializing",
                                 "recreation/fitness" = "Recreation and fitness",
                                 "shopping/errands" = "Other purpose",
                                 "transfer" = "Other purpose",
                                 "other" = "Other purpose",
                                 "home" = "Going home")) %>% 
  group_by(tpurp_c,chain_c,time_band) %>% 
  summarize(rolling_count = sum(rolling_count)) %>% 
  
  # Keep only other trips
  filter(chain_c == "Other") %>%
  # Exclude very small number of work trips that somehow are excluded from work chains
  filter(tpurp_c != "work") %>% 
  
  # Create ggplot object
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp_c), position = position_stack(reverse = T)) +
  
  # Adjust axes
  scale_x_datetime(labels = scales::date_format("%l:%M%p", # Time without leading zero
                                                tz = "America/Chicago"),
                   breaks = tim_breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  
  scale_fill_brewer(type = "qual") +
  
  # Add CMAP style
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3,
             xlab = "Trip purposes for 'Other' trip chains by time of day") #+
# facet_wrap(~chain_c, ncol = 1)


finalize_plot(trips_in_motion_p5,
              sidebar_width = 0,
              overwrite = TRUE
)

# Calculate the share of the morning peak taken up by School and Transporting others
trip_times_tpurp_c_chain_c_mdt %>%
  # Remove interval
  select(-time_band_interval) %>% 
  # Keep only other trips in the morning
  filter(chain_c == "other",
         time_band < "2020-01-01 12:00:00 CST") %>% 
  # Group by time band and create totals + percents of totals by purpose
  group_by(time_band) %>% 
  mutate(total = sum(rolling_count),
         pct = rolling_count / total) %>% 
  # Keep two purposes of interest
  filter(tpurp_c %in% c("school","transport")) %>% 
  # Remove unneeded variables
  select(-count,-rolling_count,-chain_c) %>% 
  # Pivot
  pivot_wider(names_from = tpurp_c,values_from = c(pct)) %>% 
  mutate(transport_school_pct = transport+school) %>% 
  # Show the percent at the peak
  arrange(-total) %>% 
  View()

