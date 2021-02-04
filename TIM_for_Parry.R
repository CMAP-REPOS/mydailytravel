#################################################
#                                               #
#                 Library loading               #
#                                               #
#################################################

library(tidyverse) # A package that enables new data manipulation functionality (and a broader "grammar" for R)
library(ggplot2) # A graphing package
library(lubridate) # A date handling package
library(slider) # A package that allows rolling average calculation
library(cmapplot) # CMAP's custom style package (an extension of ggplot)
# cmapplot can be installed by running the following code (once per machine)
# devtools::install_github("CMAP-REPOS/cmapplot")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Load recoding information
setwd("~/GitHub/mydailytravel")
source("recoding.R")

# Load My Daily Travel
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, arrtime, deptime, travtime, tpurp, distance, hdist)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, aage, schol, hisp, race, pertrips, wtperfin, tcoff, tcdays, emply_ask, jobs, wplace)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region, county_fips, home)

# Trip chains
chains <- read_csv("chains.csv")

# home location flag
home_wip <- region %>%
  filter(home == 1) %>% # identify home locations
  select(sampno,
         home_county = county_fips) %>%
  distinct() # keep distinct home locations based on sample

# There are 68 households coded as having home locations in multiple counties. Identify them.
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace multi-county records with a county_fips of 999
home <- home_wip %>%
  mutate(home_county = case_when(
    sampno %in% two_homes ~ 999,
    TRUE ~ home_county)) %>%
  distinct()

# Remove WIP tables
rm(home_wip, two_homes)

# add combined duration and distance for placeGroup trips
placeGroupStats <- trips %>%
  mutate(hdist = ifelse(hdist >= 0,hdist,0),
         distance = ifelse(distance >= 0,distance,0),
         travtime = ifelse(travtime >= 0,travtime,0)) %>%
  group_by(sampno,perno,placeGroup) %>%
  summarize(hdist_pg = sum(hdist, na.rm = TRUE),
            distance_pg = sum(distance, na.rm = TRUE),
            travtime_pg = sum(travtime, na.rm = TRUE),
            arrtime_pg = max(arrtime),
            deptime_pg = min(deptime))

# merge datasets and filter for destination and distance
mdt <- trips %>% # 128,229 records
  inner_join(ppl, by = c("sampno", "perno")) %>% # 128,229 records
  inner_join(hh, by = "sampno") %>% # 128,229 records
  inner_join(region, by = c("sampno", "locno")) %>% # 128,229 records
  inner_join(home, by = c("sampno")) %>% # 128,229 records
  inner_join(chains, by = c("sampno", "perno", "placeno")) %>% # 128,229 records
  # Remove trips that leave the CMAP region
  filter(out_region==0) %>% # 126,075 records
  # Remove trips >= 100 miles
  filter(distance<100) # 125,955 records

# take care of collapsed trips with placeGroup
mdt <- mdt %>%
  # distinct takes the first row for duplicates, so order by distance to get right mode
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>% # 125,103 records
  # add combined distance and time values calculated above (note some trips are missing one or more records)
  left_join(.,placeGroupStats, by = c("sampno","perno","placeGroup")) # 125,103 records

# Remove placegroup stats
rm(placeGroupStats)


# recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode = factor(mode),
         mode = recode_factor(mode,
                              !!!recode_mode_detailed_mdt))%>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

# Recode trip purposes and group into buckets for comparison
mdt <- mdt %>%
  mutate(tpurp = factor(tpurp)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_mdt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_mdt)) %>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels))

# Recode trip chains in MDT data
mdt <- mdt %>%
  mutate(chain = case_when(
    home_to_work == 1 ~ "Work trip",
    work_to_work == 1 ~ "Work trip",
    work_to_home == 1 ~ "Return home (work)",
    home_to_shop == 1 ~ "Shopping trip",
    shop_to_shop == 1 ~ "Shopping trip",
    shop_to_home == 1 ~ "Return home (shopping)",
    TRUE ~ "Other trip"
  )) %>%
  mutate(chain = factor(chain,
                        levels = c("Work trip","Return home (work)",
                                   "Shopping trip","Return home (shopping)",
                                   "Other trip")))
# remove recoding helpers
rm(recode_income_buckets_mdt,recode_income_buckets_tt,
   recode_mode_buckets_mdt,recode_mode_buckets_tt,
   recode_tpurp_buckets_mdt,recode_tpurp_buckets_tt,
   recode_income_detailed_mdt,recode_income_detailed_tt,
   recode_mode_detailed_mdt,recode_mode_detailed_tt,
   recode_tpurp_detailed_mdt,recode_tpurp_detailed_tt)

setwd("~/GitHub/mydailytravel")

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

# Create helper values for date identification and modification
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24


# Convert arrival and departure times to date-time objects
tim_mdt_datetimes <-
  mdt %>%
  # Convert arrival time to datetime object
  mutate(arrtime_pg = ymd_hms(arrtime_pg),
         deptime_pg = ymd_hms(deptime_pg)) %>%
  # Arrange by sampno, perno, and placegroup to enable extraction of departure
  # time
  arrange(desc(sampno),perno,placeGroup)

# Extract start times and shift them down by one (since a trip begins at the
# departure time of the previous record and ends at the arrival time of the
# current record)
start_times_pg <-
  tibble(start_times_pg = c(ymd_hms(NA))) %>%
  rbind(tim_mdt_datetimes %>%
          select(start_times_pg = deptime_pg) %>%
          head(n = nrow(tim_mdt_datetimes) - 1))

# Filter and process data into working file for calculations
tim_mdt_wip <-
  # Start with the date-time version of the overall mdt data table
  tim_mdt_datetimes %>%        # 125,103 records
  # Add the start times for each trip, identified above
  cbind(start_times_pg) %>%

  # Filter out unwanted trips (>= 15 hours, "beginning" mode, and 0 distance)
  filter(
    # Remove trips > 15 hours
    travtime_pg < 15 * 60,     # 125,096 records
    # Remove "beginning" trips
    mode != "beginning",       # 97,007 records
    # Remove trips with 0 place group distance
    distance_pg > 0            # 96,949 records
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
      trip_end <= threshold ~ trip_end + day_value,
      TRUE ~ trip_end
    ),
    trip_start = case_when(
      trip_start <= threshold ~ trip_start + day_value,
      TRUE ~ trip_start
    )) %>%
  # Create trip interval - this can be used to identify whether a trip is within
  # a given time interval
  mutate(trip_interval = lubridate::interval(trip_start,trip_end,
                                             tz = "America/Chicago")) %>%

  # Add helper columns for crosstab analyses (e.g., both mode and chain) - these
  # will be used to minimize processing time. We use a '-' to separate each. If
  # others are added, they should follow the same convention.
  #
  # Create combined mode and purpose
  mutate(mode_tpurp = paste(mode,tpurp,sep = "-")) %>%
  # Create combined mode and purpose category
  mutate(mode_tpurp_c = paste(mode,tpurp_c,sep = "-")) %>%
  # Create combined mode and chain category
  mutate(mode_chain = paste(mode,chain,sep = "-")) %>%
  # Create combined mode category and chain category
  mutate(mode_c_chain = paste(mode_c,chain,sep = "-"))


################################################################################
# Purpose: Calculate trip times within the intervals that meet given criteria.
#
################################################################################
# Inputs: * base_weights, a numeric vector. This defaults to the full list of
#           weights in the tim_mdt_wip table, but can be modified to use a
#           shorter list (for subset analyses) or a different column (e.g., for
#           unweighted analyses)
#         * trip_interval, a vector of date-time objects. This defaults to the
#           full list of trip intervals in the tim_mdt_wip table, but can be
#           modified to use a shorter list for subset analyses.
#         * criteria, a vector of strings. This will be the categorization into
#           which trip counts are distributed (e.g., mode categories or trip
#           chains by mode).
#         * rolling_interval, numeric. This is the width of the time interval in
#           minutes in which trips will be counted. Defaults to 5.
#         * rolling_n, numeric. This is the length of the rolling average
#           window, in minutes. Defaults to 25 minutes. The rolling average is
#           calculated as a straddling rolling average, with the interval plus a
#           symmetric window on either side. rolling_n should be an odd multiple
#           of rolling_interval.
#         * crosstab, bool. Defaults to FALSE. this should be set to TRUE if the
#           criteria includes two buckets (e.g., chain and mode category).
#         * crosstab1, crosstab2, both strings. This should be supplied when
#           crosstab = T. Each string will be outputted as the variable name for
#           the final trips in motion calculation. For example, a crosstab
#           analysis of tim_wip_mdt$mode_tpurp_c should have a value of
#           crosstab1 = "mode" and crosstab2 = "tpurp_c"
################################################################################
# Outputs: This function calculates the rolling average of trips in motion
#          within a specified set of categories (e.g., by mode) over the course
#          of a single day, running from 3am to 3am.
################################################################################
tim_calculator <- function(base_weights = tim_mdt_wip$wtperfin,
                           trip_interval = tim_mdt_wip$trip_interval,
                           criteria,
                           rolling_interval = 5,
                           rolling_n = 25,
                           crosstab = F,
                           crosstab1 = NULL,
                           crosstab2 = NULL) {

  # Create a helper tibble of possible criteria
  possibilities <- tibble(identifier = unique(criteria))

  # Create tibble of all possible trip intervals
  trip_times <-
    # Establish sequence of times over the day (in one minute increments)
    tibble(time_band = seq.POSIXt(
      from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
      to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
      # Interval using input above, defaults to 5
      by = paste0(rolling_interval," min")))

  # Calculate number of intervals on either side of the interval needed for
  # rolling average
  rolling_n_calc <- ((rolling_n / rolling_interval) - 1) / 2

  # Calculate trips in motion within the intervals meeting criteria
  raw_count <-
    # Start with all trip intervals
    trip_times %>%
    # Add all possible modes and/or purposes and/or chains
    full_join(possibilities, by = character()) %>%
    # Sum the weights that meet both criteria
    mutate(count = mapply(function(x,y) sum(base_weights[which(
      x %within% trip_interval &
        y == criteria)]),
      time_band,
      identifier
    ))

  # If crosstab, need to split identifier and group by both columns. Otherwise,
  # group by identifier.
  if (!crosstab) {
    grouped_count <-
      raw_count %>%
      group_by(identifier)
  } else {
    grouped_count <-
      raw_count %>%
      # Split the data using the '-' as a separator
      separate(identifier,into = c(crosstab1,crosstab2) , sep = "-") %>%
      # using the .data[[crosstabx]] format allows us to use the string as a
      # variable name
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


################################################################################
# Use the function we just developed to calculate the rolling averages as needed

# Trips in motion by mode category (25 minute rolling average)
trip_times_mode_c_mdt <-
  tim_calculator(criteria = tim_mdt_wip$mode_c)


# Trips in motion by trip chain (25 minute rolling average)
trip_times_chains_mdt <-
  tim_calculator(criteria = tim_mdt_wip$chain)

# Trips in motion by trip chain and mode category (25 minute rolling average)
trip_times_mode_c_and_chain_mdt_25 <-
  tim_calculator(criteria = tim_mdt_wip$mode_c_chain,
                 crosstab = T,crosstab1 = "mode_c",crosstab2 = "chain")

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
  trip_times_mode_c_mdt %>%
  filter(identifier != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = TRUE)) +
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#a7efe8","#6d8692",
                               "#0084ac","#efa7a7","#67ac00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(trips_in_motion_p1,
              "Trips in motion in the CMAP region by mode on weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p1",
              # mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)

# Graph trips in motion by trip chains
trips_in_motion_p6 <-
  trip_times_chains_mdt %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = identifier), position = position_stack(reverse = T)) +
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  scale_fill_discrete(type = c("#009ccc","#72cae5","#cc8200",
                               "#e5b172","#3d6600")) +
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

# Graph trips in motion by mode category, faceting for different chain types
trips_in_motion_p7 <-
  trip_times_mode_c_and_chain_mdt_25 %>%
  mutate(mode_c = factor(mode_c, levels = mode_c_levels)) %>%
  filter(mode_c != "missing") %>%
  mutate(category = fct_collapse(chain,
                                 Work = c("Work trip","Return home (work)"),
                                 Other = "Other trip",
                                 Shopping = c("Shopping trip",
                                              "Return home (shopping)"))) %>%
  mutate(category = factor(category, levels = c("Work","Shopping","Other"))) %>%
  group_by(time_band,mode_c,category) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = mode_c), position = position_stack(reverse = TRUE)) +
  facet_wrap(~category, ncol = 1) +
  scale_x_datetime(labels = scales::date_format("%H:%M",
                                                tz = "America/Chicago"),
                   breaks = breaks) +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  scale_fill_discrete(type = c("#8c0000","#e5bd72","#a7efe8","#6d8692",
                               "#0084ac","#efa7a7","#67ac00")) +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 10)

finalize_plot(trips_in_motion_p7,
              "Trips in motion in the CMAP region by trip chain type on
              weekdays.",
              "Note: Trips in motion are 25-minute rolling averages.
              <br><br>
              Source: CMAP analysis of My Daily Travel survey.",
              filename = "trips_in_motion_p7",
              # mode = "png",
              overwrite = TRUE,
              height = 6.3,
              width = 11.3)
