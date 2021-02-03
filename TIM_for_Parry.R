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

# add combined duration and distance for placeGroup trips
placeGroupStats <- mdt %>%
  filter(hdist >= 0,
         distance >= 0,
         travtime >= 0) %>%
  group_by(sampno,perno,placeGroup) %>%
  summarize(hdist_pg = sum(hdist, na.rm = TRUE),
            distance_pg = sum(distance, na.rm = TRUE),
            travtime_pg = sum(travtime, na.rm = TRUE),
            arrtime_pg = max(arrtime))

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
                               levels = c("Work trip",
                                          "Return home (work)",
                                          "Shopping trip",
                                          "Return home (shopping)",
                                          "Other trip")))

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
  mutate(mode_chain = paste(mode,chain,sep = "-")) %>%
  # Create combined mode category and chain category
  mutate(mode_c_chain = paste(mode_c,chain,sep = "-"))


# Extract possible modes and/or purposes
possible_mode_c <- tibble(identifier = unique(tim_mdt_wip$mode_c))
possible_chain <- tibble(identifier = unique(tim_mdt_wip$chain))


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

#################################################################
#                                                               #
#    Scripts to calculate trips in motion by time interval      #                          #
#                                                               #
#################################################################

trip_times_mode_c_mdt <-
  tim_calculator(possibilities = possible_mode_c,
                 criteria2 = tim_mdt_wip$mode_c)


trip_times_chains_mdt <-
  tim_calculator(possibilities = possible_chain,
                 criteria2 = tim_mdt_wip$chain)


#################################################################
#                                                               #
#                        Charts                                 #
#                                                               #
#################################################################

breaks <- seq.POSIXt(from = as.POSIXct("2020-01-01 03:00:00"),
                     to = as.POSIXct("2020-01-02 03:00:00"),
                     by = "3 hours")

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
              # mode = "png",
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
