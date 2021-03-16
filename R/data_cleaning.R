# This file loads and cleans the data used in analyses of My Daily Travel and
# Travel Tracker. The file outputs a number of tables; the most important are:
#
# - mdt: this table represents the trips taken in the MDT survey, with added
# characteristics about the person, household, and location of the trip. Trips
# that are within the same "place group" are collapsed into one trip with the
# cumulative travel time, distance, and start/end times. Trips that neither
# start nor end in Cook, Kane, Kendall, McHenry, Lake, DuPage, Will, DeKalb, or
# Grundy County are excluded. Trips greater than 100 miles are also excluded.
# Mode, purpose, income, and race and ethnicity are added and recoded, using
# lists and vectors from recoding.R where relevant.
#
# - tt: this table represents the trips taken in the TT survey, with added
# characteristics about the person, household, and location of the trip. Trips
# on the weekend are excluded and weights are adjusted to account for surveys
# that had two weekday trip diaries. Trips that neither start nor end in Cook,
# Kane, Kendall, McHenry, Lake, DuPage, Will, DeKalb, or Grundy County are
# excluded. Trips greater than 100 miles are also excluded. Mode, purpose, and
# income are added and recoded, using lists and vectors from recoding.R where
# relevant.
#
# - mdt_all_respondents and tt_all_respondents: these tables includes person and
# household information for all respondents in the survey, whether or not they
# traveled, with income recoded using the definitions from recoding.R.



#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(lubridate)

#################################################
#                                               #
#                 Load and clean MDT            #
#                                               #
#################################################

# Load recoding information
setwd("~/GitHub/mydailytravel")
source("R/recoding.R")

# Load My Daily Travel
setwd("C:/Users/dcomeaux/Chicago Metropolitan Agency for Planning/Transportation Focus Area - Documents/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, perno, locno, placeno, # Identifiers for the household (sampno),
                     # the person (perno), the destination of the trip (locno),
                     # and the order in which that place was visited in the
                     # context of the entire day (placeno).

         placeGroup, # A supplement for placeno that combines continuous trips
                     # that were artificially broken apart into multiple
                     # segments.

         tpurp,      # The purpose of the trip.
         mode,       # The mode of the trip.

         arrtime,    # The arrival time to the place identified above. This
                     # represents the end time of the trip to the identified
                     # place.
         deptime,    # The departure time from the place identified above. This
                     # represents the start time of the next trip (if
                     # applicable).

         travtime,   # The travel time of the trip.

         distance,   # The network distance of the trip.
         hdist       # The haversine distance of the trip (used for comparisons
                     # with Travel Tracker)
         )

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, # Identifiers for household (sampno) and person (perno)

         age,        # The age (in years) of the respondent.
         aage,       # Range of ages (only for those without an answer to age).
         age18,      # Either younger than 18 (2) or 18 or older (2)

         schol,      # School enrollment status.
         smode,      # Regular mode used to get to school.

         hisp,       # The individual's status as Hispanic/non-Hispanic
         race,       # The individual's race (self-identified).

         pertrips,   # The number of trips taken by the traveler.

         tcoff,      # Whether or not telecommuting is offered by the
                     # respondent's workplace.
         tcdays,     # How often the respondent telecommutes.

         emply_ask,  # Is the respondent employed?
         jobs,       # How many jobs does the respondent report having?

         wplace,     # What kind of workplace does the respondent have (i.e., a
                     # fixed workplace, work at home, etc.)?
         wmode,      # How does the respondent usually get to work?
         wtrav,      # How many times a week does the respondent travel to work?

         smrtphn,    # Do they own a smartphone?
         tnc_use,    # How often did they use Uber, Lyft, or Via in the last week?
         tnc_cost,   # How much do those trips usually cost?
         
         
         wtperfin    # The weight for the respondent.
         )

# household info
hh <- read_csv("household.csv") %>%
  select(sampno,     # Identifier for the household.
         hhinc,      # Household income (in dollars).
         hhveh,      # Number of vehicles in the household.
         travday     # The day of the week for the household's travel diary.
  )

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno,locno, # Identifier for the household (sampno) and location
                     # (locno)

         loctype,    # The type of location (e.g., home, work, school).

         out_region, # Whether the location is outside the CMAP modeled region
                     # (Cook, Kane, Kendall, DuPage, McHenry, Will, Lake, Grundy,
                     # and DeKalb Counties, Illinois).

         state_fips,county_fips,tract_fips, # The state, tract, and county FIPS
                     # codes for the location

         home,       # Whether the location is a designated home location.

         latitude, longitude # The lat/long coordinates of the place.
  )

# Trip chains (provided by CMAP R&A staff). These provide sampno, perno, and
# placeno as identifiers, as well as identify the trip as part of a chain to
# work or a shopping trip.
chains <- read_csv("chains.csv")

# Travel zones (provided by CMAP R&A staff).
zones <- read_csv("zones.csv") %>%
  select(sampno,     # The household identifier.
         cluster)    # Designates the household's inclusion in one of 11 travel
                     # zones, used for weighting the overall survey.

# Home location flag
home_wip <- region %>%
  # Identify home locations
  filter(home == 1) %>%
  # Keep relevant variables and rename
  select(sampno,
         home_state = state_fips,
         home_county = county_fips,
         home_tract = tract_fips,
         home_lat = latitude,
         home_long = longitude) %>%
  # Keep distinct home locations based on sample
  distinct(sampno,home_county,.keep_all = TRUE)

# # Note - there are four homes out of state and 7 homes with an unknown state.
# # Each of these households also has a home in the CMAP region. They are
# # handled by the two_homes approach below.
#
# out_of_state <-
#   home_wip %>% filter(home_state != 17) %>%
#   select(sampno)
#
# home_wip %>%
#   filter(sampno %in% out_of_state$sampno) %>%


# There are 68 households coded as having home locations in multiple counties. Identify them.
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace multi-county records with a county_fips of 999
home <- home_wip %>%
  mutate(
    # Make everyone's home state Illinois (since we verified that everyone has
    # at least one home in Illinois above).
    home_state = case_when(
      sampno %in% two_homes ~ 17,
      TRUE ~ home_state),
    # Replace multi-county home households with 999 for the county.
    home_county = case_when(
      sampno %in% two_homes ~ 999,
      TRUE ~ home_county),
    # Replace multi-county home households with 999999 for the tract.
    home_tract = case_when(
      sampno %in% two_homes ~ 999999,
      TRUE ~ home_tract
    )) %>%
  # Select distinct rows to eliminate double counting of two-home households.
  distinct(sampno,home_county,.keep_all = TRUE)

# Remove WIP tables
rm(home_wip, two_homes)

# Add combined duration and distance for placeGroup trips
placeGroupStats <- trips %>%
  mutate(
    # Replace negative hdist, distance, and time values (indicating no data)
    # with 0.
    hdist = ifelse(hdist >= 0,hdist,0),
    distance = ifelse(distance >= 0,distance,0),
    travtime = ifelse(travtime >= 0,travtime,0),
    # Put arrival and departure times as datetime objects.
    arrtime = lubridate::ymd_hms(arrtime, tz = "America/Chicago"),
    deptime = lubridate::ymd_hms(deptime, tz = "America/Chicago")) %>%
  # Now group by placeGroup and identifiers to collapse artificially separated
  # trips.
  group_by(sampno,perno,placeGroup) %>%
  # Create new totals
  summarize(
    # For distance and time, sum all trips within a placeGroup.
    hdist_pg = sum(hdist, na.rm = TRUE),
    distance_pg = sum(distance, na.rm = TRUE),
    travtime_pg = sum(travtime, na.rm = TRUE),
    # Identify the latest arrival time within the placeGroup. This represents
    # the end of the trip.
    arrtime_pg = max(arrtime),
    # Identify the latest departure time within the placeGroup. This represents
    # the beginning of the next trip.
    deptime_pg = max(deptime))

# merge datasets
mdt <- trips %>% # 128,229 records
  inner_join(ppl, by = c("sampno", "perno")) %>% # 128,229 records
  inner_join(hh, by = "sampno") %>% # 128,229 records
  inner_join(region, by = c("sampno", "locno")) %>% # 128,229 records
  inner_join(home, by = c("sampno")) %>% # 128,229 records
  inner_join(chains, by = c("sampno", "perno", "placeno")) # 128,229 records

# take care of collapsed trips with placeGroup
mdt <- mdt %>%
  # distinct takes the first row for duplicates, so order by distance to get the
  # "main" mode of the trip.
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>% # 127333 records
  # Add combined distance and time values calculated above (note some trips are
  # missing one or more records)
  left_join(placeGroupStats, by = c("sampno","perno","placeGroup")) %>% # 127333 records
  arrange(desc(sampno),perno,placeGroup) %>%
  # Add lagged trip start time
  mutate(start_times_pg = lag(deptime_pg,1)) %>%
  # Add lagged identifiers
  mutate(sampno_lag = lag(sampno,1),
         perno_lag = lag(perno,1)) %>%
  # Add lagged out_region flag
  mutate(out_region_lag = lag(out_region,1)) %>%
  # Check whether the previous trip is a match
  mutate(check = perno == perno_lag & sampno == sampno_lag) %>%
  # If it isn't a match, change start_times_pg and out_region_lag to NA and -1
  mutate(
    start_times_pg = case_when(
      check ~ start_times_pg,
      !check ~ ymd_hms(NA),
      TRUE ~ ymd_hms(NA)),
    out_region_lag = case_when(
      check ~ out_region_lag,
      !check ~ -1,
      TRUE ~ -1)
    ) %>%
  # Create new out_region flag for trips that neither started nor ended in the
  # CMAP region
  mutate(out_region_trip = case_when(
    out_region == 0 | out_region_lag == 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # Calculate travel time based on actual departure and arrival
  mutate(travtime_pg_calc = (arrtime_pg - start_times_pg)/60) %>%
  select(-sampno_lag,-perno_lag,-check,-out_region_lag)

mdt <- mdt %>%
  # Remove trips that both start and end outside the CMAP region
  filter(out_region_trip==0) %>% # 125752 records
  # Remove trips >= 100 miles
  filter(distance<100) %>% # 125463 records
  # Remove unneeded variables
  select(-c(
    # Flags used for filtering
    out_region_trip,home,
    # Original arrival and departures (superseded by pg)
    deptime,arrtime,
    # Deptime_pg - incorporated into new start_times_pg column
    deptime_pg,
    # Distances and durations (superseded by pg)
    hdist,distance,travtime))

# Remove placegroup stats
rm(placeGroupStats)


# Create a dataset for all respondents (regardless of whether they traveled on
# survey day)
mdt_all_respondents <- ppl %>% # 30,683 records
  inner_join(hh, by = "sampno") %>% # 30,683 records
  inner_join(home, by = c("sampno")) # 30,863 records

#################################################
#                                               #
#                 Load and clean TT             #
#                                               #
#################################################

# Downloaded from CMAP data portal; exported from Microsoft Access database to
# a set of csv files.
setwd("C:/Users/dcomeaux/Chicago Metropolitan Agency for Planning/Transportation Focus Area - Documents/My Daily Travel 2020/2008 survey")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(MPO,        # Whether the person is a respondent for the CMAP survey
                     # (1) or the NIRPC survey (2).

         SAMPN,      # The household identifier.
         SURVEY,     # Whether the household had a 1- or 2-day survey.
         DAY,        # The day of the week of the travel day.
         HHVEH,      # The number of household vehicles.
         INCOM       # Household income.
  )

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN,PERNO,# The identifier for households (SAMPN) and person (PERNO).

         PTRIPS1, PTRIPS2, # The number of trips taken by the traveler on the
                     # first day of the travel diary (PTRIPS1) and, if
                     # applicable, on the second day (PTRIPS2).

         AGE,        # Age (in years). 99 is "Don't know/refused."
         AGEB,       # Range of ages (for those who did not provide an age).

         SCHOL,      # Status of school enrollment.
         SMODE,      # Mode regularly used to get to school.

         WGTP        # The weight for the respondent.
  )

# trips
tt_place <- read_csv("place_public.csv") %>%
  select(SAMPN, PERNO, DAYNO, PLANO, locno, # Identifiers for the household
                     # (SAMPN), person (PERNO), survey day (DAYNO), order of
                     # place visited (PLANO), and location visited (locno).

         TPURP,      # The purpose of the trip.
         MODE,       # The mode of the trip.

         DIST,       # The haversine distance from the start of the trip (i.e.,
                     # the location of the previous record) to the end of the
                     # trip at this location.
         TRPDUR,     # The duration of the trip in minutes.

         ARR_HR, ARR_MIN, # The arrival hour and minute at the place in this
                     # record. This represents the end of the trip identified by
                     # this record.
         DEP_HR, DEP_MIN  # The departure hour and minute from the place in this
                     # record. This represents the beginning of the next trip
                     # (if applicable).
         )

# Location file
tt_location <- read.csv("loc_public.csv") %>%
  select(LOCNO,      # The location identifier.
         FIPS,       # The state and county FIPS code of the location.
         TRACT       # The tract number of the location.
  )

# home location
tt_home <- tt_location %>%
  select(LOCNO, FIPS) %>%
  # Home locations have a 9 as their first number. Identify them.
  mutate(home = case_when(
    substr(LOCNO,1,1) == "9" ~ 1,
    TRUE ~ 0)) %>%
  # Keep only home locations.
  filter(home == 1) %>%
  # Extract FIPS for state from larger FIPS code
  mutate(home_state = as.integer(substr(FIPS,1,2))) %>%
  # Extract FIPS for county from larger FIPS code
  mutate(home_county = as.integer(substr(FIPS,3,5))) %>%
  # Remove unneeded variables
  select(-FIPS,-home) %>%
  # Add to the place-based records to link households and individuals to homes.
  inner_join(tt_place %>% select(SAMPN,PERNO,locno),
             by = c("LOCNO" = "locno"))

# # Check - is each sample associated with exactly one home
# test1 <- tt_home %>% distinct(SAMPN,LOCNO,home_county,.keep_all = TRUE)
#
# number <- test1 %>%
#   group_by(SAMPN) %>%
#   summarize(n = n())
#
# test1 %>%
#   left_join(.,
#             number,
#             by = "SAMPN") %>%
#   filter(n > 1)
#
# test2 <- tt_home %>% distinct(SAMPN,.keep_all = TRUE)
#
# test1 %>%
#   left_join(.,
#             number,
#             by = "SAMPN") %>%
#   filter(is.na(n))
# # Answer: Yes - except for 14 records with no sample number
#
# # remove tests
# rm(test1,test2,number)

# Create flag for home county location
tt_home <- tt_home %>% # 105,554 records
  select(SAMPN,home_county,home_state) %>%
  distinct() # 14,376 records


# Combine datasets
tt <- tt_place %>% # 218,945 records
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>% # 218,945 records
  inner_join(tt_hh, by = c("SAMPN")) %>% # 218,945 records
  inner_join(tt_location, by = c("locno" = "LOCNO")) %>% # 218,945 records
  left_join(tt_home, by = "SAMPN") # 218,945 records (58 records lack a home county; they are kept for analyses that do not rely on home location)

# Flag weekend trips and adjust weights accordingly
tt <- tt %>%
  mutate(
    # Flag weekend travel days (those that had two day surveys that started on a
    # Friday (day 5) or a Sunday (day 7))
    weekend = if_else(SURVEY==2 & DAY==5 & DAYNO==2, 1,
                      if_else(SURVEY==2 & DAY==7 & DAYNO==1, 1, 0)),
    # Identify respondents with two weekday travel days (two-day surveys that
    # began on Monday through Thursday).
    weekdays2 = if_else(SURVEY==2 & DAY==1 |
                          SURVEY==2 & DAY==2 |
                          SURVEY==2 & DAY==3 |
                          SURVEY==2 & DAY==4, 1, 0),
    # If respondent recorded two weekdays, divide weight in half. Otherwise,
    # take original weight.
    weight = if_else(weekdays2==1, WGTP/2, WGTP))

# Identify trips that either start or end within the CMAP region
tt <- tt %>%
  arrange(SAMPN,PERNO) %>%
  # Create a flag - is the location in the nine counties?
  mutate(out_region = ifelse(FIPS %in% cmap_state_nine_counties,
                             0,
                             1)) %>%

  mutate(
    # Use lag to identify the out_region status of the starting point of the
    # trip (from the previous record).
    out_region_lag = lag(out_region,1),
    # Also get lagged identifiers to validate comparison.
    perno_lag = lag(PERNO,1),
    sampn_lag = lag(SAMPN,1)) %>%
  mutate(check = PERNO == perno_lag & SAMPN == sampn_lag) %>%
  # If it isn't a match, change out_region_lag to -1
  mutate(
    out_region_lag = case_when(
      check ~ out_region_lag,
      !check ~ -1,
      TRUE ~ -1)
  ) %>%
  # Create new out_region flag for trips that neither started nor ended in the
  # CMAP region
  mutate(out_region_trip = case_when(
    out_region == 0 | out_region_lag == 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # Remove unneeded variables.
  select(-sampn_lag,-perno_lag,-out_region_lag)


# Identify trip start times (based on lagged DEP_HR and DEP_MIN)
tt <- tt %>%
  arrange(SAMPN,PERNO) %>%
  # Use lag to get the start time of the current trip
  mutate(
    start_hr_lag = lag(DEP_HR,1),
    start_min_lag = lag(DEP_MIN,1)) %>%
  # Calculate the start time as an alternative for non-valid lags
  mutate(
    start_time_calc = lubridate::hms(paste(ARR_HR,ARR_MIN,0)) - TRPDUR * 60) %>%
  # Extract the start hour and minute from the calculated start time.
  mutate(
    start_hr_calc = hour(start_time_calc),
    start_min_calc = minute(start_time_calc)) %>%
  # If it isn't a match, change the lagged times to calculated ones using the
  # "check" variable created above.
  mutate(
    start_hr = case_when(
      check ~ start_hr_lag,
      !check ~ start_hr_calc),
    start_min = case_when(
      check ~ start_min_lag,
      !check ~ start_min_calc)) %>%
  # Remove unneeded variables
  select(-check,-start_hr_lag,-start_min_lag,
         -start_time_calc,-start_hr_calc,-start_min_calc)

# Remove trips not part of the CMAP survey, that start and end outside the nine
# county region, that are over 100 miles, and/or that are on weekends
tt <- tt %>%           # 218945 records
  filter(MPO==1) %>%   # 159856 records
  filter(out_region_trip == 0) %>% # 153437 records
  filter(DIST<100) %>% # 153437 records
  filter(weekend==0)   # 135306 records

# Select the correct number of trips per day (based on day number)
tt <- tt %>%
  mutate(pertrips = ifelse(DAYNO == 1,PTRIPS1,PTRIPS2)) %>%
  # And filter out unneeded variables
  select(-c(
    # Departure times, since these have been added to relevant records as start
    # times
    DEP_HR,DEP_MIN,
    # Flags used for filtering
    MPO,weekend,out_region_trip))


# Create a TT dataset of all respondents (regardless of trip behavior)
tt_all_respondents <- tt_ppl %>% # 32,366 records
  inner_join(tt_hh, by = c("SAMPN")) %>% # 32,366 records
  left_join(tt_home, by = "SAMPN") # 32,366 records (22 records lack a home
    # county; they are kept for analyses that do not rely on home location)


#################################################
#                                               #
#                 Recoding                      #
#                                               #
#################################################


# Recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode = factor(recode(mode,
                              !!!recode_mode_detailed_mdt))) %>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

tt <- tt %>%
  mutate(mode = factor(recode(MODE,
                              !!!recode_mode_detailed_tt))) %>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_tt)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  select(-MODE)

# Recode trip purposes and group into buckets for comparison
mdt <- mdt %>%
  mutate(tpurp = factor(recode(tpurp,
                               !!!recode_tpurp_detailed_mdt))) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_mdt)) %>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels))

tt <- tt %>%
  mutate(tpurp = factor(recode(TPURP,
                               !!!recode_tpurp_detailed_tt))) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_tt))%>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels)) %>%
  select(-TPURP)


# Recode incomes and group into buckets for comparison
mdt <- mdt %>%
  mutate(income = factor(recode(hhinc,
                                !!!recode_income_detailed_mdt))) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt <- tt %>%
  mutate(income = factor(recode(INCOM,
                                !!!recode_income_detailed_tt))) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_tt))

mdt_all_respondents <- mdt_all_respondents %>%
  mutate(income = factor(recode(hhinc,
                                !!!recode_income_detailed_mdt))) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt_all_respondents <- tt_all_respondents %>%
  mutate(income = factor(recode(INCOM,
                                !!!recode_income_detailed_tt))) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_tt))

# Recode into race and ethnicity groups
mdt <- mdt %>%
  mutate(race_eth = recode(race,
                           "1" = "white",
                           "2" = "black",
                           "3" = "asian",
                           "4" = "other",
                           "5" = "other",
                           "6" = "other",
                           "97" = "other",
                           "-8" = "missing",
                           "-7" = "missing")) %>%
  mutate(race_eth = case_when(
    hisp == 1 ~ "hispanic",
    TRUE ~ race_eth)) %>%
  select(-race,-hisp)

mdt_all_respondents <- mdt_all_respondents %>%
  mutate(race_eth = recode(race,
                           "1" = "white",
                           "2" = "black",
                           "3" = "asian",
                           "4" = "other",
                           "5" = "other",
                           "6" = "other",
                           "97" = "other",
                           "-8" = "missing",
                           "-7" = "missing")) %>%
  mutate(race_eth = case_when(
    hisp == 1 ~ "hispanic",
    TRUE ~ race_eth)) %>%
  select(-race,-hisp)

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
                                   "Other trip"))) %>% 
  # Remove chain helper columns
  select(-c(home_to_work,work_to_work,work_to_home,
            home_to_shop,shop_to_shop,shop_to_home))

#################################################
#                                               #
#                 Cleanup                       #
#                                               #
#################################################

# remove recoding helpers
rm(recode_income_buckets_mdt,recode_income_buckets_tt,
   recode_mode_buckets_mdt,recode_mode_buckets_tt,
   recode_tpurp_buckets_mdt,recode_tpurp_buckets_tt,
   recode_income_detailed_mdt,recode_income_detailed_tt,
   recode_mode_detailed_mdt,recode_mode_detailed_tt,
   recode_tpurp_detailed_mdt,recode_tpurp_detailed_tt)

setwd("~/GitHub/mydailytravel")
