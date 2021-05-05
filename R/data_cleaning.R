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

# # Run code once per machine to install GitHub-based packages (not on CRAN)
# devtools::install_github("CMAP-REPOS/cmapplot")
# devtools::install_github("CMAP-REPOS/cmapgeo")
# devtools::install_github("coolbutuseless/ggpattern")

library(tidyverse)
library(lubridate)
library(RODBC)
library(cmapgeo)
library(sf)

#################################################
#                                               #
#                 Load and clean MDT            #
#                                               #
#################################################

# Load recoding information
setwd("~/GitHub/mydailytravel")
source("R/recoding.R")

# # Load My Daily Travel (download from internet)
# mdt_zip <- tempfile()
# download.file("https://datahub.cmap.illinois.gov/dataset/02a047a1-e7b8-4ca7-b754-54f2b9bfeab6/resource/c9e82b87-0b4c-45ea-9c06-9cdebdb7071f/download/MyDailyTravelData.zip",mdt_zip,quiet = T)

# Load My Daily Travel (from source)
mdt_zip <- "./source/MyDailyTravelData.zip"


# trips
trips <- read.csv(unzip(mdt_zip,files = "place.csv")) %>%
  select(sampno, perno, locno, placeno, # Identifiers for the household (sampno),
                     # the person (perno), the destination of the trip (locno),
                     # and the order in which that place was visited in the
                     # context of the entire day (placeno).

         placeGroup, # A supplement for placeno that combines continuous trips
                     # that were artificially broken apart into multiple
                     # segments.

         tpurp,      # The purpose of the trip.
         mode,       # The mode of the trip.
         
         hhparty,    # The number of travelers (including the respondent) on the 
         party,      # trip, both household and non-household.

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
ppl <- read.csv(unzip(mdt_zip,files = "person.csv")) %>%
  select(sampno, perno, # Identifiers for household (sampno) and person (perno)

         age,        # The age (in years) of the respondent.
         aage,       # Range of ages (only for those without an answer to age).
         age18,      # Either younger than 18 (2) or 18 or older (1)
         
         sex,        # Either male (1) or female (2)

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
         tnc_purp,   # For what type of trip do you use the service most?
         
         
         wtperfin    # The weight for the respondent.
         )

veh <- read.csv(unzip(mdt_zip,files = "vehicle.csv")) %>% 
  select(sampno,     # Household and vehicle identifiers
         vehno,      
         
         fuel,       # Fuel type of vehicle
         
         parkd       # Location of vehicle parking
         )

# household info
hh <- read.csv(unzip(mdt_zip,files = "household.csv")) %>%
  select(sampno,     # Identifier for the household.
         hhinc,      # Household income (in dollars).
         hhsize,     # Household size.
         hhveh,      # Number of vehicles in the household.
         wthhfin,
         travday     # The day of the week for the household's travel diary.
  )

# location file w/region flag
location_raw <- read.csv(unzip(mdt_zip,files = "location.csv")) %>%
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

file.remove("place.csv","person.csv","household.csv","location.csv","vehicle.csv")
rm(mdt_zip)

# Travel zones (provided by CMAP RAP staff).
zones <- read_csv("source/zones.csv") %>%
  select(sampno,     # The household identifier.
         cluster)    # Designates the household's inclusion in one of 11 travel
                     # zones, used for weighting the overall survey.


# Municipalities of location IDs (provided by CMAP RAP staff).
munis <- read_csv("source/loc_city.csv") %>%
  select(sampno,     # The household identifier
         locno,      # The location identifier (unique with HH identifier)
         city)       # The municipality of the location (if in an incorporated area)

# Trip chains (provided by CMAP RAP staff). Chains represent continuous journeys
# that may have more than one destination along a larger circuit (i.e., a
# commute to work with a stop at a coffee shop in the morning, lunch at mid-day,
# and a visit to the grocery store in the evening).
chains <- read_csv("source/chains.csv") %>% 
  select(sampno,    # The household identifier
         perno,     # The person identifier
         placeno,   # The place number
         
         work_chain,   # Whether the trip is part of a chain to work
         
         home_to_work, # For work trip chains, is this trip part of the journey 
         work_to_work, # from home to work, a trip while at work, or a trip part
         work_to_home, # of the journey home from work?
         
         shop_chain,   # Whether the trip is part of a chain to shop
         
         home_to_shop, # Similar for shopping chains
         shop_to_shop,
         shop_to_home)
         
# Add flags to locations
location <- 
  location_raw %>% 
  # # Add municipality locations (ARCHIVED)
  # left_join(
  #   municipality_locations_mdt,
  #   by = c("sampno","locno")
  # ) %>%
  # Add travel zones - note these correspond to the home location of the
  # household, and not the location
  left_join(zones, by = "sampno") %>% 
  # Add municipality names
  left_join(munis, by = c("sampno","locno")) %>% 
  
  mutate(county_chi_name = case_when(
    # Use the three Chicago travel zones to ID Chicago locations (note this
    # appears to excludes portions of the Far Northwest Side (portions of
    # Norwood Park and Edison Park) that fall within the Chicago city limits -
    # however, given the assignment of XY coordinates to the centroids of
    # tracts, neither method allows for perfect sorting. We thus use travel
    # zones, since they were used in the weighting of the survey.
    city == "CHICAGO" ~ "Chicago", 
    # Everything else in Cook County belongs to Suburban Cook
    state_fips == 17 & county_fips == 31 ~ "Suburban Cook",
    # And everything else is assigned using state and county FIPS with no issue.
    state_fips == 17 & county_fips == 37 ~ "DeKalb",
    state_fips == 17 & county_fips == 63 ~ "Grundy",
    state_fips == 17 & county_fips == 97 ~ "Lake",
    state_fips == 17 & county_fips == 43 ~ "DuPage",
    state_fips == 17 & county_fips == 89 ~ "Kane",
    state_fips == 17 & county_fips == 93 ~ "Kendall",
    state_fips == 17 & county_fips == 111 ~ "McHenry",
    state_fips == 17 & county_fips == 197 ~ "Will")) %>% 
  # And add a flag for whether the location is in the set of locations that were
  # coded in TT (and thus have distances associated with them)
  mutate(outside_tt = case_when(
    state_fips == 17 & county_fips %in% c(cmap_seven_counties,63) ~ 0,
    state_fips == 18 & county_fips %in% c(89,91,127) ~ 0,
    TRUE ~ 1
    ))

# # ARCHIVE - add back if using location assignment of centroids
# rm(municipality_locations_mdt,location_xy_mdt)

# Home location flag
home_wip <- location %>%
  # Identify home locations
  filter(loctype == 1) %>%
  # Exclude home locations outside the CMAP region
  filter(county_fips %in% cmap_nine_counties,
         state_fips == "17") %>%
  # Keep relevant variables and rename
  select(sampno,
         home_state = state_fips,
         home_county = county_fips,
         home_tract = tract_fips,
         home_lat = latitude,
         home_long = longitude,
         county_chi_name) %>%
  # Keep distinct home locations based on sample. Note this will collapse
  # records for any households that have two homes in the same county (as
  # intended).
  distinct(sampno,home_state,home_county,county_chi_name,.keep_all = TRUE)

# There is 1 household that is recorded as having a home in both Chicago and
# Suburban Cook
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace that one household with a separate county FIPS
home <- home_wip %>%
  mutate(
    # Make everyone's home state Illinois.
    home_state = 17,
    # Replace multi-county home households with 999
    home_county = case_when(
      # The household with a home in Chicago and Suburban Cook
      sampno %in% two_homes ~ 999,
      # Otherwise, keep as before
      TRUE ~ as.double(home_county)),
    # Replace multi-county home households with 999999 for the tract.
    home_tract = case_when(
      sampno %in% two_homes ~ 999999,
      TRUE ~ as.double(home_tract)
    )) %>%
  # Select distinct rows to eliminate double counting of two-home households.
  distinct(sampno,home_county,.keep_all = TRUE) %>%
  # Create flag for home county with Chicago and suburban Cook
  mutate(home_county_chi = case_when(
    home_county == 999 ~ "Homes in multiple jurisdictions (Chicago/Cook)",
    TRUE ~ county_chi_name)) %>% 
  # Create flag for Chicago vs. Suburban Cook vs. Other
  mutate(geog = case_when(
    home_county_chi %in% c("Chicago","Suburban Cook",
                           "Homes in multiple jurisdictions (Chicago/Cook)") ~ home_county_chi,
    # Assign the rest as "Other suburban counties".
    TRUE ~ "Other suburban counties"
  )) %>% 
  mutate(geog = factor(geog, levels = c("Chicago","Suburban Cook",
                                        "Other suburban counties",
                                        "Homes in multiple jurisdictions (Chicago/Cook)"))) %>% 
  # Remove unnecessary variable
  select(-county_chi_name)

# Remove WIP tables and Chicago tracts
rm(home_wip, two_homes, location_raw)

# Add combined duration and distance for placeGroup trips. These trips are
# single trips that were split across multiple records. We merge them, keeping
# the trip with the longest distance as the primary record for mode and purpose,
# but extracting the total distance, travel time, and start/end times and
# locations.
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
  # First, we need to know where the trip is going to
  arrange(placeno) %>% 
  # Flag the last location
  mutate(locno_last = tail(locno,1)) %>% 
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
    deptime_pg = max(deptime),
    # And keep the location of the last trip
    locno_pg = first(locno_last)
    )

# Merge datasets (except location, which has to be added after accounting for
# placegroup trips)
mdt_wip1 <- trips %>% # 128,229 records
  inner_join(ppl, by = c("sampno", "perno")) %>% # 128,229 records
  inner_join(hh, by = "sampno") %>% # 128,229 records
  inner_join(home, by = c("sampno")) %>% # 128,229 records
  inner_join(chains, by = c("sampno", "perno", "placeno")) # 128,229 records

# take care of collapsed trips with placeGroup and add lags for trip origins
mdt_wip2 <- mdt_wip1 %>%
  # distinct takes the first row for duplicates, so order by distance to get the
  # "main" mode of the trip.
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>% # 127333 records
  # Add combined distance and time values calculated above (note some trips are
  # missing one or more records)
  left_join(placeGroupStats, by = c("sampno","perno","placeGroup")) %>% # 127333 records
  # Fill NAs in locno_pg
  mutate(locno_pg = case_when(
    is.na(locno_pg) ~ locno,
    TRUE ~ locno_pg
  )) %>% 
  # Add location information
  inner_join(location, by = c("sampno", "locno_pg" = "locno")) %>% # 128,229 records
  
  # Sort to allow lagging
  arrange(sampno,perno,placeGroup) %>%
  # Group by household and traveler ID for lagged figures
  group_by(sampno,perno) %>% 
  # Add lagged trip start time
  mutate(start_times_pg = lag(deptime_pg,1)) %>%
  # Add lagged trip origin location and TT inclusion
  mutate(county_chi_name_lag = lag(county_chi_name,1),
         outside_tt_lag = lag(outside_tt,1)) %>% 
  # Add lagged out_region flag
  mutate(out_region_lag = lag(out_region,1)) %>%
  # Ungroup
  ungroup() %>% 
  # Create new out_region flag for trips that neither started nor ended in the
  # CMAP region. We assign 0 to trips that started and/or ended in the region,
  # and 1 to other trips.
  mutate(out_region_trip = case_when(
    out_region == 0 | out_region_lag == 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # Create a similar flag for trips that are in the TT travel region
  mutate(out_tt_trip = case_when(
    # If both the origin and destination are inside the TT region, it is 0
    outside_tt == 0 & outside_tt_lag == 0 ~ 0,
    # Otherwise, it is 1 (outside the TT region)
    TRUE ~ 1
  )) %>% 
  # Calculate travel time based on actual departure and arrival
  mutate(travtime_pg_calc = case_when(
    # If the trip start time is NA, use original figure
    is.na(start_times_pg) ~ travtime_pg,
    # If the trip start time is after the arrival time, use original figure
    arrtime_pg < start_times_pg ~ travtime_pg,
    # Otherwise, calculate travel time based on trip start time and trip arrival time
    TRUE ~ as.numeric((arrtime_pg - start_times_pg)/60))) %>%
  select(-c(out_region_lag,outside_tt,outside_tt_lag))

mdt <- 
  mdt_wip2 %>% # 127333 records
  # Keep only trips that have an out_region_trip value of 0, implying they start
  # and/or end in the region
  filter(out_region_trip==0) %>% # 125752 records
  # Remove trips >= 100 miles
  filter(distance<100) %>% # 125463 records
  # Remove unneeded variables
  select(-c(
    # Flags used for filtering
    out_region_trip,home,
    # Original arrival and departures (superseded by pg)
    deptime,arrtime,
    # Original location number (superseded by pg)
    locno,
    # Deptime_pg - incorporated into new start_times_pg column
    deptime_pg,
    # Distances and durations (superseded by pg)
    hdist,distance,travtime))

# Remove placegroup stats
rm(placeGroupStats,mdt_wip1,mdt_wip2)


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

# The underlying Access database can be downloaded from the CMAP data portal.
# For unknown reasons, if directly downloaded through R, the Access database is
# corrupted. Those interested in replicating or extending on this analysis can
# download the file at the following link, and adjust the target directory for
# the `odbcConnectAccess2007` function call accordingly:
# "https://datahub.cmap.illinois.gov/dataset/1f424666-3885-493d-8774-81ba7ac625f2/resource/092af96e-9c7a-4182-a1e1-ecff588a9de0/download/TravelTrackerRevisedSurveyData.accdb"Downloaded

con <- odbcConnectAccess2007("C:/Users/dcomeaux/Chicago Metropolitan Agency for Planning/Transportation Focus Area - Documents/My Daily Travel 2020/2008 survey/TravelTrackerRevisedSurveyData.accdb")

# Household
tt_hh <- sqlFetch(con,'hh_public') %>%
  select(MPO,        # Whether the person is a respondent for the CMAP survey
                     # (1) or the NIRPC survey (2).

         SAMPN,      # The household identifier.
         SURVEY,     # Whether the household had a 1- or 2-day survey.
         DAY,        # The day of the week of the travel day.
         HHVEH,      # The number of household vehicles.
         INCOM,      # Household income.
         WGTHH       # Household weight
  )

# people
tt_ppl <- sqlFetch(con,"per_public") %>%
  select(SAMPN,PERNO,# The identifier for households (SAMPN) and person (PERNO).

         PTRIPS1, PTRIPS2, # The number of trips taken by the traveler on the
                     # first day of the travel diary (PTRIPS1) and, if
                     # applicable, on the second day (PTRIPS2).

         AGE,        # Age (in years). 99 is "Don't know/refused."
         AGEB,       # Range of ages (for those who did not provide an age).
         
         GEND,       # Gender (1 is Male, 2 is Female, 9 is Refused)

         SCHOL,      # Status of school enrollment.
         SMODE,      # Mode regularly used to get to school.
         
         EMPLY,      # Is this person employed? 1 is full-time, 2 is part-time
         WLOC,       # Work location?
         JOBS,       # How many jobs?
         TELEW,      # Telecommute to work (only for respondents who work outside the home)
         WHOME,      # How often does the respondent telecommute? 1 is almost every day, 2 is once a week or more
         
         RACE,       # The race of the respondent (note - only asked for head of household)
         HISP,       # The Hispanic status of the respondent (note - only asked for head of household)

         WGTP        # The weight for the respondent.
  )

# trips
tt_place <- sqlFetch(con,"place_public") %>%
  select(SAMPN, PERNO, DAYNO, PLANO, locno, # Identifiers for the household
                     # (SAMPN), person (PERNO), survey day (DAYNO), order of
                     # place visited (PLANO), and location visited (locno).

         TPURP,      # The purpose of the trip.
         MODE,       # The mode of the trip.

         DIST,       # The haversine distance from the start of the trip (i.e.,
                     # the location of the previous record) to the end of the
                     # trip at this location.
         TRPDUR,     # The duration of the trip in minutes.
         
         TOTTR,      # Total number of travelers
         HHMEM,      # Total number of household members traveling

         ARR_HR, ARR_MIN, # The arrival hour and minute at the place in this
                     # record. This represents the end of the trip identified by
                     # this record.
         DEP_HR, DEP_MIN  # The departure hour and minute from the place in this
                     # record. This represents the beginning of the next trip
                     # (if applicable).
         )

# Location file
tt_location <- sqlFetch(con,"loc_public") %>%
  select(LOCNO,      # The location identifier.
         FIPS,       # The state and county FIPS code of the location.
         TRACT,      # The tract number of the location.
         X_PUBLIC,   # The centroid of the census tract of location
         Y_PUBLIC    # The centroid of the census tract of location
  )

tt_veh <- sqlFetch(con,"veh_public") %>%
  select(SAMPN,     # Household and vehicle identifiers
         VEHNO,      
         
         PARKD       # Location of vehicle parking
  )

# Close the database connection
odbcClose(con)


# Add status in City of Chicago to location file

# Add geometry to latitude and longitude for locations
location_xy_tt <- st_as_sf(tt_location %>% filter(!is.na(X_PUBLIC)),
                           coords = c(x = "X_PUBLIC", y = "Y_PUBLIC"),
                           crs = 4326)

location_xy_tt <- sf::st_transform(location_xy_tt,crs = cmapgeo::cmap_crs)

# Join municipalities with the location file to determine which locations fall
# within municipal borders
municipality_locations_tt <- 
  st_join(x = location_xy_tt,
          y = cmapgeo::municipality_sf,
          join = st_within) %>% 
  # Remove geometry
  as_tibble() %>% 
  # Keep relevant variables
  select(LOCNO,municipality)

# Add municipality flags to locations
tt_location <- 
  left_join(
    tt_location,
    municipality_locations_tt,
    by = "LOCNO"
  ) %>% 
  mutate(county_chi_name = case_when(
    municipality == "Chicago" & FIPS == 17031  ~ "Chicago",
    FIPS == 17031 ~ "Suburban Cook",
    FIPS == 17063 ~ "Grundy",
    FIPS == 17097 ~ "Lake",
    FIPS == 17043 ~ "DuPage",
    FIPS == 17089 ~ "Kane",
    FIPS == 17093 ~ "Kendall",
    FIPS == 17111 ~ "McHenry",
    FIPS == 17197 ~ "Will"))

rm(municipality_locations_tt,location_xy_tt)

# home location
tt_home <- tt_location %>%
  select(LOCNO, FIPS,home_county_chi = county_chi_name) %>%
  # Home locations have a 9 as their first number. Identify them.
  mutate(home = case_when(
    substr(LOCNO,1,1) == "9" ~ 1,
    TRUE ~ 0)) %>%
  # Keep only home locations.
  filter(home == 1) %>%
  # Create a flag for Chicago vs Suburban Cook vs other
  mutate(geog = case_when(
    home_county_chi == "Chicago" ~ "Chicago",
    home_county_chi == "Suburban Cook" ~ "Suburban Cook",
    is.na(home_county_chi) ~ "Other - NA",
    TRUE ~ "Other suburban counties")) %>% 
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
  select(SAMPN,home_county,home_state,home_county_chi,geog) %>%
  distinct() # 14,376 records


# Combine datasets
tt_wip1 <- tt_place %>% # 218,945 records
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>% # 218,945 records
  inner_join(tt_hh, by = c("SAMPN")) %>% # 218,945 records
  inner_join(tt_location, by = c("locno" = "LOCNO")) %>% # 218,945 records
  left_join(tt_home, by = "SAMPN") # 218,945 records (58 records lack a home county; they are kept for analyses that do not rely on home location)

# Flag weekend trips and adjust weights accordingly
tt_wip2 <- tt_wip1 %>%
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

# Create a TT dataset of all respondents (regardless of trip behavior)
tt_all_respondents <- tt_ppl %>% # 32,366 records
  inner_join(tt_hh, by = c("SAMPN")) %>% # 32,366 records
  left_join(tt_home, by = "SAMPN") %>%  # 32,366 records (22 records lack a home
                                        # county; they are kept for analyses
                                        # that do not rely on home location)
  # Keep only CMAP survey respondents
  filter(MPO==1) 

# Identify trips that either start or end within the CMAP region
tt_wip3 <- tt_wip2 %>%
  arrange(SAMPN,PERNO) %>%
  # Create a flag - is the location in the nine counties?
  mutate(out_region = ifelse(FIPS %in% cmap_state_nine_counties,
                             0, # 0 if not - these are in-region locations
                             1) # 1 if true - these are out-region locations
  ) %>%
  
  # Group by SAMPN and PERNO for lagging
  group_by(SAMPN,PERNO) %>% 
  # Use lag to identify the out_region status of the starting point of the
  # trip (from the previous record).
  mutate(out_region_lag = lag(out_region,1)) %>%
  # Use lag to get the start time of the current trip
  mutate(
    start_hr_lag = lag(DEP_HR,1),
    start_min_lag = lag(DEP_MIN,1)) %>%
  ungroup() %>% 
  # Create new out_region flag for trips that neither started nor ended in the
  # CMAP region
  mutate(out_region_trip = case_when(
    out_region == 0 | out_region_lag == 0 ~ 0,
    TRUE ~ 1
  )) %>%
  # Calculate the start time as an alternative for non-valid lags
  mutate(
    start_time_calc = lubridate::hms(paste(ARR_HR,ARR_MIN,0)) - TRPDUR * 60) %>%
  # Extract the start hour and minute from the calculated start time.
  mutate(
    start_hr_calc = hour(start_time_calc),
    start_min_calc = minute(start_time_calc)) %>%
  # If the lagged variables are NA, that means there was no preceding records.
  # These should typically be the first record, but add values in case some
  # records are incomplete.
  mutate(
    start_hr = case_when(
      !is.na(start_hr_lag) ~ start_hr_lag,
      TRUE ~ start_hr_calc),
    start_min = case_when(
      !is.na(start_min_lag) ~ start_min_lag,
      TRUE ~ start_min_calc)) %>%
  # Remove unneeded variables.
  select(-out_region_lag,-start_hr_lag,-start_min_lag,
         -start_time_calc,-start_hr_calc,-start_min_calc)


# Remove trips not part of the CMAP survey, that start and end outside the nine
# county region, that are over 100 miles, and/or that are on weekends
tt <- tt_wip3 %>%           # 218945 records
  filter(MPO==1) %>%   # 159856 records
  filter(out_region_trip == 0) %>% # 153437 records
  filter(DIST<100) %>% # 153437 records
  filter(weekend==0) %>% # 135306 records
  # Select the correct number of trips per day (based on day number)
  mutate(pertrips = ifelse(DAYNO == 1,PTRIPS1,PTRIPS2)) %>%
  # And filter out unneeded variables
  select(-c(
    # Departure times, since these have been added to relevant records as start
    # times
    DEP_HR,DEP_MIN,
    # Flags used for filtering
    MPO,weekend,out_region_trip))


rm(tt_wip1,tt_wip2,tt_wip3)


#################################################
#                                               #
#                 Recoding                      #
#                                               #
#################################################


# Recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode_n = mode,
         mode = factor(recode(mode,
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
    TRUE ~ race_eth)) 



# Recode into race and ethnicity groups
tt <- tt %>%
  mutate(race_eth = recode(RACE,
                           "1" = "white",
                           "2" = "black",
                           "3" = "other",
                           "4" = "asian",
                           "7" = "other",
                           "9" = "missing")) %>%
  mutate(race_eth = case_when(
    HISP == 1 ~ "hispanic",
    TRUE ~ race_eth)) %>%
  select(-RACE,-HISP)

tt_all_respondents <- tt_all_respondents %>%
  mutate(race_eth = recode(RACE,
                           "1" = "white",
                           "2" = "black",
                           "3" = "other",
                           "4" = "asian",
                           "7" = "other",
                           "9" = "missing")) %>%
  mutate(race_eth = case_when(
    HISP == 1 ~ "hispanic",
    TRUE ~ race_eth))

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
