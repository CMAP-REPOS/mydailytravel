# This file loads and cleans the data used in analyses of My Daily Travel and
# Travel Tracker. The file outputs a number of tables; the most important are:
#
# - mdt: this table represents the trips taken in the MDT survey, with added
#   characteristics about the person, household, and location of the trip. Trips
#   that are within the same "place group" are collapsed into one trip with the
#   cumulative travel time, distance, and start/end times. Trips that leave the
#   CMAP region or are greater than 100 miles are excluded. Mode, purpose,
#   income, and race and ethnicity are added and recoded, using lists and
#   vectors from recoding.R where relevant.
#
# - tt: this table represents the trips taken in the TT survey, with added
#   characteristics about the person, household, and location of the trip. Trips
#   on the weekend are excluded and weights are adjusted to account for surveys
#   that had two weekday trip diaries. Trips out of the CMAP region and trips
#   greater than 100 miles are also excluded.
#
# - mdt_all_respondents and tt_all_respondents: these tables includes person and
#   household information for all respondents in the survey, whether or not they
#   traveled, with income recoded using the definitions from recoding.R.



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
source("recoding.R")

# Load My Daily Travel
setwd("C:/Users/dcomeaux/Chicago Metropolitan Agency for Planning/Transportation Focus Area - Documents/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, arrtime, deptime, travtime, tpurp, distance, hdist)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, aage, schol, smode, hisp, race, pertrips, wtperfin, tcoff, tcdays, emply_ask, jobs, wplace)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, loctype, out_region, county_fips, tract_fips, home)

# Trip chains
chains <- read_csv("chains.csv")

# Travel zones
zones <- read_csv("zones.csv") %>%
  select(sampno,cluster)

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
         travtime = ifelse(travtime >= 0,travtime,0),
         arrtime = ymd_hms(arrtime, tz = "America/Chicago"),
         deptime = ymd_hms(deptime, tz = "America/Chicago")) %>%
  group_by(sampno,perno,placeGroup) %>%
  summarize(hdist_pg = sum(hdist, na.rm = TRUE),
            distance_pg = sum(distance, na.rm = TRUE),
            travtime_pg = sum(travtime, na.rm = TRUE),
            arrtime_pg = max(arrtime),
            deptime_pg = min(deptime))

# merge datasets
mdt <- trips %>% # 128,229 records
  inner_join(ppl, by = c("sampno", "perno")) %>% # 128,229 records
  inner_join(hh, by = "sampno") %>% # 128,229 records
  inner_join(region, by = c("sampno", "locno")) %>% # 128,229 records
  inner_join(home, by = c("sampno")) %>% # 128,229 records
  inner_join(chains, by = c("sampno", "perno", "placeno")) # 128,229 records

# take care of collapsed trips with placeGroup
mdt <- mdt %>%
  # distinct takes the first row for duplicates, so order by distance to get right mode
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>% # 127333 records
  # add combined distance and time values calculated above (note some trips are missing one or more records)
  left_join(placeGroupStats, by = c("sampno","perno","placeGroup")) %>% # 127333 records
  arrange(desc(sampno),perno,placeGroup) %>%
  # Add lagged trip start time
  mutate(start_times_pg = lag(deptime_pg,1)) %>%
  mutate(sampno_lag = lag(sampno,1),
         perno_lag = lag(perno,1)) %>%
  # Add lagged out_region flag
  mutate(out_region_lag = lag(out_region,1)) %>%
  # Check whether the previous trip is a match
  mutate(check = perno == perno_lag & sampno == sampno_lag) %>%
  # If it isn't a match, change start_times_pg and out_region_lag to NA
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
  # Create new out_region flag for trips that neither started nor ended in the CMAP region
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
  filter(distance<100) # 125463 records

# Remove placegroup stats
rm(placeGroupStats)


# Create a dataset for all respondents (regardless of whether they traveled on survey day)
mdt_all_respondents <- ppl %>% # 30,683 records
  inner_join(hh, by = "sampno") %>% # 30,683 records
  inner_join(home, by = c("sampno")) # 30,863 records

#################################################
#                                               #
#                 Load and clean TT             #
#                                               #
#################################################

# Downloaded from CMAP data portal; exported from Microsoft Access database to csv.
setwd("C:/Users/dcomeaux/Chicago Metropolitan Agency for Planning/Transportation Focus Area - Documents/My Daily Travel 2020/2008 survey")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(SAMPN, SURVEY, ASSN, DAY, HHVEH, INCOM)

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN, PERNO, MPO_per = MPO, SURVEY, PTRIPS1, PTRIPS2, AGE, AGEB, SCHOL, SMODE, HISP, RACE, WGTP)

# trips
tt_place <- read_csv("place_public.csv") %>%
  select(MPO, SAMPN, PERNO, DAYNO, PLANO, locno, TPURP, MODE, DIST, TRPDUR)

# Location file
tt_location <- read.csv("loc_public.csv") %>%
  select(LOCNO, FIPS, TRACT)

# home location
tt_home <- tt_location %>%
  select(LOCNO, FIPS) %>%
  mutate(home = case_when(
    substr(LOCNO,1,1) == "9" ~ 1,
    TRUE ~ 0)) %>%
  filter(home == 1) %>%
  # Extract FIPS for county from larger FIPS code
  mutate(home_county = as.integer(substr(FIPS,3,5))) %>%
  select(-FIPS,home) %>%
  inner_join(.,
             tt_place %>% select(SAMPN,PERNO,PLANO,locno),
             by = c("LOCNO" = "locno"))

# Check - is each sample associated with exactly one home
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
  select(SAMPN,home_county) %>%
  distinct() # 14,376 records


# Combine datasets
tt <- tt_place %>% # 218,945 records
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>% # 218,945 records
  inner_join(tt_hh, by = c("SAMPN", "SURVEY")) %>% # 218,945 records
  inner_join(tt_location, by = c("locno" = "LOCNO")) %>% # 218,945 records
  left_join(tt_home, by = "SAMPN") # 218,945 records (58 records lack a home county; they are kept for analyses that do not rely on home location)

# Flag weekend trips and adjust weights accordingly
tt <- tt %>%
  mutate(weekend = if_else(SURVEY==2 & DAY==5 & DAYNO==2, 1,
                           if_else(SURVEY==2 & DAY==7 & DAYNO==1, 1, 0)),
         weekdays2 = if_else(SURVEY==2 & DAY==1 |
                               SURVEY==2 & DAY==2 |
                               SURVEY==2 & DAY==3 |
                               SURVEY==2 & DAY==4, 1, 0),
         # If respondent recorded two weekdays, divide weight in half
         weight = if_else(weekdays2==1, WGTP/2, WGTP))

# Identify trips that either start or end within the CMAP region
tt <- tt %>%
  arrange(SAMPN,PERNO) %>%
  mutate(out_region = ifelse(FIPS %in% cmap_state_nine_counties,
                             1,
                             0)) %>%
  mutate(out_region_lag = lag(out_region,1),
         perno_lag = lag(PERNO,1),
         sampn_lag = lag(SAMPN,1)) %>%
  mutate(check = PERNO == perno_lag & SAMPN == sampn_lag) %>%
  # If it isn't a match, change =out_region_lag to NA
  mutate(
    out_region_lag = case_when(
      check ~ out_region_lag,
      !check ~ -1,
      TRUE ~ -1)
  ) %>%
  # Create new out_region flag for trips that neither started nor ended in the CMAP region
  mutate(out_region_trip = case_when(
    out_region == 0 | out_region_lag == 0 ~ 0,
    TRUE ~ 1
  )) %>%
  select(-sampn_lag,-perno_lag,-check,-out_region_lag)


# Remove trips not part of the CMAP survey, that start or end (but not both)
# outside the nine county region, are over 100 miles, and/or on weekends
tt <- tt %>%           # 218945 records
  filter(MPO==1) %>%   # 159856 records
  filter(out_region_trip == 1) %>% # 153437 records
  filter(DIST<100) %>% # 153437 records
  filter(weekend==0)   # 135306 records

# Select the correct number of trips per day (based on day number)
tt <- tt %>%
  mutate(pertrips = ifelse(DAYNO == 1,PTRIPS1,PTRIPS2))


# Create a TT dataset of all respondents (regardless of trip behavior)
tt_all_respondents <- tt_ppl %>% # 32,366 records
  inner_join(tt_hh, by = c("SAMPN", "SURVEY")) %>% # 32,366 records
  left_join(tt_home, by = "SAMPN") # 32,366 records (22 records lack a home county; they are kept for analyses that do not rely on home location)


#################################################
#                                               #
#                 Recoding                      #
#                                               #
#################################################


# recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode = factor(mode),
         mode = recode_factor(mode,
                              !!!recode_mode_detailed_mdt))%>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

tt <- tt %>%
  mutate(MODE = factor(MODE),
         MODE = recode_factor(MODE,
                              !!!recode_mode_detailed_tt)) %>%
  mutate(mode_c = fct_collapse(MODE,
                               !!!recode_mode_buckets_tt)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))



# Recode trip purposes and group into buckets for comparison
mdt <- mdt %>%
  mutate(tpurp = factor(tpurp)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_mdt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_mdt)) %>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels))

tt <- tt %>%
  mutate(tpurp = factor(TPURP)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_tt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_tt))%>%
  mutate(tpurp_c = factor(tpurp_c,levels = tpurp_c_levels))


# Recode incomes and group into buckets for comparison
mdt <- mdt %>%
  mutate(income = factor(hhinc),
         income = recode_factor(income,!!!recode_income_detailed_mdt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt <- tt %>%
  mutate(income = factor(INCOM),
         income = recode_factor(income,!!!recode_income_detailed_tt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_tt))

mdt_all_respondents <- mdt_all_respondents %>%
  mutate(income = factor(hhinc),
         income = recode_factor(income,!!!recode_income_detailed_mdt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt_all_respondents <- tt_all_respondents %>%
  mutate(income = factor(INCOM),
         income = recode_factor(income,!!!recode_income_detailed_tt)) %>%
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
    TRUE ~ race_eth))

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
