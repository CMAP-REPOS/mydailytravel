# This script analyzes average travel behavior in MDT and TT

#################################################
#                                               #
#                 Library loading               #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(forcats)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


### MY DAILY TRAVEL - closely aligns with Sarah's work, seem to be right

# filter out trips we don't want to evaluate
avgtravel_mdt <-
  mdt %>%                    # 125463 records
  filter(
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place. age < 0 are all respondents without a numeric age value.
    age >= 5 |               # 125459 records
      (age <0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8)) |
      sampno %in% c(70038312, 70051607)) %>%
  # Filter out "beginning" trips
  filter(mode != "beginning") %>% # 97374 records
  # Keep only people who traveled
  filter(pertrips > 0) %>%   # 97373 records
  # Eliminate 0 distance trips (network - use for MDT-specific analyses)
  filter(distance_pg > 0) %>%   # 97316 records
  # Eliminate 0 distance trips (haversine - only use for comparisons with TT)
  # filter(hdist_pg > 0) %>%   # 96383 records
  # Add calculation for weighted distance
  mutate(hdist_pg_weight = hdist_pg * wtperfin,
         dist_pg_weight = distance_pg * wtperfin)

# Calculate total number of daily travelers who take at least one trip
daily_travelers_mdt <-
  avgtravel_mdt %>%
  select(sampno,perno,wtperfin) %>%
  distinct() %>%
  summarize(total_travelers = sum(wtperfin))
# 
# # ARCHIVED
# ##### TRAVEL TRACKER - note that due to differences in collection of distances
# ##### for out-region trips, these numbers are likely not comparable with MDT.
# 
# avgtravel_tt <-
#   tt %>%                    # 139769 records
#   # Keep only trips by travelers at least 5 years old. Note that 99 is DK/RF for
#   # AGE.
#   filter((AGE >= 5 & AGE < 99) | 
#            (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
#            (AGEB == 2 & AGE == 99)) %>% # 132680 records
#   # Filter out the first record for each traveler (PLANO == 1)
#   filter(PLANO != 1) %>%    # 105568 records
#   # Include only travelers who made at least one trip
#   filter(pertrips > 0) %>%  # 105568 records
#   # Exclude zero distance trips (note that TT did not capture distances for
#   # trips outside the region, so this means that travelers who only traveled to
#   # or from the region, but not within the region, will be excluded).
#   filter(DIST > 0) %>%      # 100573 records
#   # Calculate weighted distances
#   mutate(dist_weight = DIST * weight)
# 
# # Calculate total number of daily travelers who take at least one trip
# daily_travelers_tt <-
#   avgtravel_tt %>%
#   select(SAMPN,PERNO,DAYNO,weight) %>%
#   distinct() %>%
#   summarize(total_travelers = sum(weight))


# # How many of the travelers are excluded based on the out-of-region travel
# # identified above? The weights are ~210K. Other differences from Sarah's
# # analysis appear to be due to her inclusion of weekend travelers in the
# # overall pie.
# tt %>%
#   group_by(SAMPN,PERNO,DAYNO) %>%
#   summarize(DIST = sum(DIST),
#             pertrips = first(pertrips),
#             weight = first(weight)) %>%
#   filter(DIST == 0 & pertrips > 0) %>%
#   ungroup() %>%
#   summarize(total = sum(weight))


# Different method - calculate total number of daily travelers who take at
# least one trip
# daily_travelers_tt_test <-
#   tt_ppl %>%
#   mutate(
#
#     age5 = case_when(
#       (AGE >= 5 & AGE < 99) ~ 1,
#       AGEB == 2 & AGE == 99 ~ 1,
#       AGE == 99 & SCHOL %in% c(4,5,6,7,8) ~ 1,
#       TRUE ~ 0),
#
#     notravel = case_when(
#       SURVEY == 1 & PTRIPS1 == 0 ~ 1,
#       SURVEY == 2 & PTRIPS1 == 0 & PTRIPS2 == 0 ~ 1,
#       TRUE ~ 0
#
#       )) %>%
#   filter(age5 == 1,
#          notravel == 0,
#          MPO_per == 1)
#
# daily_travelers_tt_test %>%
#   summarize(sum = sum(WGTP))


# Identify travelers in one set but not the other
# missing <-
#   daily_travelers_tt_test %>%
#   anti_join(daily_travelers_tt, by = c("SAMPN","PERNO"))
#
# sum(missing$WGTP)
#
# missing %>%
#   left_join(tt_place, by = c("SAMPN","PERNO")) %>%
#   View()

#################################################
#                                               #
#            Average resident behavior          #
#                                               #
#################################################

################################################################################
#
# My Daily Travel
################################################################################

# NOTE: The calculations for MDT use "network distance." If a comparison with TT
# is desired, you should instead use the haversine distance, which will require
# changing the calls to distance variables to use the hdist variants and
# modifying the exclusion of nonzero distance trips above.

# Calculate summary statistics
avgtravel_mdt %>%
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(daily_travelers_mdt,
            by = character()) %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  View()


# Total distance
sum(avgtravel_mdt$dist_pg_weight)

# Mileage per traveler
sum(avgtravel_mdt$dist_pg_weight) / daily_travelers_mdt

# Average trip length
sum(avgtravel_mdt$dist_pg_weight) / sum(avgtravel_mdt$wtperfin)

# Trips per person
sum(avgtravel_mdt$wtperfin) / daily_travelers_mdt

################################################################################
#
# Travel Tracker
################################################################################
# 
# # NOTE: As coded in this file, these statistics are not comparable with MDT
# # because they use haversine distances. If desired, the MDT calculations can be
# # modified to use haversine distances (see note above). However, given the
# # differences in survey methodology, direct comparison is difficult.
# 
# # Calculate summary statistics
# avgtravel_tt %>%
#   summarize(
#     total_distance = sum(dist_weight),
#     total_trips = sum(weight),
#     avg_trip_length = total_distance / total_trips,
#   ) %>%
#   cbind(daily_travelers_tt) %>%
#   mutate(distance_per_capita = total_distance / total_travelers,
#          trips_per_capita = total_trips / total_travelers) %>%
#   View()
# 
# 
# # Total distance
# sum(avgtravel_tt$dist_weight)
# 
# # Mileage per traveler
# sum(avgtravel_tt$dist_weight) / daily_travelers_tt
# 
# # Average trip length
# sum(avgtravel_tt$dist_weight) / sum(avgtravel_tt$weight)
# 
# # Trips per person
# sum(avgtravel_tt$weight) / daily_travelers_tt
