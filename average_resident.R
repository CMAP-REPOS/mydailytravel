library(tidyverse)
library(cmapplot)
library(forcats)




#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

#################################################
#                                               #
#            Average resident behavior          #
#                                               #
#################################################


### MY DAILY TRAVEL - closely aligns with Sarah's work, seem to be right

# filter out trips we don't want to evaluate
avgtravel_mdt <-
  mdt %>%
  filter(
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place
    age >= 5 |
      age <0 & aage %in% c(2,3,4,5,6,7) |
      age < 0 & schol %in% c(4,5,6,7,8) |
      age <0 & sampno %in% c(70038312, 70051607)) %>%
  # Keep only people who traveled
  filter(pertrips > 0) %>%
  # Eliminate 0 distance trips
  filter(hdist_pg > 0) %>%
  # Add calculation for weighted distance
  mutate(hdist_pg_weight = hdist_pg * wtperfin)

# Calculate total number of daily travelers who take at least one trip
daily_travelers_mdt <-
  avgtravel_mdt %>%
  select(sampno,perno,wtperfin) %>%
  distinct() %>%
  summarize(total_travelers = sum(wtperfin))

# Calculate summary statistics
avgtravel_mdt %>%
  summarize(
    total_distance = sum(hdist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(daily_travelers_mdt,
            by = character()) %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  View()


# Total distance
sum(avgtravel_mdt$hdist_pg_weight)

# Mileage per traveler
sum(avgtravel_mdt$hdist_pg_weight) / daily_travelers_mdt

# Average trip length
sum(avgtravel_mdt$hdist_pg_weight) / sum(avgtravel_mdt$wtperfin)

# Trips per person
sum(avgtravel_mdt$wtperfin) / daily_travelers_mdt




##### TRAVEL TRACKER - note that due to differences in collection of distances
##### for out-region trips, these numbers are likely not comparable with MDT.

avgtravel_tt <-
  tt %>%
  # Keep only trips by travelers at least 5 years old. Note that 99 is DK/RF for AGE.
  filter((AGE >= 5 & AGE < 99) |
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGEB == 2 & AGE == 99)) %>%
  # Include only travelers who made at least one trip
  filter(pertrips > 0) %>%
  # Exclude zero distance trips (note that TT did not capture distances for
  # trips outside the region, so this means that travelers who only traveled to
  # or from the region, but not within the region, will be excluded).
  filter(DIST > 0) %>%
  # Calculate weighted distances
  mutate(dist_weight = DIST * weight)

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

# Calculate total number of daily travelers who take at least one trip
daily_travelers_tt <-
  avgtravel_tt %>%
  select(SAMPN,PERNO,DAYNO,weight) %>%
  distinct()

daily_travelers_tt %>%
  summarize(total_travelers = sum(weight))

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

# Calculate summary statistics by county
avgtravel_tt %>%
  summarize(
    total_distance = sum(dist_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  cbind(daily_travelers_tt %>% summarize(total_travelers = sum(weight))) %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  View()


# Total distance
sum(avgtravel_tt$dist_weight)

# Mileage per traveler
sum(avgtravel_tt$dist_weight) / sum(daily_travelers_tt$weight)

# Average trip length
sum(avgtravel_tt$dist_weight) / sum(avgtravel_tt$weight)

# Trips per person
sum(avgtravel_tt$weight) / daily_travelers_tt



tt %>%
  filter(AGE >= 5,
         DIST > 0) %>%
  summarize(total_distance = sum(DIST * weight, na.rm = TRUE),
            avg_distance = weighted.mean(DIST,weight, na.rm = TRUE))

avgtravel_tt %>%
  select(SAMPN,PERNO,DAYNO,pertrips,weight) %>%
  distinct() %>%
  summarize(total_trips = weighted.mean(pertrips,weight))

