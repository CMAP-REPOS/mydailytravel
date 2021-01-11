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
avgtravel_mdt <- mdt %>%
  filter(
    # Keep only trips in the region
    out_region == 0,
    # Keep only trips under 100 miles
    distance_pg < 100,
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place
    age >= 5 | aage %in% c(2,3,4,5,6,7) |
      schol %in% c(4,5,6,7,8) | sampno %in% c(70038312, 70051607),
    # Keep only people who traveled
    pertrips > 0) %>%
  mutate(hdist_pg_weight = hdist_pg * wtperfin)

# Calculate total number of daily travelers who take at least one trip
daily_travelers_mdt <-
  avgtravel_mdt %>%
  select(sampno,perno,wtperfin,home_county) %>%
  distinct() %>%
  group_by(home_county) %>%
  summarize(total_travelers = sum(wtperfin))

daily_travelers_mdt <-
  daily_travelers_mdt %>%
  rbind(.,
        tibble(home_county = 1000,
               total_travelers = sum(daily_travelers_mdt$total_travelers)))

# Calculate summary statistics by county
avgtravel_mdt %>%
  rbind(.,
        avgtravel_mdt %>% mutate(home_county = 1000)) %>%
  group_by(home_county) %>%
  summarize(
    total_distance = sum(hdist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(.,
            daily_travelers_mdt,
            by = "home_county") %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  filter(home_county %in% c(cmap_counties,1000)) %>%
  View()


# Total distance
sum(avgtravel_mdt$hdist_pg_weight)

# Mileage per traveler
sum(avgtravel_mdt$hdist_pg_weight) / daily_travelers_mdt

# Average trip length
sum(avgtravel_mdt$hdist_pg_weight) / sum(avgtravel_mdt$wtperfin)

# Trips per person
sum(avgtravel_mdt$wtperfin) / daily_travelers_mdt




##### TRAVEL TRACKER - don't trust these numbers right now

avgtravel_tt <-
  tt %>%
  filter(
    # Keep only trips by travelers at least 5 years old
    AGE >= 5,
    # Keep only trips less than 100 miles
    DIST > 0 & DIST < 100,
    # Include only travelers who made at least one trip
    pertrips > 0
  ) %>%
  mutate(dist_weight = DIST * weight)


# calculate total number of daily travelers who take at least one trip
daily_travelers_tt_test <-
  tt_ppl %>%
  mutate(

    age5 = case_when(
      (AGE >= 5 & AGE < 99) ~ 1,
      AGEB == 2 ~ 1,
      TRUE ~ 0),

    notravel = case_when(
      SURVEY == 1 & PTRIPS1 == 0 ~ 1,
      SURVEY == 2 & PTRIPS1 == 0 & PTRIPS2 == 0 ~ 1,
      TRUE ~ 0

      )) %>%
  filter(age5 == 1,
         notravel == 0,
         MPO == 1)

daily_travelers_tt_test %>%
  summarize(sum = sum(WGTP))


# Calculate total number of daily travelers who take at least one trip
daily_travelers_tt <-
  avgtravel_tt %>%
  select(SAMPN,PERNO,DAYNO,weight,home_county) %>%
  distinct() %>% View()
  group_by(home_county) %>%
  summarize(total_travelers = sum(weight))

daily_travelers_tt <-
  daily_travelers_tt %>%
  rbind(.,
        tibble(home_county = 1000,
               total_travelers = sum(daily_travelers_tt$total_travelers)))

# Calculate summary statistics by county
avgtravel_tt %>%
  rbind(.,
        avgtravel_tt %>% mutate(home_county = 1000)) %>%
  group_by(home_county) %>%
  summarize(
    total_distance = sum(dist_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(.,
            daily_travelers_tt,
            by = "home_county") %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  filter(home_county %in% c(cmap_counties,1000)) %>%
  View()


# Total distance
sum(avgtravel_tt$dist_weight)

# Mileage per traveler
sum(avgtravel_tt$dist_weight) / daily_travelers_tt

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
