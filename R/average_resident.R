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

# Age bins
age_breaks <- c(-1,17,29, 49, 69, 150)
age_labels <- c("5 to 17","18 to 29", "30 to 49",  "50 to 69", "70 and above")

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
         dist_pg_weight = distance_pg * wtperfin) %>% 
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels))
  

# Identify distinct list of travelers that took at least one trip to enable
# summing by different demographic characteristics
distinct_daily_travelers_mdt <-
  avgtravel_mdt %>%
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)) %>% 
  select(sampno,perno,wtperfin,race_eth,sex,income_c,home_county_chi,age_bin) %>%
  distinct() 

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

# Calculate summary statistics (code reused below for variations by demography)
travel_overall <-
  # Get data
  avgtravel_mdt %>%
  # Calculate summaries of distance, trips, and trip lengths
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  # Add total number of travelers
  left_join(distinct_daily_travelers_mdt %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = character()) %>%
  # Calculate distance and trips per capita using total travelers
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  # Add variables for combining with other calculations
  mutate(type = "Overall",
         subtype = "Overall")

# Calculate summary statistics by gender (reusing overall code)
travel_gender <-
  avgtravel_mdt %>%
  group_by(sex) %>% 
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(distinct_daily_travelers_mdt %>% 
              group_by(sex) %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = "sex") %>%
  group_by(sex) %>% 
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  ungroup() %>% 
  mutate(type = "Gender") %>% 
  # Remove individuals without a response
  filter(sex > 0) %>% 
  # Recode for ease of understanding
  mutate(subtype = recode(sex,
                          "1" = "Male",
                          "2" = "Female")) %>% 
  select(-sex)

# Calculate summary statistics by race and ethnicity (reusing overall code)
travel_race_eth <- 
  avgtravel_mdt %>%
  group_by(race_eth) %>% 
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(distinct_daily_travelers_mdt %>% 
              group_by(race_eth) %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = "race_eth") %>%
  group_by(race_eth) %>% 
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  ungroup() %>% 
  # Remove missing 
  filter(race_eth != "missing") %>% 
  # Recode for ease of understanding
  mutate(type = "Race and ethnicity",
         subtype = recode_factor(race_eth,
                                 "white" = "White",
                                 "hispanic" = "Hispanic",
                                 "black" = "Black",
                                 "asian" = "Asian",
                                 "other" = "Other")) %>%
  select(-race_eth)


# Calculate summary statistics by income (reusing overall code)
travel_income <- 
  avgtravel_mdt %>%
  group_by(income_c) %>% 
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(distinct_daily_travelers_mdt %>% 
              group_by(income_c) %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = "income_c") %>%
  group_by(income_c) %>% 
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  ungroup() %>% 
  # Remove individuals without a response
  filter(income_c != "missing") %>% 
  # Recode for ease of understanding
  mutate(type = "Household income",
         subtype = recode_factor(income_c,
                                 "low" = "Less than $35K",
                                 "middle-low" = "$35K to $59K",
                                 "middle-high" = "$60K to $99K",
                                 "high" = "$100K or more")) %>%
  select(-income_c)

# Calculate summary statistics by age (reusing overall code)
travel_age <-
  avgtravel_mdt %>%
  group_by(age_bin) %>% 
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(distinct_daily_travelers_mdt %>% 
              group_by(age_bin) %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = "age_bin") %>%
  group_by(age_bin) %>% 
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  ungroup() %>% 
  # Remove individuals without a response
  filter(!is.na(age_bin)) %>% 
  mutate(type = "Age") %>% 
  rename(subtype = age_bin)

# Calculate summary statistics by home jurisdiction (reusing overall code)
travel_home <-
  avgtravel_mdt %>%
  group_by(home_county_chi) %>% 
  summarize(
    total_distance = sum(dist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(distinct_daily_travelers_mdt %>% 
              group_by(home_county_chi) %>% 
              summarize(total_travelers = sum(wtperfin)),
            by = "home_county_chi") %>%
  group_by(home_county_chi) %>% 
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>% 
  ungroup() %>% 
  # Remove individuals in DeKalb, Grundy, or those with 2+ homes
  filter(!(home_county_chi %in% c("DeKalb","Grundy","Homes in multiple counties"))) %>% 
  mutate(type = "Home jurisdiction") %>% 
  rename(subtype = home_county_chi)



# Combine different travel statistic calculations
travel_summaries <-
  rbind(travel_overall,
        travel_gender,
        travel_age,
        travel_income,
        travel_race_eth,
        travel_home) %>% 
  select(type,subtype,trips_per_capita,avg_trip_length,distance_per_capita) %>% 
  mutate(across(c(trips_per_capita,avg_trip_length,distance_per_capita),~round(.,2))) %>% 
  # Add levels
  mutate(type = factor(type,
                       levels = c("Gender","Race and ethnicity","Household income","Age","Overall","Home jurisdiction"))) %>% 
  mutate(subtype = factor(subtype,
                          levels = c("Overall",
                                     "Male","Female",
                                     "White","Asian","Black","Hispanic","Other",
                                     "5 to 17","18 to 29","30 to 49","50 to 69","70 and above",
                                     "Less than $35K","$35K to $59K","$60K to $99K","$100K or more"
                                     )))

# Plot of trips and distances by demographic characteristics
average_resident_p1 <-
  # Get data
  travel_summaries %>%
  # Pivot longer
  pivot_longer(cols = c(trips_per_capita,avg_trip_length,distance_per_capita)) %>% 
  # Exclude total distances
  filter(name != "distance_per_capita") %>% 
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>% 
  # Rename variables we are keeping
  mutate(name = recode_factor(factor(name,levels = c("trips_per_capita","avg_trip_length")),
                       "trips_per_capita" = "Trips per day",
                       "avg_trip_length" = "Distance per trip (miles)")) %>% 
  # Exclude overall and geography
  filter(!(type %in% c("Overall","Home jurisdiction"))) %>%
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype, fill = name)) +
  geom_col(position = position_dodge2(width = 1,padding = 0.15,reverse = T),
           width = .8) +
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = name),
             position = position_dodge2(width = .9,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             hjust = 0) +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,8)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             # xlab = "Travel patterns by demographic characteristics",
             strip.text = element_text(hjust = 0.5)) +
  cmap_fill_discrete(palette = "legislation") +
  
  # Add faceting
  facet_wrap(~type,ncol = 2,scales = "free_y")

finalize_plot(average_resident_p1,
              sidebar_width = 0,
              "Average travel patterns vary significantly based on demographic
              characteristics.",
              caption = "Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "average_resident_p1",
              mode = "png",
              overwrite = T)
  


### Individual statistics

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
