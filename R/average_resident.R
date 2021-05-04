# This script analyzes average travel behavior in MDT and TT

#################################################
#                                               #
#                 Library loading               #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(forcats)
library(ggpattern)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


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
      (age < 0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8)) |
      sampno %in% c(70038312, 70051607)) %>%
  # Filter out "beginning" trips
  filter(mode != "beginning") %>% # 97374 records
  # Keep only people who traveled
  filter(pertrips > 0) %>%   # 97373 records
  # Keep only trips < 15 hours
  filter(travtime_pg_calc < 15*60) %>%  # 97364 records
  # Keep only trips > 0 minutes
  filter(travtime_pg_calc > 0) %>% # 97330 records
  
  ###### RUN ONE OF THE TWO BELOW (MDT-only vs. MDT + TT comparison)
  
  # ### MDT ONLY
  # # Eliminate 0 distance trips (network - use for MDT-specific analyses)
  # filter(distance_pg > 0) %>%   # 97273 records

  ### MDT VS TT
  # Eliminate 0 distance trips (haversine - only use for comparisons with TT)
  filter(hdist_pg > 0) %>%   # 96343 records
  # Filter out trips that were not within the TT travel region (only for
  # comparisons with TT - see explanation in TT data prep below)
  filter(out_tt_trip == 0) %>% # 94531 records
  # Keep only residents of the seven counties, Grundy (63) and double-home
  # residents in the seven county region (999) for comparability with TT. This
  # excludes residents of DeKalb.
  filter(home_county %in% c(cmap_seven_counties,63,999)) %>% # 94431 records
  
  
  # Add calculation for weighted distance and time
  mutate(hdist_pg_weight = hdist_pg * wtperfin,
         dist_pg_weight = distance_pg * wtperfin,
         time_weight = travtime_pg_calc * wtperfin) %>% 
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)) %>% 
  # Keep only variables of interest
  select(
    # First, select which distance will be used
    chosen_distance = hdist_pg_weight,
    # Keep time weight
    time_weight,
    # Keep others and rename weight for merging with TT
    sampno,perno,age_bin,sex,income_c,race_eth,home_county_chi,weight = wtperfin) %>% 
  mutate(survey = "mdt")

# Create a similarly filtered list of all respondents (With age filters but not travel)
avgtravel_all_respondents_mdt <-
  mdt_all_respondents %>%  # 30683
  filter(
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place. age < 0 are all respondents without a numeric age value.
    age >= 5 |               # 125459 records
      (age < 0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8)) |
      sampno %in% c(70038312, 70051607)) # 28573

# Identify distinct list of travelers that took at least one trip to enable
# summing by different demographic characteristics
distinct_daily_travelers_mdt <-
  avgtravel_mdt %>% # 94431
  select(sampno,perno,weight,race_eth,sex,income_c,home_county_chi,age_bin,survey) %>%
  distinct() # 24311

# Identify individuals who did travel in the survey, but are excluded based on 
# the filtering criteria (e.g., they did not travel in the CMAP area)
ineligible_travelers_mdt <-
  # Take the full list of respondents
  avgtravel_all_respondents_mdt %>% # 28573
  # Remove all respondents who are included in the list of distinct daily travelers
  anti_join(distinct_daily_travelers_mdt, by = c("sampno","perno")) %>% # 4262
  # Identify those of the remainder that did travel, and are thus excluded from
  # the list of distinct travelers based on some other condition
  filter(pertrips > 0) %>% # 696
  # add age bins and survey ID
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels),
         survey = "mdt") %>% 
  # Keep relevant variables and rename weight for merging with TT
  select(sampno,perno,weight = wtperfin,race_eth,sex,income_c,
         home_county_chi,age_bin,survey)

# Add back the ineligible travelers for the purpose of travel percent calculation
total_traveler_universe_mdt <-
  rbind(ineligible_travelers_mdt,
        distinct_daily_travelers_mdt)

# Identify list of residents (includes ineligible travelers)
distinct_residents_mdt <-
  avgtravel_all_respondents_mdt %>% # 28573
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)) %>%
  mutate(survey = "mdt") %>%
  # Keep relevant variables and rename weights for merging with TT
  select(sampno,perno,weight = wtperfin,race_eth,sex,income_c,
         home_county_chi,age_bin,survey)
  

avgtravel_tt <-
  tt %>%                    # 139769 records
  # Keep only trips by travelers at least 5 years old. Note that 99 is DK/RF for
  # AGE. We also keep travelers with unknown age based on school enrollment or
  # AGEB of 2, which indicates 16+
  filter((AGE >= 5 & AGE < 99) |
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGEB == 2 & AGE == 99)) %>% # 132680 records
  # Filter out the first record for each traveler (PLANO == 1)
  filter(PLANO != 1) %>%    # 105568 records
  # Include only travelers who made at least one trip
  filter(pertrips > 0) %>%  # 105568 records
  # Exclude zero distance trips (note that TT did not capture distances for
  # trips outside the seven counties, Illinois' Grundy County, and Lake,
  # LaPorte, and Porter Counties in Indiana, so this means that travelers who
  # only traveled to or from those counties, but not within them, will be
  # excluded, in addition to any travelers who only traveled outside the
  # region).
  filter(DIST > 0) %>%      # 100573 records
  # Exclude trips with 0 travel time or more than 15 hours
  filter(TRPDUR > 0 & TRPDUR < 15 * 60) %>%  # 100573 records
  # Calculate weighted distances and times
  mutate(dist_weight = DIST * weight,
         time_weight = TRPDUR * weight) %>% 
  # Add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks,labels=age_labels)) %>% 
  # Keep only variables of interest and rename to match MDT variables
  select(chosen_distance = dist_weight,
         time_weight,
         sampno = SAMPN,
         perno = PERNO,
         sex = GEND,
         age_bin,income_c,race_eth,home_county_chi,weight,
         DAYNO) %>% 
  mutate(survey = "tt")

# Create a similar list of all TT respondents with age filters
avgtravel_all_respondents_tt <-
  tt_all_respondents %>%   # 23808 records
  # Keep only trips by travelers at least 5 years old. Note that 99 is DK/RF for
  # AGE. We also keep travelers with unknown age based on school enrollment or
  # AGEB of 2, which indicates 16+
  filter((AGE >= 5 & AGE < 99) |
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGEB == 2 & AGE == 99)) # 22378 records

# Identify distinct list of travelers that took at least one trip to enable
# summing by different demographic characteristics. This includes the DAYNO
# variable to capture respondents who had a two-day weekday survey.
distinct_daily_travelers_tt <-
  avgtravel_tt %>% # 100573
  select(sampno,perno,weight,race_eth,sex,income_c,
         home_county_chi,age_bin,survey,DAYNO) %>%
  distinct() %>% # 24065
  select(-DAYNO)

# Identify individuals who did travel in the survey, but are excluded based on 
# the travel-based filtering criteria (e.g., they did not travel in the CMAP area)
ineligible_travelers_tt <-
  avgtravel_all_respondents_tt %>% 
  anti_join(distinct_daily_travelers_tt,
            by = c("SAMPN" = "sampno","PERNO" = "perno")) %>%
  # Keep travelers who traveled on a non-weekend. This means that they either
  # had trips on their first day for all non-Sunday surveys, or trips on their
  # second day for all non-Friday surveys.
  filter(((PTRIPS1 > 0 & DAY != 7) | (PTRIPS2 > 0 & DAY != 5))) %>% 
  # Add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks,labels=age_labels),
         survey = "tt") %>% 
  rename(sampno = SAMPN,
         perno = PERNO) %>% 
  select(sampno,perno,weight = WGTP,race_eth,sex = GEND,income_c,
         home_county_chi,age_bin,survey)

# Add back the ineligible travelers for the purpose of travel percent calculation
total_traveler_universe_tt <-
  rbind(ineligible_travelers_tt,
        distinct_daily_travelers_tt)

# ALL RESIDENTS
# Find the number of residents (including ineligible travelers)
distinct_residents_tt <-
  avgtravel_all_respondents_tt %>% 
  # Add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks,labels=age_labels)) %>%
  mutate(survey = "tt") %>%
  select(sampno = SAMPN,perno = PERNO,weight = WGTP,
         race_eth,sex = GEND,income_c,home_county_chi,age_bin,survey)

# Combine TT and MDT data
avgtravel <-
  rbind(avgtravel_mdt,
        avgtravel_tt %>% select(-DAYNO))

# Number of travelers for travel statistics (distance, time, number)
distinct_daily_travelers <-
  rbind(distinct_daily_travelers_mdt,
        distinct_daily_travelers_tt)

# Number of travelers for whether individuals are traveling
total_travel_universe <-
  rbind(total_traveler_universe_mdt,
        total_traveler_universe_tt)

distinct_residents <-
  rbind(distinct_residents_mdt,
        distinct_residents_tt)

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

################################################################################
# Summary statistics
################################################################################

# Calculate summary statistics (code reused below for variations by demography)
travel_overall <-
  # Get data
  avgtravel %>%
  # Group by survey
  group_by(survey) %>% 
  # Calculate summaries of distance, trips, and trip lengths
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("survey")) %>% 
  # Calculate distance and trips per capita using total travelers
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
  # Add variables for combining with other calculations
  mutate(type = "Overall",
         subtype = "Overall")


## Export details
### Trips
travel_overall %>% select(survey,total_trips)
### Miles
travel_overall %>% select(survey,total_distance)
### Time (in hours)
travel_overall %>% select(survey,total_time) %>% mutate(total_time = total_time / 60)
### Distance per traveler
travel_overall %>% select(survey,distance_per_capita)
### Trips per traveler
travel_overall %>% select(survey,trips_per_capita)
### Time per trip
travel_overall %>% select(survey,avg_trip_time)
### Distance per trip
travel_overall %>% select(survey,avg_trip_length)


# Calculate summary statistics by gender (reusing overall code)
travel_sex <-
  avgtravel %>%
  group_by(survey,sex) %>% 
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(sex,survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("sex","survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(sex,survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("sex","survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(sex,survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("sex","survey")) %>% 
  # Calculate figures
  group_by(sex,survey) %>% 
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
  ungroup() %>% 
  mutate(type = "Sex") %>% 
  # Remove individuals without a response
  filter(sex %in% c(1,2)) %>% 
  # Recode for ease of understanding
  mutate(subtype = recode(sex,
                          "1" = "Male",
                          "2" = "Female")) %>% 
  select(-sex)

# Calculate summary statistics by income (reusing overall code)
travel_income <- 
  avgtravel %>%
  group_by(survey,income_c) %>% 
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(income_c,survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("income_c","survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(income_c,survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("income_c","survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(income_c,survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("income_c","survey")) %>% 
  # Calculate figures
  group_by(income_c,survey) %>% 
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
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
  avgtravel %>%
  group_by(survey,age_bin) %>% 
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(age_bin,survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("age_bin","survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(age_bin,survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("age_bin","survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(age_bin,survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("age_bin","survey")) %>% 
  # Calculate figures
  group_by(age_bin,survey) %>% 
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
  ungroup() %>% 
  # Remove individuals without a response
  filter(!is.na(age_bin)) %>% 
  mutate(type = "Age") %>% 
  rename(subtype = age_bin)

# Calculate summary statistics by home jurisdiction (reusing overall code)
travel_home <-
  avgtravel %>%
  group_by(survey,home_county_chi) %>% 
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(home_county_chi,survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("home_county_chi","survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(home_county_chi,survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("home_county_chi","survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(home_county_chi,survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("home_county_chi","survey")) %>% 
  # Calculate figures
  group_by(home_county_chi,survey) %>% 
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
  ungroup() %>% 
  # Keep the nine counties but remove those that span multiple counties
  filter(home_county_chi %in% c("Cook","DeKalb","DuPage","Grundy","Kane",
                                "Kendall","Lake","McHenry","Will")) %>% 
  mutate(type = "Home jurisdiction") %>% 
  rename(subtype = home_county_chi)

# Calculate summary statistics by race and ethnicity (reusing overall code) -
# this analysis only uses MDT since TT did not have race/ethnicity for all
# household members
travel_race_eth <- 
  avgtravel_mdt %>%
  group_by(race_eth,survey) %>% 
  summarize(
    total_distance = sum(chosen_distance),
    total_time = sum(time_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
    avg_trip_time = total_time / total_trips,
    n = n()
  ) %>%
  # Add total number of travelers (only eligible travelers)
  left_join(distinct_daily_travelers %>%
              group_by(race_eth,survey) %>% 
              summarize(total_eligible_travelers = sum(weight)),
            by = c("race_eth","survey")) %>%  
  # Add total number of travelers (all travelers)
  left_join(total_travel_universe %>%
              group_by(race_eth,survey) %>% 
              summarize(total_travelers = sum(weight)),
            by = c("race_eth","survey")) %>%
  # Add total number of residents
  left_join(distinct_residents %>% 
              group_by(race_eth,survey) %>% 
              summarize(total_residents = sum(weight)),
            by = c("race_eth","survey")) %>% 
  # Calculate figures
  group_by(race_eth,survey) %>% 
  mutate(distance_per_capita = total_distance / total_eligible_travelers,
         trips_per_capita = total_trips / total_eligible_travelers,
         time_per_capita = total_time / total_eligible_travelers,
         traveling_pct = total_travelers / total_residents) %>% 
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

################################################################################
# Plot summary statistics
################################################################################

# Combine different travel statistic calculations
travel_summaries <-
  rbind(travel_overall,
        travel_sex,
        travel_age,
        travel_income,
        travel_race_eth) %>% 
  # Keep relevant variables
  select(type,subtype,trips_per_capita,avg_trip_length,avg_trip_time,
         distance_per_capita,time_per_capita,traveling_pct,n,survey) %>% 
  # Round for ease of review
  mutate(across(c(trips_per_capita,avg_trip_length,avg_trip_time,
                  distance_per_capita,time_per_capita,traveling_pct),~round(.,4))) %>% 
  # Add levels
  mutate(type = factor(type,
                       levels = c("Sex","Race and ethnicity","Household income","Age","Overall","Home jurisdiction"))) %>% 
  mutate(subtype = factor(subtype,
                          levels = c("Overall",
                                     "Male","Female",
                                     "White","Asian","Black","Hispanic","Other",
                                     "5 to 17","18 to 29","30 to 49","50 to 69","70 and above",
                                     "Less than $35K","$35K to $59K","$60K to $99K","$100K or more"
                                     ))) %>% 
  # Pivot longer
  pivot_longer(cols = c(trips_per_capita,avg_trip_length,avg_trip_time,
                        distance_per_capita,time_per_capita,traveling_pct,n))

# Extract values for regional averages, which will be graphed as value lines
travel_summaries_vlines <-
  travel_summaries %>%
  group_by(survey) %>% 
  filter(type == "Overall" & name %in% c("trips_per_capita",
                                         "avg_trip_length",
                                         "avg_trip_time",
                                         "traveling_pct")) %>% 
  select(-subtype,-type) %>% 
  left_join(tibble(type = c("Sex","Race and ethnicity","Household income","Age")),
            by = character()) %>% 
  mutate(name = recode_factor(factor(name,levels = c("trips_per_capita",
                                                     "avg_trip_length",
                                                     "avg_trip_time",
                                                     "traveling_pct")),
                              "trips_per_capita" = "Trips/day",
                              "avg_trip_length" = "Distance/trip (mi.)",
                              "avg_trip_time" = "Time/trip (min.)",
                              "traveling_pct" = "Percent traveling"))

################################################################################
# Plot of trips and distances by demographic characteristics for MDT
################################################################################

# Plot
average_resident_p1 <-
  # Get data
  travel_summaries %>%
  # Keep MDT
  filter(survey == "mdt") %>% 
  # Exclude total distances
  filter(name %in% c("trips_per_capita",
                     "avg_trip_length",
                     "avg_trip_time")) %>% 
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype))),
         type = factor(type,levels = rev(levels(type)))) %>% 
  # Add blank for label positioning
  mutate(blank = case_when(
    name == "trips_per_capita" ~ 5,
    name == "avg_trip_length" ~ 6,
    name == "avg_trip_time" ~ 31,
  )) %>% 
  # Rename variables we are keeping
  mutate(name = recode_factor(factor(name,levels = c("trips_per_capita",
                                                     "avg_trip_length",
                                                     "avg_trip_time")),
                       "trips_per_capita" = "Trips/day",
                       "avg_trip_length" = "Distance/trip (mi.)",
                       "avg_trip_time" = "Time/trip (min.)")) %>% 
  # Exclude overall and geography
  filter(!(type %in% c("Overall","Home jurisdiction"))) %>%
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype, fill = type)) +
  # Add columns
  geom_col(width = .8) +
  
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = travel_summaries_vlines %>% 
               filter(survey == "mdt") %>%
               filter(name != "Percent traveling") %>% 
               mutate(color = "Regional average"),
             mapping = aes(xintercept = value,
                           color = color),
             linetype = "dashed",
             size = .65
  ) +
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = name),
             position = position_dodge2(width = .9,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             hjust = 0) +
  
  # Add geom_blank for positioning
  geom_blank(aes(x = blank)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             # xlab = "Travel patterns by demographic characteristics",
             strip.text.x = element_text(hjust = 0.5,vjust = 1),
             strip.text.y = element_blank()) +
  cmap_fill_discrete(palette = "legislation") +
  scale_color_discrete(type = "black") +
  # Reorder legends
  guides(color = guide_legend(order = 2),fill = guide_legend(order = 1)) +
  
  # Add faceting
  facet_grid(type~name,
             # ncol = 3,
             scales = "free",
             )

# Export finalized graphic
finalize_plot(average_resident_p1,
              sidebar_width = 0,
              "Average travel patterns vary significantly based on demographic
              characteristics.",
              caption = 
              paste0("Note: Figures are calculated based only on individuals 
              who traveled and thus exclude individuals with zero trips. 
              Non-travelers are disproportionately low-income, non-white, and 
              aged between 18 and 29 or older than 70.
              <br><br>
              Includes trips by travelers aged 5 and older who live in the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy. For comparability with 
              Travel Tracker survey results, only includes trips within 
              that region and between that region and the Indiana counties of 
              Lake, LaPorte, and Porter. 
              Distances are calculated as point-to-point ('haversine') and do 
              not account for additional distance traveled along the route. 
              'Hispanic' includes respondents who identified as Hispanic of any 
              racial category. Other categories are non-Hispanic. For the 
              categorization by sex, the survey asked respondents whether they
              were male or female. A small number of respondents chose not to 
              answer and are excluded based on small sample sizes.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_daily_travelers_mdt),big.mark = ","),
                     " travelers. 
              Across all categories, travelers with an 'Other' race and 
              ethnicity have the lowest sample size, with ",
                     format(
                       distinct_daily_travelers_mdt %>% 
                         filter(race_eth == "other") %>% 
                         count() %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                     " individual travelers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "average_resident_p1",
              mode = "png",
              height = 8,
              overwrite = T)
  


################################################################################
# Plot of percent traveling for MDT
################################################################################

# Plot
average_resident_p2 <-
  # Get data
  travel_summaries %>%
  # Keep MDT
  filter(survey == "mdt") %>% 
  # Exclude total distances
  filter(name %in% c("traveling_pct")) %>% 
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype))),
         type = factor(type,levels = rev(levels(type)))) %>% 
  # Exclude overall and geography
  filter(!(type %in% c("Overall"))) %>%
  # Mutate name to match
  mutate(name = "Percent traveling") %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype, fill = type)) +
  # Add columns
  geom_col(width = .8) +
  
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = travel_summaries_vlines %>% 
               filter(survey == "mdt") %>%
               filter(name == "Percent traveling") %>% 
               mutate(color = paste0("Regional average (",round(100*value[1],1),"%)")),
             mapping = aes(xintercept = value,
                           color = color),
             linetype = "dashed",
             size = .65
  ) +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(value),
                 group = name),
             position = position_dodge2(width = .9,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             hjust = 0) +
  
  # Adjust scale
  scale_x_continuous(limits = c(0,1.15),
                     labels = scales::label_percent(accuracy = 1),
                     breaks = c(0,.25,.5,.75,1)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Percent of residents who traveled",
             strip.text.x = element_blank(),
             strip.text.y = element_blank()) +
  cmap_fill_discrete(palette = "legislation") +
  scale_color_discrete(type = "black") +
  
  # Add faceting
  facet_wrap(~type,
             ncol = 2,
             scale = "free") +
  
  # Reorder legends
  guides(color = guide_legend(order = 2),fill = guide_legend(order = 1))

# Export finalized graphic
finalize_plot(average_resident_p2,
              sidebar_width = 0,
              "Low-income, older, and non-white residents were the least likely 
              to travel on a weekday.",
              caption = 
              paste0("Note: Figures are based on the travel behavior of 
              residents aged 5 or older of the CMAP seven county region (Cook, 
              DuPage, Kane, Kendall, Lake, McHenry, and Will), as well as Grundy 
              and DeKalb.
              Individuals were counted as 'traveling' if they had at least one 
              trip on their assigned travel day, no matter whether that trip was 
              in the CMAP region. 'Hispanic' includes respondents who identified 
              as Hispanic of any racial category. Other categories are 
              non-Hispanic. 
              For the categorization by sex, the survey asked respondents 
              whether they were male or female. A small number of respondents 
              chose not to answer and are excluded based on small sample sizes. 
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_residents_mdt),big.mark = ","),
                     " residents. 
              Across all categories, residents with an 'Other' race and 
              ethnicity have the lowest sample size, with ",
                     (distinct_residents_mdt %>% 
                        count(race_eth) %>% 
                        filter(race_eth == "other"))$n," individual residents.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "average_resident_p2",
              mode = "png",
              height = 6,
              overwrite = T)


# Identify sample sizes ("Other" race/eth has the lowest)

distinct_residents_mdt %>% count(age_bin)
distinct_residents_mdt %>% count(income_c)
distinct_residents_mdt %>% count(race_eth)
distinct_residents_mdt %>% count(sex)

################################################################################
# Plot of trips and distances for MDT vs. TT, comparing demographics
################################################################################

# Plot
average_resident_p3 <-
  # Get data
  travel_summaries %>%
  # Keep only total distances
  filter(name == "distance_per_capita") %>% 
  # Keep overall, sex, age, and income
  filter(type %in% c("Household income","Age","Sex")) %>% 
  # Identify entries where MDT has a higher value than TT
  mutate(helper = ifelse(survey == "mdt", value,-1*value)) %>% 
  group_by(type,subtype,name) %>% 
  mutate(increasing = sum(helper)) %>% 
  mutate(increasing = case_when(
    increasing > 0 ~ "Increased travel",
    TRUE ~ "Decreased travel"
  )) %>% 
  # Rename variables we are keeping
  mutate(survey = recode_factor(factor(survey),
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  # Fix ordering
  mutate(subtype = factor(subtype, levels = rev(levels(subtype)))) %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype,pattern = increasing)) +
  
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = survey),
                              color = "white",
                              pattern_fill = "white",
                              pattern_angle = 45,
                              pattern_density = 0.3,
                              pattern_spacing = 0.05,
                              pattern_key_scale_factor = 0.15,
                              position = position_dodge2(width = 1,
                                                         padding = 0.15,
                                                         reverse = T),
                              width = 0.8) +
  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("Increased travel" = "stripe",
                                  "Decreased travel" = "none")) +
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = survey),
             position = position_dodge2(width = .8,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             hjust = 0) +
  
  facet_wrap(~type,ncol = 3,scales = "free_y",dir = "v") +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,30)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Distance per day for residents who traveled (miles)",
             strip.text = element_text(hjust = 0.5,vjust = 1)) +
  cmap_fill_discrete(palette = "friday",reverse = T) +
  
  # Adjust legend for formatting
  guides(pattern = guide_legend(order = 2,override.aes = list(fill = "white", color = "black")),
         fill = guide_legend(order = 1,override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(average_resident_p3,
              "In contrast to the overall regional decline, lower-income and 
              older travelers reported increased travel in 2019 compared to 2008.",
              caption = 
              paste0(
              "Note: Includes trips by travelers aged 5 and older who live in the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy. Only includes trips within 
              that region and between that region and the Indiana counties of 
              Lake, LaPorte, and Porter. 
              Distances are calculated as point-to-point ('haversine') and do 
              not account for additional distance traveled along the route. For 
              the categorization by sex, the survey asked respondents whether 
              they were male or female. A small number of respondents chose not 
              to answer and are excluded based on small sample sizes.
              Finally, note that household incomes are not adjusted for 
              inflation, and so there may be some households from Travel Tracker 
              that should be compared to the next-highest household income 
              category (but cannot be due to available survey responses).
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_daily_travelers_mdt),big.mark = ","),
                     " travelers for My Daily Travel and ",
                     format(nrow(distinct_daily_travelers_tt),big.mark = ","),
                     " for Travel Tracker. Across all categories 
              and both surveys, travelers aged 70 and above in My Daily Travel 
              have the lowest sample size, with ",
                     format(
                       distinct_daily_travelers %>% 
                         filter(age_bin == "70 and above", survey == "mdt") %>% 
                         count() %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                     " individual travelers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel and Travel Tracker data."),
              filename = "average_resident_p3",
              sidebar_width = 0,
              mode = "png",
              height = 6.5,
              overwrite = T)

# Identify sample sizes
distinct_daily_travelers %>% count(survey,age_bin)
distinct_daily_travelers %>% count(survey,income_c)
distinct_daily_travelers %>% count(survey,sex)


################################################################################
# Plot of trips and distances for MDT vs. TT
################################################################################

# Plot
average_resident_p4 <-
  # Get data
  travel_summaries %>%
  # Exclude total distances
  filter(name %in% c("trips_per_capita","avg_trip_length")) %>% 
  # Keep only overall
  filter(type == "Overall") %>% 
  # Rename variables we are keeping
  mutate(name = recode_factor(factor(name,levels = c("trips_per_capita","avg_trip_length")),
                              "trips_per_capita" = "Trips per day        ",
                              "avg_trip_length" = "Distance per trip (miles)"),
         survey = recode_factor(factor(survey),
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = name, y = value, fill = survey)) +
  geom_col(position = position_dodge2(width = 1,padding = 0.15,reverse = F),
           width = .8) +
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = survey),
             position = position_dodge2(width = .8,reverse = F),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             vjust = 0) +
  
  # Adjust axes
  # scale_x_continuous(limits = c(0,8)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "h",hline = 0,
             xlab = "Travel characteristics over time",
             strip.text = element_text(hjust = 0.5,vjust = 1)) +
  cmap_fill_discrete(palette = "friday")

# Export finalized graphic
finalize_plot(average_resident_p4,
              "Travelers in the region in 2019 were taking slightly fewer, and 
              slightly shorter, trips than they were in 2008.",
              caption = 
              "Note: Includes trips by travelers aged 5 and older who live in the 
              Illinois counties of Cook, DuPage, Grundy, Kane, Kendall, Lake, 
              McHenry, and Will. Only includes trips within that region and 
              between that region and the Indiana counties of Lake, LaPorte, and 
              Porter. 
              Distances are calculated as point-to-point ('haversine') and do 
              not account for additional distance traveled along the route.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel and Travel Tracker data.",
              filename = "average_resident_p4",
              # mode = "png",
              # height = 6.5,
              overwrite = T)



################################################################################
#
# Understand non-travelers in MDT
################################################################################

# Create universe of all possible travelers, excluding the ones also excluded
# above (note this requires the "DAILY TRAVELERS" option in the code)

mdt_avg_excluded <-
  mdt %>% 
  filter(pertrips > 0) %>% 
  anti_join(distinct_daily_travelers_mdt %>% select(sampno,perno),
            by = c("sampno","perno")) %>% 
  distinct(sampno,perno)

mdt_all_respondents_excl <-
  mdt_all_respondents %>% 
  anti_join(mdt_avg_excluded, by = c("sampno","perno")) %>% 
  # filter by age
  filter(
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place. age < 0 are all respondents without a numeric age value.
    age >= 5 |               # 125459 records
      (age <0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8)) |
      sampno %in% c(70038312, 70051607)) %>%
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels))
  

# Generate variation from population totals for non-travelers (in percentage
# points)

# First, household income
mdt_notravel_income_c <-
  mdt_all_respondents_excl %>% 
  # Calculate baseline totals
  group_by(income_c) %>% 
  mutate(totalpop = sum(weight)) %>% 
  ungroup() %>% 
  # Identify travelers vs. non-travelers (keeping only those with an entry for
  # `pertrips`)
  filter(pertrips >= 0) %>% 
  mutate(traveled = ifelse(pertrips == 0,"No","Yes")) %>% 
  # Calculate totals by category, removing missing
  filter(income_c != "missing") %>% 
  group_by(income_c,traveled) %>% 
  summarize(pop = sum(weight),
            totalpop = median(totalpop)) %>% 
  # Now calculate proportions
  group_by(traveled) %>% 
  mutate(pct = pop/sum(pop),
         totalpoppct = totalpop/sum(totalpop),
         variation = pct - totalpoppct) %>% 
  # Keep only non-travelers
  filter(traveled == "No")


# Next, race and ethnicity
mdt_notravel_race_eth <-
  mdt_all_respondents_excl %>% 
  # Calculate baseline totals
  group_by(race_eth) %>% 
  mutate(totalpop = sum(weight)) %>% 
  ungroup() %>% 
  # Identify travelers vs. non-travelers (keeping only those with an entry for
  # `pertrips`)
  filter(pertrips >= 0) %>% 
  mutate(traveled = ifelse(pertrips == 0,"No","Yes")) %>% 
  # Calculate totals by category, removing missing
  filter(race_eth != "missing") %>% 
  group_by(race_eth,traveled) %>% 
  summarize(pop = sum(weight),
            totalpop = median(totalpop)) %>% 
  # Now calculate proportions
  group_by(traveled) %>% 
  mutate(pct = pop/sum(pop),
         totalpoppct = totalpop/sum(totalpop),
         variation = pct - totalpoppct) %>% 
  # Keep only non-travelers
  filter(traveled == "No")

# Next, sex
mdt_notravel_sex <-
  mdt_all_respondents_excl %>% 
  # Calculate baseline totals
  group_by(sex) %>% 
  mutate(totalpop = sum(weight)) %>% 
  ungroup() %>% 
  # Identify travelers vs. non-travelers (keeping only those with an entry for
  # `pertrips`)
  filter(pertrips >= 0) %>% 
  mutate(traveled = ifelse(pertrips == 0,"No","Yes")) %>% 
  # Calculate totals by category, removing missing
  filter(sex >0) %>% 
  group_by(sex,traveled) %>% 
  summarize(pop = sum(weight),
            totalpop = median(totalpop)) %>% 
  # Now calculate proportions
  group_by(traveled) %>% 
  mutate(pct = pop/sum(pop),
         totalpoppct = totalpop/sum(totalpop),
         variation = pct - totalpoppct) %>% 
  # Keep only non-travelers
  filter(traveled == "No")


# Finally, age
mdt_notravel_age <-
  mdt_all_respondents_excl %>% 
  # Calculate baseline totals
  group_by(age_bin) %>% 
  mutate(totalpop = sum(weight)) %>% 
  ungroup() %>% 
  # Identify travelers vs. non-travelers (keeping only those with an entry for
  # `pertrips`)
  filter(pertrips >= 0) %>% 
  mutate(traveled = ifelse(pertrips == 0,"No","Yes")) %>% 
  # Calculate totals by category, removing missing
  filter(!is.na(age_bin)) %>% 
  group_by(age_bin,traveled) %>% 
  summarize(pop = sum(weight),
            totalpop = median(totalpop)) %>% 
  # Now calculate proportions
  group_by(traveled) %>% 
  mutate(pct = pop/sum(pop),
         totalpoppct = totalpop/sum(totalpop),
         variation = pct - totalpoppct) %>% 
  # Keep only non-travelers
  filter(traveled == "No")


################################################################################
#
# Travel Tracker validity tests
################################################################################

# Sarah's original method - calculate total number of daily travelers who take
# at least one trip
daily_travelers_tt_baseline <-
  tt_ppl %>%
  left_join(tt_hh, by = c("SAMPN")) %>% 
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

# This method identifies 7,076,812 travelers in the region
daily_travelers_tt_baseline %>%
  summarize(sum = sum(WGTP))

# Note that the method used in the charts and analyses above identifies only
# 6,694,147 travelers. Below, we account for the difference, which is 382,665
# travelers.
distinct_daily_travelers_tt %>% 
  summarize(sum = sum(weight))

# First, we add additional travelers that are included in tt above
daily_travelers_tt_test <-
  tt_ppl %>%
  left_join(tt_hh, by = c("SAMPN")) %>% 
  mutate(
    
    age5 = case_when(
      (AGE >= 5 & AGE < 99) ~ 1,
      AGEB == 2 ~ 1,
      (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) ~ 1,
      TRUE ~ 0),
    
    notravel = case_when(
      SURVEY == 1 & PTRIPS1 == 0 ~ 1,
      SURVEY == 2 & PTRIPS1 == 0 & PTRIPS2 == 0 ~ 1,
      TRUE ~ 0
      
    )) %>%
  filter(age5 == 1,
         notravel == 0,
         MPO == 1)

# The new baseline is 7,080,209 travelers
daily_travelers_tt_test %>% 
  summarize(sum = sum(WGTP))

# How many of the travelers are excluded based on status as only weekend
# travelers?
tt_weekenders <-
  daily_travelers_tt_test %>%
  group_by(SAMPN,PERNO) %>%
  summarize(PTRIPS1 = first(PTRIPS1),
            PTRIPS2 = first(PTRIPS2),
            weight = first(WGTP),
            DAY = first(DAY),
            SURVEY = first(SURVEY)) %>%
  # Keep travelers who are recorded as having trips but have no travel on their
  # weekday travel day
  filter((SURVEY==2 & DAY==5 & PTRIPS1 == 0) | 
           (SURVEY==2 & DAY==7 & PTRIPS2 == 0)) %>%
  ungroup()

# This accounts for 104,371 travelers.
tt_weekenders %>% 
  summarize(total = sum(weight))

# How many of the travelers are excluded based on the out-of-region travel
# identified above (and are not weekenders)?
tt_out_of_region <-
  daily_travelers_tt_test %>%
  # Remove all travelers accounted for as only weekend travelers
  anti_join(tt_weekenders, by = c("SAMPN","PERNO")) %>% 
  # Add trip information. We want to find all travelers that only traveled
  # outside the region (and thus have zero distance) on their travel days. 
  left_join(tt_place, by = c("SAMPN","PERNO")) %>% 
  # Keep only weekday travel
  filter(!((DAYNO == 2 & DAY == 5) | (DAYNO == 1 & DAY == 7))) %>% 
  group_by(SAMPN,PERNO) %>%
  summarize(DIST = sum(DIST),
            weight = first(WGTP)) %>%
  filter(DIST == 0) %>%
  ungroup()

# This accounts for 182,123 travelers.
tt_out_of_region %>% 
  summarize(total = sum(weight))

# Identify travelers in one set but not the other
missing <-
  daily_travelers_tt_test %>%
  anti_join(distinct_daily_travelers_tt, by = c("SAMPN" = "sampno","PERNO" = "perno"))

# These add to 286,494 travelers (649 weighted records)
sum(missing$WGTP)

# This accounts for all missing travelers
missing_unaccounted <-
  missing %>% 
  anti_join(tt_weekenders, by = c("SAMPN","PERNO")) %>% 
  anti_join(tt_out_of_region, by = c("SAMPN","PERNO"))

missing_unaccounted %>% 
  summarize(total = sum(WGTP))

# The remaining discrepancy is primarily based on the adjustment to weights for
# travelers in 2-day surveys that only traveled on one of their two travel days.
tt_one_of_two_weekdays <-
  daily_travelers_tt_test %>%
  # Remove all travelers accounted for as only weekend travelers
  anti_join(tt_weekenders, by = c("SAMPN","PERNO")) %>%   
  # And out of region travelers
  anti_join(tt_out_of_region, by = c("SAMPN","PERNO")) %>% 
  # Keep those with 2 weekdays
  filter(SURVEY == 2 & DAY %in% c(1,2,3,4)) %>% 
  # Add trip information. We want to find all travelers with zero trips and/or
  # travel distance on one (but not both) of their two days.
  left_join(tt_place, by = c("SAMPN","PERNO")) %>% 
  group_by(SAMPN,PERNO,DAYNO,PTRIPS1,PTRIPS2) %>%
  summarize(DIST = sum(DIST),
            weight = first(WGTP)) %>%
  filter(DIST == 0 | (PTRIPS1 == 0 & PTRIPS2 >0) | (PTRIPS1 > 0 & PTRIPS2 == 0)) %>% 
  ungroup() %>% 
  distinct(SAMPN,PERNO,weight) 

# This accounts for 99,568 travelers that have been double-weighted in Sarah's
# but single-weighted in mine.
tt_one_of_two_weekdays %>% 
  summarize(total = sum(weight) / 2)

# This exactly accounts for the difference between the two figures

# The updated baseline
(daily_travelers_tt_test %>% summarize(sum = sum(WGTP))) -
  # Less the weekday double weighting
  (tt_one_of_two_weekdays %>% summarize(total = sum(weight) / 2)) -
  # Less the weekend-only travelers
  (tt_weekenders %>% summarize(total = sum(weight))) -
  # Less the out of region only travelers
  (tt_out_of_region %>% summarize(total = sum(weight))) ==
  # Equals the total travelers above
  (distinct_daily_travelers_tt %>% summarize(sum = sum(weight)))

# Confirm there are no travelers included above not captured in the test
distinct_daily_travelers_tt %>% 
  anti_join(daily_travelers_tt_test, by = c("sampno" = "SAMPN","perno"="PERNO"))

   