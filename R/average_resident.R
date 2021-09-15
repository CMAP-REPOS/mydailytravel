# This script analyzes average travel behavior in My Daily Travel and Travel
# Tracker. It is referenced in Policy Brief #2.

#################################################
#                                               #
#                 Library loading               #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
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
age_breaks_avg_res <- c(-1,17,29, 49, 69, 150)
age_labels_avg_res <- c("5 to 17","18 to 29", "30 to 49",  "50 to 69", "70 and above")

# filter out trips we don't want to evaluate
avgtravel_mdt <-
  mdt %>%                    # 125463 records
  filter(
    # Keep only trips with travelers at least 5 years old. age < 0 are all
    # respondents without a numeric age value.
    age >= 5 |               # 125447 records
      (age < 0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8))) %>%
  # Filter out "beginning" trips
  filter(mode != "beginning") %>% # 97365 records
  # Keep only people who traveled
  filter(pertrips > 0) %>%   # 97364 records
  # Keep only trips < 15 hours
  filter(travtime_pg_calc < 15*60) %>%  # 97354 records
  # Keep only trips > 0 minutes
  filter(travtime_pg_calc > 0) %>% # 97321 records
  
  ###### RUN ONE OF THE TWO BELOW (MDT-only vs. MDT + TT comparison)
  
  # ### MDT ONLY
  # # Eliminate 0 distance trips (network - use for MDT-specific analyses)
  # filter(distance_pg > 0) %>%   # 97264 records

  ### MDT VS TT
  # Eliminate 0 distance trips (haversine - only use for comparisons with TT)
  filter(hdist_pg > 0) %>%   # 96334 records
  # Filter out trips that were not within the TT travel region (only for
  # comparisons with TT - see explanation in TT data prep below)
  filter(out_tt_trip == 0) %>% # 95872 records
  
  # Add age bins
  mutate(age_bin=cut(age,breaks=age_breaks_avg_res,labels=age_labels_avg_res)) %>% 
  # Keep only variables of interest
  select(sampno,perno,age_bin,sex,income_c,race_eth,home_county_chi,disab,
         hdist = hdist_pg, distance = distance_pg, travtime = travtime_pg,
         weight,orig_weight) %>% 
  mutate(survey = "mdt")

# Create a similarly filtered list of all respondents (with age filters but not
# travel)
avgtravel_all_respondents_mdt <-
  mdt_all_respondents %>%  # 30683
  filter(
    # Keep only trips with travelers at least 5 years old.
    age >= 5 |               
      (age < 0 & aage %in% c(2,3,4,5,6,7)) |
      (age < 0 & schol %in% c(4,5,6,7,8))) # 28570

# Identify distinct list of travelers that took at least one trip to enable
# summing by different demographic characteristics
distinct_daily_travelers_mdt <-
  avgtravel_mdt %>% # 95872
  select(sampno,perno,weight,orig_weight,race_eth,sex,income_c,disab,
         home_county_chi,age_bin,survey) %>%
  distinct() # 24625

# Identify individuals who did travel in the survey, but are excluded based on 
# the filtering criteria (e.g., they did not travel in the CMAP area)
ineligible_travelers_mdt <-
  # Take the full list of respondents
  avgtravel_all_respondents_mdt %>% # 28570
  # Remove all respondents who are included in the list of distinct daily travelers
  anti_join(distinct_daily_travelers_mdt, by = c("sampno","perno")) %>% # 3945
  # Identify those of the remainder that did travel, and are thus excluded from
  # the list of distinct travelers based on some other condition
  filter(pertrips > 0) %>% # 379
  # add age bins and survey ID
  mutate(age_bin=cut(age,breaks=age_breaks_avg_res,labels=age_labels_avg_res),
         survey = "mdt") %>% 
  # Keep relevant variables and rename weight for merging with TT
  select(sampno,perno,weight,orig_weight,race_eth,sex,income_c,disab,
         home_county_chi,age_bin,survey)

# Add back the ineligible travelers for the purpose of travel percent calculation
total_traveler_universe_mdt <-
  rbind(ineligible_travelers_mdt,
        distinct_daily_travelers_mdt)

# Identify list of residents (includes ineligible travelers)
distinct_residents_mdt <-
  avgtravel_all_respondents_mdt %>% # 28570
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks_avg_res,labels=age_labels_avg_res)) %>%
  mutate(survey = "mdt") %>%
  # Keep relevant variables and rename weights for merging with TT
  select(sampno,perno,weight,orig_weight,race_eth,sex,income_c,disab,
         home_county_chi,age_bin,survey)
  

avgtravel_tt <-
  tt %>%                    # 139765 records
  # Keep only trips by travelers at least 5 years old. Note that 99 is DK/RF for
  # AGE. We also keep travelers with unknown age based on school enrollment or
  # AGEB of 2, which indicates 16+
  filter((AGE >= 5 & AGE < 99) |
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGEB == 2 & AGE == 99)) %>% # 132676 records
  # Filter out the first record for each traveler (PLANO == 1)
  filter(PLANO != 1) %>%    # 105568 records
  # Exclude zero distance trips (note that TT did not capture distances for
  # trips outside the seven counties, Illinois' Grundy County, and Lake,
  # LaPorte, and Porter Counties in Indiana, so this means that travelers who
  # only traveled to or from those counties, but not within that region, will be
  # excluded, in addition to any travelers who only traveled outside the
  # region).
  filter(DIST > 0) %>%      # 100573 records
  # Exclude trips with 0 travel time or more than 15 hours
  filter(TRPDUR > 0 & TRPDUR < 15 * 60) %>%  # 100573 records
  # Add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks_avg_res,labels=age_labels_avg_res)) %>% 
  # Add blank column for non-haversine distances
  mutate(distance = 0) %>% 
  # Keep only variables of interest and rename to match MDT variables
  select(hdist = DIST,
         distance,
         travtime = TRPDUR,
         sampno = SAMPN,
         perno = PERNO,
         sex = GEND,
         disab = DISAB,
         age_bin,income_c,race_eth,home_county_chi,weight,
         DAYNO) %>% 
  mutate(survey = "tt",
         orig_weight = weight)

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
  select(sampno,perno,weight,orig_weight,race_eth,sex,income_c,disab,
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
  filter(((PTRIPS1 > 0 & DAY != 7) | (PTRIPS2 > 0 & DAY != 5))) %>% # 382
  # Adjust weights to reflect two-day travel behavior, if applicable
  mutate(weight = case_when(
    # Any one-day surveys should be kept at full weight
    SURVEY == 1 ~ WGTP,
    # Two day surveys that are both weekdays that have travel are also full
    # weight
    DAY %in% c(1,2,3,4) & PTRIPS1 > 0 & PTRIPS2 > 0 ~ WGTP,
    # Other two day weekday surveys should be half weight
    DAY %in% c(1,2,3,4) ~ WGTP/2,
    # And any weekday/weekend surveys included had weekday travel, so they
    # should be full weight
    DAY %in% c(5,7) ~ WGTP)) %>% 
  # Add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks_avg_res,labels=age_labels_avg_res),
         survey = "tt",
         orig_weight = weight) %>% 
  rename(sampno = SAMPN,
         perno = PERNO) %>% 
  select(sampno,perno,weight,orig_weight,race_eth,sex = GEND,income_c,disab = DISAB,
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
  mutate(age_bin=cut(AGE,breaks=age_breaks_avg_res,labels=age_labels_avg_res)) %>%
  mutate(survey = "tt",
         orig_weight = WGTP) %>%
  select(sampno = SAMPN,perno = PERNO,weight = WGTP,orig_weight,disab = DISAB,
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

# Distinct residents from both surveys
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

# NOTE: The primary distance variable in MDT was "network distance." The code
# below uses the haversine distance to enable comparability with TT. If you want
# to just look at total distance traveled, that will require changing the calls
# to distance variables to use the dist variants and modifying the exclusion of
# nonzero distance trips above.

################################################################################
# Summary statistics
################################################################################

# Helper function to calculate summary statistics
travel_calculator <- function(data = avgtravel,grouping,
                              chosen_distance,weight = "weight") {
  data %>% 
    group_by(across(all_of(grouping))) %>% 
    summarize(
      total_distance = sum(.data[[chosen_distance]] * .data[[weight]]),
      total_distance_uw = sum(.data[[chosen_distance]]),
      total_time = sum(travtime * .data[[weight]]),
      total_time_uw = sum(travtime),
      total_trips = sum(.data[[weight]]),
      total_trips_uw = n(),
      avg_trip_length = total_distance / total_trips,
      avg_trip_length_uw = total_distance_uw / total_trips_uw,
      avg_trip_time = total_time / total_trips,
      avg_trip_time_uw = total_time_uw / total_trips_uw,
      n = n()
    )  %>% 
    # Add total number of travelers (only eligible travelers)
    left_join(distinct_daily_travelers %>%
                group_by(across(all_of(grouping))) %>% 
                summarize(total_eligible_travelers = sum(.data[[weight]])),
              by = c(grouping)) %>%  
    # Add unweighted number of travelers (only eligible)
    left_join(distinct_daily_travelers %>% 
                # Remove double entries for multi-day surveys
                distinct() %>% 
                group_by(across(all_of(grouping))) %>% 
                summarize(total_eligible_travelers_uw = n()),
              by = c(grouping)) %>% 
    # Add total number of travelers (all travelers)
    left_join(total_travel_universe %>%
                group_by(across(all_of(grouping))) %>% 
                summarize(total_travelers = sum(.data[[weight]])),
              by = c(grouping)) %>%
    # Add total number of unweighted travelers (all travelers)
    left_join(total_travel_universe %>%
                # Remove double  entries for multi-day surveys
                distinct() %>% 
                group_by(across(all_of(grouping))) %>% 
                summarize(total_travelers_uw = n()),
              by = c(grouping)) %>%
    # Add total number of residents
    left_join(distinct_residents %>% 
                group_by(across(all_of(grouping))) %>% 
                summarize(total_residents = sum(.data[[weight]]),
                          total_residents_uw = n()),
              by = c(grouping)) %>% 
    # Calculate distance and trips per capita using total travelers
    mutate(distance_per_capita = total_distance / total_eligible_travelers,
           distance_per_capita_uw = total_distance_uw / total_eligible_travelers_uw,
           trips_per_capita = total_trips / total_eligible_travelers,
           trips_per_capita_uw = total_trips_uw / total_eligible_travelers_uw,
           time_per_capita = total_time / total_eligible_travelers,
           time_per_capita_uw = total_time_uw / total_eligible_travelers_uw,
           traveling_pct = total_travelers / total_residents,
           traveling_pct_uw = total_travelers_uw / total_residents_uw)
    
}


# Calculate summary statistics (code reused below for variations by demography)
travel_overall <-
  travel_calculator(avgtravel,"survey","hdist","weight") %>% 
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
  travel_calculator(avgtravel,c("survey","sex"),"hdist","weight") %>% 
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
  travel_calculator(avgtravel,c("survey","income_c"),"hdist","weight") %>% 
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
  travel_calculator(avgtravel,c("survey","age_bin"),"hdist","weight") %>% 
  ungroup() %>% 
  # Remove individuals without a response
  filter(!is.na(age_bin)) %>% 
  mutate(type = "Age") %>% 
  rename(subtype = age_bin)

# Calculate summary statistics by home jurisdiction (reusing overall code)
travel_home <-
  travel_calculator(avgtravel,c("survey","home_county_chi"),"hdist","weight") %>% 
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
  travel_calculator(avgtravel,c("survey","race_eth"),"hdist","weight") %>% 
  ungroup() %>% 
  # Remove missing 
  filter(race_eth != "missing") %>% 
  # Recode for ease of understanding
  mutate(type = "Race and ethnicity",
         subtype = recode_factor(race_eth,
                                 "white" = "White",
                                 "latino" = "Latino",
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
  # Add levels
  mutate(type = factor(type,
                       levels = c("Race and ethnicity","Household income",
                                  "Age","Sex","Overall","Home jurisdiction"))) %>% 
  mutate(subtype = factor(subtype,
                          levels = c("Overall",
                                     "White","Asian","Black","Latino","Other",
                                     "Less than $35K","$35K to $59K","$60K to $99K","$100K or more",
                                     "5 to 17","18 to 29","30 to 49","50 to 69","70 and above",
                                     "Male","Female"
                                     ))) %>% 
  # Pivot longer
  pivot_longer(cols = c(total_distance:traveling_pct_uw))

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

regional_averages <-
  travel_summaries_vlines %>% 
  filter(survey == "mdt") %>%
  ungroup() %>% 
  select(-survey) %>% 
  distinct()

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
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>%
  # Add blank for label positioning
  mutate(blank = case_when(
    name == "trips_per_capita" ~ 5.5,
    name == "avg_trip_length" ~ 5.99,
    name == "avg_trip_time" ~ 36,
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
  
  scale_y_discrete(limits = c("Female","Male", 
                              "",
                              "70 and above","50 to 69","30 to 49","18 to 29","5 to 17",
                              "",
                              "$100K or more","$60K to $99K","$35K to $59K","Less than $35K",
                              "",
                              "Other","Latino","Black","Asian","White","")) +
  scale_x_continuous(expand = expansion(mult = c(0.05,0))) +
  
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = regional_averages %>%
               filter(name != "Percent traveling"), # %>%
               # mutate(color = "Regional average"),
             mapping = aes(xintercept = value,
                           # color = color
                           ),
             linetype = "dashed",
             size = .33
  ) +
  
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = name),
             position = position_dodge2(width = .9,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             label.r = grid::unit(0,"lines"),
             hjust = -.02) +
    # Add geom_blank for positioning
  geom_blank(aes(x = blank)) +

  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Average behavior for residents who traveled",
             strip.text.x = element_text(family = "Whitney Semibold",
                                         hjust = 0.5,vjust = 1),
             strip.text.y = element_blank()) +
  scale_fill_discrete(type = c("#D8BA37","#D93636","#38B2D8","#7451A1")) +
  
  # Add regional labels
  geom_label(data = regional_averages %>%
               filter(name != "Percent traveling"),
             mapping = aes(x = value * .05,
                           y = 20,
                           label = paste0("Regional avg. (",
                                          round(value,1),")")),
             hjust = 0,
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             fill = "light gray"
  ) +
  
  facet_grid(~name,
             scales = "free"
  )

# Export finalized graphic
finalize_plot(average_resident_p1,
              "Average travel patterns vary significantly based on demographic
              characteristics.",
              caption = paste0(
              "Note: Figures are calculated based only on individuals
              who traveled and thus exclude individuals with zero trips.
              Includes trips by travelers age 5 and older who live in the
              CMAP seven-county region, Grundy, and DeKalb.
              Distances are point-to-point and do not account for additional 
              distance traveled along the route.
              See \"About the data\" for more information on race, ethnicity, and sex.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_daily_travelers_mdt),big.mark = ","),
                     " travelers.
              Across all categories, travelers with an \"Other\" race and
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
              sidebar_width = 1.75,
              mode = c("png","pdf"),
              height = 6.5,
              overwrite = T)


# Identify sample sizes ("Other" race/eth has the lowest)

distinct_daily_travelers_mdt %>% count(age_bin)
distinct_daily_travelers_mdt %>% count(income_c)
distinct_daily_travelers_mdt %>% count(race_eth)
distinct_daily_travelers_mdt %>% count(sex)

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
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>% 
  # Exclude overall and geography
  filter(!(type %in% c("Overall"))) %>%
  # Mutate name to match
  mutate(name = "Percent traveling") %>% 
  
  # Create ggplot object
  ggplot(aes(x = 1 - value, y = subtype, fill = type)) +
  # Add columns
  geom_col(width = .8) +
  
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = travel_summaries_vlines %>% 
               filter(survey == "mdt") %>%
               filter(name == "Percent traveling"),# %>% 
               # mutate(color = paste0("Regional average (",round(100*(1-value[1])),"%)")),
             mapping = aes(xintercept = 1 - value,
                           # color = color
                           ),
             linetype = "dashed",
             size = .33
  ) +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 1)(1 - value),
                 group = name),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             label.r = grid::unit(0,"lines"),
             hjust = -.02) +
  
  # Add regional label
  annotate(geom = "label",
           x = 0.001 + (1 - regional_averages %>% 
             filter(name == "Percent traveling") %>% 
             select(value) %>% distinct() %>% 
             as.numeric()),
           y = 19,
           label = paste0("Regional average (",
                          round(100*(1- regional_averages %>% 
                                       filter(name == "Percent traveling") %>% 
                                       select(value) %>% distinct() %>% 
                                       as.numeric())),
                          "%)"),
           vjust = 0.5,
           hjust = 0,
           fill = "light gray",
           label.size = 0,
           label.r = grid::unit(0,"lines")
           ) +
  
  # Adjust scale
  scale_x_continuous(limits = c(0,.18),
                     labels = scales::label_percent(accuracy = 1),
                     breaks = c(0,.05,.1,.15,.2),
                     expand = expansion(mult = c(0.05,0))
                     ) +
  scale_y_discrete(limits = c("Female","Male", 
                              "",
                              "70 and above","50 to 69","30 to 49","18 to 29","5 to 17",
                              "",
                              "$100K or more","$60K to $99K","$35K to $59K","Less than $35K",
                              "",
                              "Other","Latino","Black","Asian","White")) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Percent of residents who did not travel",
             strip.text.x = element_blank(),
             strip.text.y = element_blank()) +
  scale_fill_discrete(type = c("#D8BA37","#D93636","#38B2D8","#7451A1")) +
  scale_color_discrete(type = "black") +

  # Reorder legends
  guides(color = guide_legend(order = 2),fill = guide_legend(order = 1))

# Export finalized graphic
finalize_plot(average_resident_p2,
              "Residents from households with low income, non-white residents, 
              and older residents were the least likely to travel on a weekday.",
              caption = paste0(
                "Note: Figures are based on the travel behavior of
              residents age 5 or older of the CMAP seven-county region, Grundy, 
              and DeKalb.
              Individuals were counted as \"traveling\" if they had at least one 
              trip on their assigned travel day, no matter whether that trip was 
              in the CMAP region.
              See \"About the data\" for more information on race, ethnicity, and sex.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_residents_mdt),big.mark = ","),
                     " residents. 
              Across all categories, residents with an \"Other\" race and 
              ethnicity have the lowest sample size, with ",
                     (distinct_residents_mdt %>% 
                        count(race_eth) %>% 
                        filter(race_eth == "other"))$n,
                        " individual residents.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "average_resident_p2",
              mode = c("png","pdf"),
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
  # Keep only income
  filter(type %in% c("Household income")) %>% 
  # Identify entries where MDT has a higher value than TT
  mutate(helper = ifelse(survey == "mdt", value,-1*value)) %>% 
  group_by(# type,
           subtype,name) %>% 
  mutate(increasing = sum(helper)) %>% 
  mutate(increasing = case_when(
    increasing > 0 & survey == "mdt" ~ "Increased travel",
    TRUE ~ "Decreased travel"
  )) %>% 
  # Rename variables we are keeping
  mutate(survey = recode_factor(factor(survey),
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  # Fix ordering
  mutate(subtype = factor(subtype, levels = rev(levels(subtype)))) %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = str_wrap_factor(subtype,18),pattern = increasing)) +
  
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = survey,
                                  pattern = increasing),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_size = 0.2,
                              pattern_spacing = 0.02,
                              position = position_dodge2(reverse = T),
                              width = 0.8) +  
  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("Increased travel" = "stripe",
                                  "Decreased travel" = "none"),
                       guide = "none") +
  
  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 1)(value),
                 group = survey),
             position = position_dodge2(width = .8,reverse = T),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             label.r = grid::unit(0,"lines"),
             hjust = -.02) +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,25),
                     expand = expansion(mult = c(.05,0))
                     ) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Distance per day for residents who traveled (miles)",
             ylab = "Household income"
             ) +
  scale_fill_discrete(type = c("#2C2B7F","#43B649")) +
  
  guides(fill = guide_legend(override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(average_resident_p3,
              "In contrast to the overall regional decline, lower-income 
              travelers reported more travel in 2019 than in 2008.",
              caption = 
              paste0(
              "Note: Includes trips by residents age 5 and older of the
              CMAP seven-county region, Grundy, and DeKalb. 
              Household incomes are not adjusted for 
              inflation, and so there may be some households from Travel Tracker 
              that should be compared to the next-highest household income 
              category.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(distinct_daily_travelers_mdt),big.mark = ","),
                     " travelers for My Daily Travel and ",
                     format(nrow(distinct_daily_travelers_tt),big.mark = ","),
                     " for Travel Tracker. Travelers with $35-59K in household 
                     income in My Daily Travel have the lowest sample size, with ",
                     format(
                       distinct_daily_travelers %>% 
                         filter(income_c == "middle-low", survey == "mdt") %>% 
                         count() %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                     " individual travelers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel and Travel Tracker data."),
              filename = "average_resident_p3",
              mode = c("png","pdf"),
              height = 5,
              overwrite = T)

# Identify sample sizes - $35-59K from MDT has the lowest
distinct_daily_travelers %>% count(survey,income_c) %>% arrange(n)


################################################################################
# Plot of statistics for MDT by disability status
################################################################################

# Calculate summary statistics by disability status
travel_disability <- 
  travel_calculator(avgtravel,c("survey","disab"),"hdist","weight") %>% 
  ungroup() %>% 
  # Remove missing (and effectively filter out all <16)
  filter(disab %in% c(1,2)) %>%
  # Recode for ease of understanding
  mutate(type = "Disability",
         subtype = recode_factor(disab,
                                 "1" = "Has a disability",
                                 "2" = "Does not have a disability")) %>%
  select(-disab) %>% 
  # Pivot longer
  pivot_longer(cols = c(total_distance:traveling_pct_uw))


# Create travel percentages
average_resident_p4 <-
  # Get data
  travel_disability %>%
  # Keep MDT
  filter(survey == "mdt") %>% 
  # Keep only percents
  filter(name %in% c("traveling_pct")) %>% 
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>% 
  # Mutate name to match
  mutate(name = "Percent traveling") %>% 
  
  # Create ggplot object
  ggplot(aes(y = 1 - value, x = str_wrap_factor(subtype,30), fill = subtype)) +
  # Add columns
  geom_col(width = .8) +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 1)(1 - value),
                 group = name),
             position = position_dodge2(width = .9,reverse = T),
             fill = "white",
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             vjust = -.02) +
  
  # Adjust scale
  scale_y_continuous(limits = c(0,0.21),
                     labels = scales::label_percent(accuracy = 1),
                     breaks = c(0,.05,.1,.15,.2)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "h",hline = 0,
             ylab = "Percent of residents who did not travel",
             legend.position = "none",
             legend.max.columns = 1) +
  
  scale_fill_discrete(type = c("#38B2D8","#D88134"))
  
finalize_plot(average_resident_p4,
              "Residents with disabilities were less likely to travel than were
              others in the region.",
              caption = 
              paste0(
                "Note: Includes trips by residents age 16 and older of the
              CMAP seven-county region, Grundy, and DeKalb.
              <br><br>
              Sample size: 
              <br>- Without disability (",
                avgtravel_all_respondents_mdt %>% count(disab) %>% filter(disab == 2) %>% select(n),
                "); 
                <br>- With disability (",
                avgtravel_all_respondents_mdt %>% count(disab) %>% filter(disab == 1) %>% select(n),
                ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "average_resident_p4",
              mode = c("png","pdf"),
              height = 4,
              overwrite = T)
