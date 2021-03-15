# This script analyzes TNC usage in MDT, using a combination of travel diary and
# survey entries

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(ggplot2)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("helper_fns.R")
source("data_cleaning.R")

# Calculate the number of trips taken by travelers on active modes like transit,
# biking, and walking, as well as TNC trips
active_travel <-
  mdt %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97374
  # Exclude trips with no travel distance.
  filter(distance_pg > 0) %>%        # 97316
  # Group data by the traveler level
  group_by(sampno,perno) %>%
  # Create flags for transit, biking, walking, and tnc trips
  summarize(takes_transit = sum(mode_c == "transit",na.rm = TRUE),
            takes_bike = sum(mode_c == "bike",na.rm = TRUE),
            takes_walk = sum(mode_c == "walk",na.rm = TRUE),
            takes_tnc = sum((mode == "rideshare") | 
                              (mode == "shared rideshare"),na.rm = TRUE)) %>%
  # Create a flag that is 1 if any of transit, biking, or walking is nonzero,
  # and zero otherwise
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))

# Create working dataset based on variables pulled above and person/hh
# characteristics from other tables
tnc <-
  mdt_all_respondents %>%
  # Keep only travelers 18+ (they are the ones that answered TNC survey
  # questions)
  filter(age >= 18 |
           (age < 0 & aage %in% c(5,6,7)) |
           (age < 0 & age18 == 1)) %>% 
  # Add information on active travel from above
  left_join(active_travel, by = c("sampno","perno")) %>%
  # Add 0s for people with no trips (who otherwise are NAs)
  mutate(across(starts_with("takes_"),~replace(.,is.na(.),0))) %>%
  # Create dummy variables for analysis
  mutate(n = 1,
         county = as.character(home_county),
         white = case_when(
           race_eth == "white" ~ 1,
           TRUE ~ 0
         ),
         black = case_when(
           race_eth == "black" ~ 1,
           TRUE ~ 0
         ),
         asian = case_when(
           race_eth == "asian" ~ 1,
           TRUE ~ 0
         ),
         hispa = case_when(
           race_eth == "hispanic" ~ 1,
           TRUE ~ 0
         ),
         other = case_when(
           race_eth == "other" ~ 1,
           TRUE ~ 0
         ),
         high_income = case_when(
           income_c %in% c("high","middle-high") ~ 1,
           TRUE ~ 0
         ))

# Age bins
breaks <- c(-1, 9, 17, 29, 39, 49, 59, 69, 79, 89)
age_labels <- c("5 to 9", "10 to 17", "18 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

# Reshape data to add flags for each county as individual columns
tnc_wide <- tnc %>%
  pivot_wider(
    names_from = county,
    id_cols = c(sampno:takes_active,white:high_income),
    values_from = n,
    names_prefix = 'county_',
    values_fill = list(n = 0)) %>%
  mutate(age_bin = cut(age, breaks = breaks,
                       labels = age_labels))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
# Regression
################################################################################

# Run linear regression
tnc_use_lm <-
  # Get data
  tnc_wide %>%
  # Filter out respondents who don't have an answer for...
    # TNC use
    filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
    # Household incomes
    filter(income_c != "missing") %>%
    # Household vehicles
    filter(!(hhveh %in% c(-9,-8,-7))) %>% 
    # Trips
    filter(pertrips != -1) %>% 
    # Race and ethnicity
    filter(race_eth != "missing") %>% 
    # Smartphone
    filter(!(smrtphn %in% c(-9,-8,-7,-1))) %>%
  # Recode smartphone to be a dummy with 1 as "yes" and 0 as "no" (previously
  # "no" was coded as 2)
  mutate(smrtphn = recode(smrtphn,
                          "1" = "1",
                          "2" = "0")) %>% 
  
  # Execute linear regression
  lm(tnc_use ~
       takes_transit + # How many times did they take transit on their travel day?
       takes_bike + # How many times did they bike on their travel day?
       takes_walk + # How many times did they walk on their travel day?
       county_31 + # Is their home in Cook County?
       high_income + # Is their household income above $60,000/year?
       hhveh + # How many household vehicles do they have?
       smrtphn + # Do they have a smartphone?
       pertrips + # How many trips did they take on their travel day?
       # Are they: 
       white + # White (non-Hispanic)?
       black + # Black?
       hispa + # Hispanic (of any race)?
       asian, # Asian?
      .,
      # We use the weights to better represent the regional trends (vs. raw
      # responses)
      weights = wtperfin
     )

summary(tnc_use_lm)

## There appears to be a positive correlation with TNC usage and transit use


################################################################################
# Statistics about usage and cost by age, race/ethnicity, and home county
################################################################################

## Look at usage by age
tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1)),
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Cost by age
tnc %>%
  filter(tnc_cost >0,
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

# Usage by race and ethnicity
tnc %>%
  filter(race_eth != "missing",
         !(tnc_use %in% c(-9,-8,-7,-1))) %>%
  group_by(race_eth) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Usage by home county
tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_use)

# Cost by home county
tnc %>%
  filter(tnc_cost>0) %>%
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_cost)
