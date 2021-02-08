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

source("data_cleaning.R")

# Create base data set with TNC variables (not included in the base data pull)
tnc_base <- read_csv("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data/person.csv") %>%
  select(sampno,
         perno,
         age,
         sex,
         hisp,
         race,
         smrtphn,
         wkstat,
         tnc_use,
         tnc_typ,
         tnc_cost,
         tnc_purp,
         disab,
         pertrips,
         wtperfin)

# Calculate the number of trips taken by travelers on active modes like transit,
# biking, and walking, as well as TNC trips
active_travel <-
  mdt %>%
  group_by(sampno,perno) %>%
  summarize(takes_transit = sum(mode_c == "transit",na.rm = TRUE),
            takes_bike = sum(mode_c == "bike",na.rm = TRUE),
            takes_walk = sum(mode_c == "walk",na.rm = TRUE),
            takes_tnc = sum((mode == "rideshare") | (mode == "shared rideshare"),na.rm = TRUE)) %>%
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))



# Call "recoding.R" to get helper vectors and lists for recoding
source("recoding.R")

# Create working dataset based on variables pulled above and person/hh
# characteristics from other tables
tnc <-
  tnc_base %>%                          # 30,683 records
  # Add home county
  inner_join(home, by = "sampno") %>%   # 30,683 records
  # Add household statistics
  inner_join(hh, by = "sampno") %>%     # 30,683 records
  # Recode race and ethnicity (uses same logic as recoding.R)
  mutate(race_eth =
           recode(race,
                  "1" = "white","2" = "black",
                  "3" = "asian","4" = "other",
                  "5" = "other","6" = "other",
                  "97" = "other","-8" = "missing",
                  "-7" = "missing")) %>%
  # Recode Hispanic status (uses same logic as recoding.R)
  mutate(race_eth = case_when(
    hisp == 1 ~ "hispanic",
    TRUE ~ race_eth))  %>%
  # Bucket household income (uses same logic as recoding.R)
  mutate(income = factor(hhinc),
         income = recode_factor(income,!!!recode_income_detailed_mdt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt)) %>%
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

# Remove recode helpers
rm(recode_income_buckets_mdt,recode_income_buckets_tt,
   recode_mode_buckets_mdt,recode_mode_buckets_tt,
   recode_tpurp_buckets_mdt,recode_tpurp_buckets_tt,
   recode_income_detailed_mdt,recode_income_detailed_tt,
   recode_mode_detailed_mdt,recode_mode_detailed_tt,
   recode_tpurp_detailed_mdt,recode_tpurp_detailed_tt)

# Age bins
breaks <- c(-1, 10, 17, 29, 39, 49, 59, 69, 79, 89)
age_labels <- c("5 to 9", "10 to 17", "18 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

# Reshape data
tnc <- tnc %>%
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

# Run linear regression
tnc_use_lm <-
  tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(income_c != "missing") %>%
  filter(!(hhveh %in% c(-9,-8,-7)),
         pertrips != -1,
         race_eth != "missing") %>%
  #filter(county_fips == 31) %>%
  #group_by(county_fips) %>%
  lm(tnc_use ~
       takes_transit +
       takes_bike +
       takes_walk +
       county_31 +
       #county_43 + county_89 + county_93 +
       #county_97 + county_111 + county_197 +
       high_income +
       white +
       black +
       hispa +
       asian +
       # other +
       hhveh +
       pertrips,
      .,
      weights = wtperfin)

summary(tnc_use_lm)




## There appears to be a positive correlation with TNC usage and transit use



### Age-based analysis



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
  filter(home_county %in% cmap_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_use)

# Cost by home county
tnc %>%
  filter(tnc_cost>0) %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_cost)
