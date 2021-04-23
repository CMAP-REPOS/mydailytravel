# This script analyzes parking behavior in MDT

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
source("R/helper_fns.R")
source("R/data_cleaning.R")


# Join the first household responder for MDT with vehicle information
household_parking <-
  mdt_all_respondents %>% 
  select(sampno,perno,wtperfin,hhinc,hhveh,
         home_state,home_county,home_tract,home_county_chi,geog,
         income,income_c,race_eth) %>% 
  filter(perno == 1) %>% 
  select(-perno) %>%
  left_join(veh, by = "sampno") %>% 
  replace_na(list(fuel = 0,parkd = 0)) %>% 
  filter(parkd > -1) %>%
  group_by(sampno,wtperfin,hhinc,hhveh,
           home_state,home_county,home_tract,home_county_chi,geog,
           income,income_c,race_eth) %>% 
  summarize(park_simp = min(parkd)) %>% 
  mutate(park_simp = recode_factor(factor(park_simp,levels = c(0,1,2,3,97)),
                               "0" = "No vehicle",
                               "1" = "On street",
                               "2" = "Off street",
                               "3" = "Off street",
                               "97" = "Other"))

pct_calculator(
  household_parking %>% 
    filter(geog != "Other",
           income_c != "missing",
           park_simp != "Other"),
  breakdown_by = "income_c",
  second_breakdown = "park_simp",
  third_breakdown = "geog",
  weight = "wtperfin") %>% arrange(park_simp,income_c) %>% View()


### RECODING FOR FUEL
  # mutate(fuel = recode_factor(factor(fuel,levels = c(0,1,2,3,4,5,97,-7,-8)),
  #                             "0" = "No vehicle",
  #                             "1" = "Gas",
  #                             "2" = "Diesel",
  #                             "3" = "Hybrid",
  #                             "4" = "EV",
  #                             "5" = "Alternative",
  #                             "97" = "Other",
  #                             "-7" = "Missing",
  #                             "-8" = "Missing"),