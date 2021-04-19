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
household_vehs <-
  mdt_all_respondents %>% 
  select(sampno,perno,wtperfin,hhinc,hhveh,
         home_state,home_county,home_tract,home_county_chi,geog,
         income,income_c,race_eth) %>% 
  filter(perno == 1) %>% 
  select(-perno) %>%
  inner_join(veh, by = "sampno") %>% 
  mutate(fuel = recode_factor(factor(fuel,levels = c(1,2,3,4,5,97,-7,-8)),
                              "1" = "Gas",
                              "2" = "Diesel",
                              "3" = "Hybrid",
                              "4" = "EV",
                              "5" = "Alternative",
                              "97" = "Other",
                              "-7" = "Missing",
                              "-8" = "Missing"),
         parkd = recode_factor(factor(parkd,levels = c(1,2,3,97,-7,-8)),
                               "1" = "On street",
                               "2" = "Off street",
                               "3" = "In my garage",
                               "97" = "Other",
                               "-7" = "Missing",
                               "-8" = "Missing"))

pct_calculator(
  household_vehs %>% 
    filter(parkd %in% c("On street","Off street", "In my garage"),
           geog != "Other",
           race_eth != "Missing"),
  breakdown_by = "race_eth",
  second_breakdown = "parkd",
  third_breakdown = "geog",
  weight = "wtperfin") %>% arrange(geog,parkd) %>% View()
