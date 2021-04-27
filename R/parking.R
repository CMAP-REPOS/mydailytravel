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

household_vehicles_mdt <-
  mdt_all_respondents %>% 
  select(sampno,perno,wtperfin,hhinc,hhveh,
         home_state,home_county,home_tract,home_county_chi,geog,
         income,income_c,race_eth) %>% 
  filter(perno == 1) %>% 
  select(-perno) %>%
  left_join(veh, by = "sampno") %>% 
  group_by(sampno) %>% 
  mutate(household_race_eth_concat = paste(race_eth, collapse = ",")) %>% 
  ungroup() %>% 
  mutate(
    household_race_eth = case_when(
      grepl("white",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "white",
      grepl("hispanic",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "hispanic",
      grepl("other",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "other",
      grepl("asian",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE) |
            grepl("black",household_race_eth_concat, fixed = TRUE)) ~ "asian",
      grepl("black",household_race_eth_concat, fixed = TRUE) & 
        !(grepl("white",household_race_eth_concat, fixed = TRUE) |
            grepl("other",household_race_eth_concat, fixed = TRUE) |
            grepl("asian",household_race_eth_concat, fixed = TRUE) |
            grepl("hispanic",household_race_eth_concat, fixed = TRUE)) ~ "black",
      TRUE ~ "mixed"
    ))
  

household_vehicles_tt <-
  tt_all_respondents %>% 
  select(SAMPN,PERNO,WGTHH,HHVEH,
         home_county_chi,geog,
         income_c) %>% 
  filter(PERNO == 1) %>% 
  select(-PERNO) %>%
  left_join(tt_veh, by = "SAMPN") 

################################################################################
# Parking
################################################################################

household_parking_mdt <-
  household_vehicles_mdt %>% # 21498
  ungroup() %>% 
  # Add values for households with no vehicle
  replace_na(list(parkd = 0)) %>% 
  # Remove entries without parking information
  filter(parkd > -1) %>% # 21484
  group_by(sampno,wtperfin,hhinc,hhveh,
           home_state,home_county,home_tract,home_county_chi,geog,
           income,income_c,race_eth) %>%
  # Keep the lowest value for parking - i.e., if a household parks any car on
  # street, it is identified as using street parking.
  summarize(parkd = min(parkd)) %>% # 12389
  ungroup() %>% 
  # Recode for ease of presentation and cluster "Off street" and "Garage" (2 and 3)
  mutate(parkd = recode_factor(factor(parkd,levels = c(0,1,2,3,97)),
                               "0" = "No vehicle",
                               "1" = "Park car(s) on-street",
                               "2" = "Only park car(s) off-street",
                               "3" = "Only park car(s) off-street",
                               "97" = "Other"))


household_parking_tt <-
  household_vehicles_tt %>% # 21498
  ungroup() %>% 
  # Add values for households with no vehicle
  replace_na(list(PARKD = 0)) %>% 
  # Remove entries without parking information
  filter(!(PARKD %in% c(8,9))) %>% # 21484
  group_by(SAMPN,WGTHH,HHVEH,
           home_county_chi,geog,
           income_c) %>%
  # Keep the lowest value for parking - i.e., if a household parks any car on
  # street, it is identified as using street parking.
  summarize(PARKD = min(PARKD)) %>% # 12389
  ungroup() %>% 
  # Recode for ease of presentation and cluster "Off street - driveway" and
  # "Garage" (2 and 3)
  mutate(parkd = recode_factor(factor(PARKD,levels = c(0,1,2,3,7)),
                               "0" = "No vehicle",
                               "1" = "Park car(s) on-street",
                               "2" = "Only park car(s) off-street",
                               "3" = "Only park car(s) off-street",
                               "97" = "Other"))

# Combine the two datasets with relevant variables
household_parking <-
  rbind(household_parking_mdt %>% select(weight = wtperfin,geog,parkd) %>% mutate(survey = "mdt"),
        household_parking_tt %>% select(weight = WGTHH,geog,parkd) %>% mutate(survey = "tt"))

# Identify percentage of parking behavior and vehicle ownership
parking_behavior <-
  pct_calculator(
  household_parking %>% 
    filter(geog != "Other",
           parkd != "Other",
           survey == "mdt"),
  breakdown_by = "parkd",
  second_breakdown = "geog",
  weight = "weight") %>% arrange(parkd,geog)


parking_p1 <-
  # Get data
  parking_behavior %>% 
  # Reorder
  mutate(geog = factor(geog, levels = c("Other suburban counties",
                                        "Suburban Cook",
                                        "Chicago")),
         parkd = factor(parkd, levels = rev(levels(parkd)))) %>% 
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(geog,15), fill = parkd,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(position = position_stack(reverse = T)) +
  
  geom_text(position = position_stack(vjust = 0.5, reverse = T),
            color = "white") +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",
             xlab = "Households by vehicle ownership and parking behavior") +
  cmap_fill_discrete(palette = "friday") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent())
  

finalize_plot(parking_p1,
              "Chicago households are the most likely to not have a vehicle or 
              to rely on street parking, but there are similar households in the 
              rest of the region as well.",
              paste0("Note: 'Park car(s)' on-street includes all households that 
              park at least one car on-street, even if other cars are parked 
              off-street. Categories without labels have values of less than 5 
              percent.
              <br><br>
              Sample size: 
              <br>- Chicago (",
              parking_behavior %>% ungroup() %>% 
                filter(geog == "Chicago") %>% 
                select(total_n) %>% 
                distinct() %>% 
                select(total_n)
              ,");
              <br>- Suburban Cook (",
              parking_behavior %>% ungroup() %>% 
                filter(geog == "Suburban Cook") %>% 
                select(total_n) %>% distinct() %>% 
                select(total_n)
              ,");
              <br>- Other suburban counties (",
              parking_behavior %>% ungroup() %>% 
                filter(geog == "Other suburban counties") %>% 
                select(total_n) %>% 
                distinct() %>% 
                select(total_n)
              ,").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "parking_p1",
              mode = "png",
              overwrite = T)

################################################################################
# Fuel type
################################################################################

household_fuel_type <-
  household_vehicles %>% 
  replace_na(list(fuel = 0)) %>% 
  filter(fuel > -1) %>%
  # group_by(sampno,wtperfin,hhinc,hhveh,
  #          home_state,home_county,home_tract,home_county_chi,geog,
  #          income,income_c,race_eth) %>% 
  # summarize(park_simp = min(parkd)) %>% 
  mutate(fuel = recode_factor(factor(fuel,levels = c(0,1,2,3,4,5,97,-7,-8)),
                              "0" = "No vehicle",
                              "1" = "Gas",
                              "2" = "Diesel",
                              "3" = "Hybrid",
                              "4" = "EV",
                              "5" = "Alternative",
                              "97" = "Other",
                              "-7" = "Missing",
                              "-8" = "Missing"))

pct_calculator(household_fuel_type %>% filter(!(fuel %in% c("No vehicle","Missing"))),
               breakdown_by = "fuel",
               second_breakdown = "household_race_eth",
               weight = "wtperfin") %>% View()


household_fuel_type %>% filter(fuel == "EV") %>% count(household_race_eth,wt = wtperfin)
