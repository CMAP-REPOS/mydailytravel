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
  # Get data
  mdt_all_respondents %>% 
  # Keep variables of interest
  select(sampno,perno,wtperfin,wthhfin,hhinc,hhveh,
         home_state,home_county,home_tract,home_county_chi,geog,
         income,income_c,race_eth) %>% 
  # Keep only head of household (this allows us to not duplicate)
  filter(perno == 1) %>% 
  # Remove unneeded person identifier
  select(-perno) %>%
  # Join with vehicle information
  left_join(veh, by = "sampno") %>% 
  # Group by household
  group_by(sampno) %>% 
  # Concatenate the race and ethnicity of household members
  mutate(household_race_eth_concat = paste(race_eth, collapse = ",")) %>% 
  ungroup() %>% 
  # Flag households as uniformly one race and ethnicity, or mixed
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
  
# ARCHIVED CODE - not used for publication
household_vehicles_tt <-
  tt_all_respondents %>%
  select(SAMPN,PERNO,WGTHH,HHVEH,
         home_county_chi,geog,
         income_c) %>%
  filter(PERNO == 1) %>%
  select(-PERNO) %>%
  left_join(tt_veh, by = "SAMPN")

################################################################################
# Total vehicles
################################################################################

# Count the total number of vehicles
total_vehs_mdt <-
  mdt_all_respondents %>% 
  select(sampno,home_county,wthhfin,hhveh) %>%
  distinct() %>% 
  summarize(total_vehs = sum(hhveh * wthhfin)) %>% 
  as.numeric()

# Count the total number of households
total_hhs_mdt <- 
  mdt_all_respondents %>% 
  select(sampno,wthhfin) %>% 
  distinct() %>% 
  count(wt = wthhfin) %>% 
  as.numeric()

# Vehicles per household
total_vehs_mdt / total_hhs_mdt

################################################################################
# Parking data setup
################################################################################

# Analyze parking behavior for MDT
household_parking_mdt <-
  household_vehicles_mdt %>% # 21498
  ungroup() %>% 
  # Add values for households with no vehicle (which are NA since they had no
  # vehicle in the join above)
  replace_na(list(parkd = 0)) %>% 
  # Remove entries without parking information
  filter(parkd > -1) %>% # 21484
  # Group to keep identifiers, weighting, and geography
  group_by(sampno,wtperfin,geog,household_race_eth) %>%
  # Keep the lowest value for parking - i.e., if a household parks any car on
  # street, it is identified as using street parking.
  summarize(parkd = min(parkd)) %>% # 12389
  ungroup() %>% 
  # Recode for ease of presentation and cluster "Off street" and "Garage" (coded
  # as 2 and 3, respectively)
  mutate(parkd = recode_factor(factor(parkd,levels = c(0,1,2,3,97)),
                               "0" = "No vehicle",
                               "1" = "Park car(s) on-street",
                               "2" = "Only park car(s) off-street",
                               "3" = "Only park car(s) off-street",
                               "97" = "Other"))

# Repeat for TT
household_parking_tt <-
  household_vehicles_tt %>% # 18246
  ungroup() %>% 
  # Add values for households with no vehicle
  replace_na(list(PARKD = 0)) %>% 
  # Remove entries without parking information
  filter(!(PARKD %in% c(8,9))) %>% # 17637
  # Keep identifiers, weighting, and geography
  group_by(SAMPN,WGTHH,geog) %>%
  # Keep the lowest value for parking - i.e., if a household parks any car on
  # street, it is identified as using street parking.
  summarize(PARKD = min(PARKD)) %>% # 10248
  ungroup() %>% 
  # Recode for ease of presentation and cluster "Off street - driveway" and
  # "Garage" (2 and 3)
  mutate(parkd = recode_factor(factor(PARKD,levels = c(0,1,2,3,97)),
                               "0" = "No vehicle",
                               "1" = "Park car(s) on-street",
                               "2" = "Only park car(s) off-street",
                               "3" = "Only park car(s) off-street",
                               "97" = "Other"))

# Combine the two datasets with relevant variables
household_parking <-
  rbind(household_parking_mdt %>% 
          select(weight = wtperfin,geog,household_race_eth,parkd) %>% 
          mutate(survey = "mdt"),
        household_parking_tt %>% 
          select(weight = WGTHH,geog,parkd) %>% 
          mutate(survey = "tt",household_race_eth = NA))

# Export total non-vehicle ownership
pct_calculator(
  household_parking %>% 
    filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties"),
           parkd != "Other",
           survey == "mdt"),
  breakdown_by = "parkd",
  weight = "weight") %>% arrange(parkd)

# Identify percentage of parking behavior and vehicle ownership for MDT
parking_behavior <-
  pct_calculator(
  household_parking %>% 
    filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties"),
           parkd != "Other",
           survey == "mdt"),
  breakdown_by = "parkd",
  second_breakdown = "geog",
  # # Archived - allows for breakdown by household race and ethnicity
  # third_breakdown = "household_race_eth",
  weight = "weight") %>% arrange(parkd)


################################################################################
# Parking behavior chart
################################################################################

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
             # Only label bars of at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(position = position_stack(reverse = T)) +
  
  # Add labels
  geom_text(position = position_stack(vjust = 0.5, reverse = T),
            color = "white") +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",
             xlab = "Household vehicle ownership and parking by home jurisdiction") +
  cmap_fill_discrete(palette = "friday") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent())
  

parking_p1_samplesize <-
  parking_behavior %>% 
  ungroup() %>% 
  select(geog,n = total_n) %>% 
  distinct()

finalize_plot(parking_p1,
              "Chicago households were the most likely to not have a vehicle or 
              to rely on street parking, but there were also similar households 
              in the rest of the region.",
              paste0("Note: 'Park car(s) on-street' includes all households that 
              park at least one car on-street, even if other car(s) are parked 
              off-street. Includes households from the CMAP seven county region 
              (Cook, DuPage, Kane, Kendall, Lake, McHenry, and Will), as well as 
              Grundy and DeKalb. Unlabeled bars have values of less than five 
              percent.
              <br><br>
              Sample size: 
              <br>- Chicago (",
              parking_p1_samplesize %>% 
                filter(geog == "Chicago") %>% 
                select(n)
              ,");
              <br>- Suburban Cook (",
              parking_p1_samplesize %>% 
                filter(geog == "Suburban Cook") %>% 
                select(n)
              ,");
              <br>- Other suburban counties (",
              parking_p1_samplesize %>% 
                filter(geog == "Other suburban counties") %>% 
                select(n)
              ,").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "parking_p1",
              mode = c("png","pdf"),
              overwrite = T)
