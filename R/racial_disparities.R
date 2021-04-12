# This script produces analyses on racial disparities in trip times in the CMAP
# region.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)
library(MetricsWeighted)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


### Filter data
all_trips_mdt <-
  mdt %>%                            # 125463 records
  filter(
    # Keep only:                     # 108612
    # Those 16 or older
    age >= 16 |
      # or those in an age category from 16 to 44
      (age < 0 & aage %in% c(4,5,6,7))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  # 85014
  # Keep only trips with nonzero distance
  filter(distance_pg > 0) %>%        # 84961
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    # 84924
  ungroup()
  # # Code to exclude high and low weight households, by zone
  # left_join(zones, by = "sampno") %>%
  # arrange(wtperfin) %>%
  # group_by(cluster) %>%
  # mutate(rank = row_number()) %>%
  # mutate(max_rank = max(rank)) %>%
  # mutate(pct_rank = rank / max_rank) %>%
  # filter(pct_rank >= 0.05,
  #        pct_rank <= 0.95) %>%
  # ungroup()

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################


################################################################################
# Travel times to work, healthcare, groceries, and the gym by race
################################################################################

# Filter data
work_time_race_mdt <-
  all_trips_mdt %>%                  # 84294 records
  filter(
    # Keep only trips of employed respondents (either those who report having 1+
    # jobs or those who report being employed)
    emply_ask == 1 | jobs > 0) %>%   # 65775 records
  # Keep only trips to a fixed work location (we are looking for "commute-type"
  # trips)
  filter(tpurp == "Worked at fixed work location") %>%  # 14160
  # Keep only trips to identified work locations
  filter(loctype == 2) %>%           # 12099
  ungroup() %>% 
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 &
    travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,tpurp) %>%
  summarize(travtime = as.numeric(MetricsWeighted::weighted_median(travtime_pg_calc, 
                                                                   w = wtperfin)))


# Filter data
other_time_race_mdt <-
  all_trips_mdt %>%                  # 84294 records
  filter(tpurp %in% c("Health care visit for self",
                      "Shopped (routine like grocery, clothing)",
                      "Went to the gym")) %>%  # 9417
  ungroup() %>% 
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 &
      travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,tpurp) %>%
  summarize(travtime = as.numeric(MetricsWeighted::weighted_median(travtime_pg_calc, 
                                                                   w = wtperfin)))

# Combine results
all_time_race_mdt <-
  rbind(work_time_race_mdt,other_time_race_mdt)

# Chart of travel time to school by household income
racial_disparities_p1 <-
  # Get data
  all_time_race_mdt %>%
  # Recode labels for publication
  mutate(race_eth = recode(race_eth,
                           "black" = "Black","white" = "White",
                           "asian" = "Asian","other" = "Other",
                           "hispanic" = "Hispanic")) %>%
  mutate(tpurp_disp = as.character(tpurp)) %>% 
  mutate(tpurp_disp = recode(factor(tpurp_disp, 
                                    levels = c("Worked at fixed work location",
                                               "Health care visit for self",
                                               "Shopped (routine like grocery, clothing)",
                                               "Went to the gym")),
                             "Worked at fixed work location" = "Work (fixed location)",
                             "Health care visit for self" = "Health care (personal)",
                             "Shopped (routine like grocery, clothing)" = "Routine shopping",
                             "Went to the gym" = "Gym")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = reorder(race_eth,-travtime), y = travtime, fill = race_eth)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 1)(travtime)),
             vjust = 0, label.size = 0, fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h", legend.position = "None",
             xlab = "Median travel times for various trip purposes (minutes)") +
  cmap_fill_race(white = "White", black = "Black", hispanic = "Hispanic", 
                 asian = "Asian", other = "Other") +
  
  # Adjust axes
  scale_y_continuous(limits = c(0,32)) +
  
  # Add faceting by trip purpose
  facet_wrap(~tpurp_disp)

# Export finalized graphic
finalize_plot(racial_disparities_p1,
              "Black residents of the region have significantly longer trips to 
              work, health care, routine shopping, and the gym than those of 
              other residents.",
              "Note: Excludes travelers younger than 16. Trips to 'Work (fixed 
              location)' only include trips made to fixed workplace locations by 
              employed respondents. In all categories, trips with no travel time 
              are excluded. Hispanic' includes individuals of any racial 
              group that identify as Hispanic. All other categories are 
              non-Hispanic.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "racial_disparities_p1",
              # mode = "png",
              # # height = 6.3,
              # width = 11.3,
              overwrite = T)

################################################################################
# BACKUP - Travel times for all trip purposes by race
################################################################################

# Filter data
all_time_race_mdt <-
  all_trips_mdt %>% # 12099 records
  
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 & 
    travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
 
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,tpurp) %>%
  summarize(travtime = as.numeric(MetricsWeighted::weighted_median(travtime_pg_calc, 
                                                                   w = wtperfin)),
            distance = as.numeric(MetricsWeighted::weighted_median(distance_pg, 
                                                                   w = wtperfin)))

# Chart of travel time to school by household income
racial_disparities_p2 <-
  # Get data
  all_time_race_mdt %>%
  # Recode labels for publication
  mutate(race_eth = recode(race_eth,
                           "black" = "Black","white" = "White",
                           "asian" = "Asian","other" = "Other",
                           "hispanic" = "Hispanic")) %>%
  
  # Create ggplot object
  ggplot(aes(x = reorder(race_eth,-travtime), y = travtime, fill = race_eth)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 1)(travtime)),
             vjust = 0, label.size = 0, fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h", legend.position = "None",
             xlab = "Median travel time to work (minutes)") +
  cmap_fill_race(white = "White", black = "Black", hispanic = "Hispanic", 
                 asian = "Asian", other = "Other") +
  facet_wrap(~tpurp)

finalize_plot(racial_disparities_p2,
              sidebar_width = 0,
              filename = "racial_disparities_p2",
              mode = "png",
              width = 13,
              overwrite = T)

################################################################################
# BACKUP - Median travel times by race and mode
################################################################################

# Filter data
all_time_race_mode_mdt <-
  all_trips_mdt %>% # 12099 records
  
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 & 
    travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,mode_c,tpurp) %>%
  summarize(travtime = as.numeric(MetricsWeighted::weighted_median(travtime_pg_calc, 
                                                                   w = wtperfin))) %>% 
  arrange(tpurp,mode_c)

