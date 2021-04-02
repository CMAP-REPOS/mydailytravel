# This script produces analyses on work trips in the CMAP region.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


### Filter data
all_work_mdt <-
  mdt %>%                            # 125463 records
  filter(
    # Keep only trips of employed respondents (either those who report having 1+
    # jobs or those who report being employed)
    emply_ask == 1 | jobs > 0) %>%   # 83291 records
  filter(
    # Keep only:                     # 83291
    # Those 16 or older
    age >= 16 |
      # or those in an age category from 16 to 44
      (age < 0 & aage %in% c(4,5,6,7))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  # 65848
  # Keep only trips with nonzero distance
  filter(distance_pg > 0) %>%        # 65808
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    # 65775
  # Keep only trips to a fixed work location (we are looking for "commute-type" trips)
  filter(tpurp == "Worked at fixed work location") %>%  # 14160
  # Keep only trips to identified work locations
  filter(loctype == 2) %>%           # 12099
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
#
# Travel times by race
################################################################################

# Filter data
work_time_race_mdt <-
  all_work_mdt %>% # 12099 records
  
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    # travtime_pg_calc < 150 & 
           travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth) %>%
  summarize(travtime = as.numeric(matrixStats::weightedMedian(travtime_pg_calc, w = wtperfin)))

# Chart of travel time to school by household income
work_trips_p1 <-
  # Get data
  work_time_race_mdt %>%
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
                 asian = "Asian", other = "Other")

finalize_plot(work_trips_p1,
              "Black residents of the region have significantly longer commutes to work than those of other residents.",
              "Note: 'Hispanic' includes individuals of any racial group that
              identify as Hispanic. All other categories are non-Hispanic.
              Includes trips by employed residents to a fixed work location.
              Trips with no travel are excluded.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "work_trips_p1",
              # mode = "png",
              # # height = 6.3,
              # width = 11.3,
              overwrite = T)

# Backup - statistics on median travel times by race and mode
# Filter data
work_time_race_mode_mdt <-
  all_work_mdt %>% # 12099 records
  
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    # travtime_pg_calc < 150 & 
    travtime_pg_calc > 0) %>% # 12044 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12012 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,mode_c) %>%
  summarize(travtime = as.numeric(matrixStats::weightedMedian(travtime_pg_calc, w = wtperfin)))
