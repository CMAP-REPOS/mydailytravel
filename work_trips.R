# This script produces analyses on work trips in the CMAP region.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)

source("helper_fns.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

### Filter data
all_work_mdt <-
  mdt %>%                            # 125091 records
  filter(
    # keep only employed respondents (either those who report having 1+ jobs or
    # those who report being employed)
    emply_ask == 1 | jobs > 0, # 17,656 records
    # Keep only:                     #
    # Those 16 or older
    age >= 16 |
      # or those in an age category from 5 to 44
      age < 0 & aage %in% c(4,5,6,7)) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  #
  # Keep only trips with nonzero distance
  filter(distance_pg > 0) %>%        #
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    #
  # Keep only trips to school
  filter(tpurp == "Worked at fixed work location") %>%    #
  # Keep only trips to identified work locations
  filter(loctype == 2) %>%               #
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

## What about travel times by race for school trips

# Filter data for MDT
work_time_race_mdt <-
  all_work_mdt %>%
  filter(
    # Only include trips that are more than 0 minutes and less than 2.5 hours
    travtime_pg_calc < 150 & travtime_pg_calc > 0,
    # Exclude households with missing race and ethnicity information
    race_eth != "missing") %>%
  group_by(race_eth) %>%
  summarize(travtime = as.numeric(weighted.mean(travtime_pg_calc, w = wtperfin))) %>%
  mutate(survey = "My Daily Travel (2019)")

# Chart of travel time to school by household income
work_trips_p1 <-
  work_time_race_mdt %>%
  mutate(label = round(travtime)) %>%
  mutate(race_eth = recode(race_eth,
                           "black" = "Black","white" = "White",
                           "asian" = "Asian","other" = "Other",
                           "hispanic" = "Hispanic")) %>%
  ggplot(aes(x = reorder(race_eth,-travtime), y = travtime, fill = race_eth)) +
  geom_col() +
  theme_cmap(gridlines = "h",
             legend.position = "None") +
  geom_label(aes(label = scales::label_number(accuracy = 1)(travtime)),
             vjust = 0, label.size = 0, fill = "white") +
  cmap_fill_race(white = "White",black = "Black",
                 hispanic = "Hispanic",asian = "Asian",
                 other = "Other")

finalize_plot(work_trips_p1,
              "Average travel time to work by race and ethnicity (minutes).",
              "Note: Includes trips by employed residents to a fixed work location.
              Trips with no travel time or lasting 150 minutes or more are
              excluded as outliers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "work_trips_p1",
              mode = "png",
              # # height = 6.3,
              # width = 11.3,
              overwrite = T)

