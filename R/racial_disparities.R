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
    # Keep only:                     # 125447
    # Those 5 or older
    age >= 5 |
      # or those in an age category from 5 to 44
      (age < 0 & aage %in% c(2,3,4,5,6,7)) |
      # Or those enrolled in school grades we know are 5+
      (age < 0 & schol %in% c(4,5,6,7,8))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  # 97635
  # Keep only trips with nonzero distance
  filter(distance_pg > 0) %>%        # 97307
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    # 97270
  # # Code to exclude high and low weight households, by zone.
  # group_by(cluster) %>%
  # arrange(cluster,wtperfin) %>%
  # # Identify the ranked order of this record by weight
  # mutate(rank = row_number()) %>%
  # # Find the total number of records in the cluster
  # mutate(max_rank = max(rank)) %>%
  # # Divide the row's rank by the total
  # mutate(pct_rank = rank / max_rank) %>%
  # ungroup() %>% 
  # # Since there are ties in weights, find the highest and lowest percent for a
  # # given weight
  # group_by(cluster,wtperfin) %>% 
  # mutate(max_wt_pct = max(pct_rank),
  #        min_wt_pct = min(pct_rank)) %>% 
  # filter(min_wt_pct >= 0.05,
  #        max_wt_pct <= 0.95) %>%      # 3179 records
  ungroup()


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
  all_trips_mdt %>%                  # 97270 records
  filter(
    # Keep only trips of employed respondents (either those who report having 1+
    # jobs or those who report being employed)
    emply_ask == 1 | jobs > 0) %>%   # 65775 records
  # Keep only trips to a fixed work location (we are looking for "commute-type"
  # trips)
  filter(tpurp == "Worked at fixed work location") %>%  # 14160
  # Keep only trips to identified work locations
  filter(loctype == 2) %>%           # 12111
  ungroup() %>% 
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 &
    travtime_pg_calc > 0) %>% # 12066 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 12034 records
  
  # Calculate weighted median of trip times by race and ethnicity
  group_by(race_eth,tpurp) %>%
  # # BACKUP - for prose, group by race, ethnicity, and mode
  # group_by(race_eth,tpurp,mode_c) %>% 
  summarize(travtime = weighted.mean(travtime_pg_calc, w = wtperfin),
            n = n())


# Filter data
other_time_race_mdt <-
  all_trips_mdt %>%                  # 97270 records
  filter(tpurp %in% c("Health care visit for self",
                      "Health care visit for someone else",
                      "Visited a person staying at the hospital",
                      "Shopped (routine like grocery, clothing)"
                      )) %>%  # 8769
  # Recode healthcare
  mutate(tpurp = recode_factor(tpurp,
                               "Health care visit for someone else" = "Health care",
                               "Visited a person staying at the hospital" = "Health care",
                               "Health care visit for self" = "Health care")) %>% 
  ungroup() %>% 
  # Only include trips that are more than 0 minutes and less than 2.5 hours
  filter(
    travtime_pg_calc < 150 &
      travtime_pg_calc > 0) %>% # 8732 records
  
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 8693 records
  
  # Calculate weighted mean of trip times by race and ethnicity
  group_by(race_eth,tpurp) %>%
  summarize(travtime = weighted.mean(travtime_pg_calc,w = wtperfin),
            n = n())

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
  mutate(tpurp_disp = 
           recode(factor(tpurp_disp, 
                         levels = c("Worked at fixed work location",
                                    "Health care",
                                    "Shopped (routine like grocery, clothing)")),
                  "Worked at fixed work location" = "Work (fixed location)",
                  "Health care visit for self" = "Health care (personal)",
                  "Shopped (routine like grocery, clothing)" = 
                    "Routine shopping (e.g., groceries or clothing)",)) %>% 
  
  # Create ggplot object
  ggplot(aes(x = reorder(race_eth,-travtime), y = travtime, fill = race_eth)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 1)(travtime)),
             vjust = 0, label.size = 0, fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h", legend.position = "None",
             xlab = "Mean travel times for various trip purposes (minutes)",
             strip.text = element_text(face = "bold",hjust = 0.5,vjust = 1)) +
  cmap_fill_race(white = "White", black = "Black", hispanic = "Hispanic", 
                 asian = "Asian", other = "Other") +
  
  # Adjust axes
  scale_y_continuous(limits = c(0,43)) +
  
  # Add faceting by trip purpose
  facet_wrap(~tpurp_disp,ncol = 1)

# Export finalized graphic
finalize_plot(racial_disparities_p1,
              "Black residents of the region had significantly longer trips to 
              work, health care, and routine shopping than those of 
              other residents.",
              caption = 
              paste0("Note: Includes trips by residents aged 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties. 
              In all categories, trips with no travel time and those lasting 
              more than two and a half hours are excluded. 
              'Hispanic' includes individuals of any racial group that identify 
              as Hispanic. All other categories are non-Hispanic.
              <br><br>
              Sample size (Black/Other/Asian/ Hispanic/White): 
              <br>- Work (",
                     paste(
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "black") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "hispanic") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "white") %>% select(n) %>% as.numeric(),
                       sep = "/"),
                       "); 
              <br>- Health (",
                     paste(
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "black",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "hispanic",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "white",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       sep = "/"),
                     "); 
              <br>- Shop (",
                     paste(
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "black",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "hispanic",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "white",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       sep = "/"),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "racial_disparities_p1",
              mode = c("png","pdf"),
              sidebar_width = 2.5,
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
  summarize(travtime = weighted.mean(travtime_pg_calc, w = wtperfin)) %>% 
  group_by(tpurp) %>% 
  arrange(-travtime) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  arrange(tpurp,rank)

all_time_race_mdt %>% 
  filter(rank == 1) %>% 
  count(race_eth)
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
  summarize(travtime = weighted.mean(travtime_pg_calc, w = wtperfin)) %>% 
  group_by(tpurp,mode_c) %>% 
  arrange(-travtime) %>% 
  mutate(rank = row_number()) %>% 
  ungroup() %>% 
  arrange(tpurp,mode_c,rank)

