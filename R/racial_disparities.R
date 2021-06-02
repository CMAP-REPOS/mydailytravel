# This script produces analyses on racial disparities in trip times in the CMAP
# region.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

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
  summarize(travtime = weighted.mean(travtime_pg_calc, w = weight),
            travtime_uw = mean(travtime_pg_calc),
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
  summarize(travtime = weighted.mean(travtime_pg_calc,w = weight),
            travtime_uw = mean(travtime_pg_calc),
            n = n())

# Combine results
all_time_race_mdt <-
  rbind(work_time_race_mdt,other_time_race_mdt)

# Chart of travel time to school by household income
racial_disparities_p1 <-
  # Get data
  all_time_race_mdt %>%
  # Recode labels for publication
  mutate(race_eth = recode_factor(factor(race_eth,levels = c("black","latino","other","asian","white")),
                           "black" = "Black","latino" = "Latino",
                           "other" = "Other","asian" = "Asian",
                           "white" = "White")) %>%
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
  ggplot(aes(x = race_eth, y = travtime, fill = race_eth)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 1)(travtime)),
             vjust = -.03, label.size = 0, fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h", legend.position = "None",
             xlab = "Mean travel times for various trip purposes (minutes)",
             strip.text = element_text(face = "bold",hjust = 0.5,vjust = 1)) +
  cmap_fill_race(white = "White", black = "Black", hispanic = "Latino", 
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
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties. 
              In all categories, trips with no travel time and those lasting 
              more than two and a half hours are excluded. 
              'Latino' includes respondents who identified as Latino or Hispanic, 
              regardless of racial category. Other categories are non-Latino.
              <br><br>
              Sample size (Black/Latino/Other/Asian/White): 
              <br>- Work (",
                     paste(
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "black") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "latino") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other") %>% select(n) %>% as.numeric(),
                       work_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian") %>% select(n) %>% as.numeric(),
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
                         filter(race_eth == "latino",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other",tpurp == "Health care") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian",tpurp == "Health care") %>% 
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
                         filter(race_eth == "latino",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "other",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "asian",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       other_time_race_mdt %>% ungroup() %>%  
                         filter(race_eth == "white",tpurp == "Shopped (routine like grocery, clothing)") %>% 
                         select(n) %>% as.numeric(),
                       sep = "/"),
                     ").
              <br><br>
              Although sample sizes for 'Other' and 'Asian' health care trips 
              are lower than desired, the estimates are similar for both
              weighted and unweighted totals and are consistent with broader 
              trends.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "racial_disparities_p1",
              mode = c("png","pdf"),
              sidebar_width = 3,
              overwrite = T)
