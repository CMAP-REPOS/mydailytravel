# This script produces charts and graphs to better understand the modes used to
# travel for specific purposes (e.g., how do people get to healthcare).

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(slider)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


# Create base dataset for mode analyses

modes_of_tpurps_base_mdt <-
  mdt %>%                             # 125463 records
  # Keep only travelers >= 16 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 5 |                   # 108622
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8))) %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 85022
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in TT (great circle vs. actual travel distance).
  # We chose to do this since the published graphics do not involve any
  # comparison between TT and MDT. However, if we instead filter out those trips
  # that have a nonzero haversine distance from MDT, the results are similar.
  filter(distance_pg > 0) %>%        # 84969
  # Exclude trips with no mode
  filter(mode_c != "missing") %>%    # 84932
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  ungroup()

# # Filter data for TT. Note: this code is included but archived because it was
# # not included in publication.
# modes_of_tpurps_base_tt <-
#   tt %>%                             # 139769 records
#   # Keep only records for travelers >= 5 or who we can identify as being >= 5
#   # based on age buckets or school enrollment. Note that 99 is DK/RF for AGE.
#   filter((AGE >= 5 & AGE < 99)|                  # 132680
#            (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
#            (AGE == 99 & AGEB == 2)) %>%
#   # Exclude the first record of the day - this is the beginning record, and does
#   # not represent a trip.
#   filter(PLANO != 1) %>%            # 105568 records
#   # Exclude trips with no travel distance.
#   filter(DIST > 0) %>%              # 100573
#   # Remove missing modes
#   filter(mode_c != "missing") %>%   # 100573
#   # Put school bus back into "other" category
#   mutate(mode_c = as.character(mode_c)) %>%
#   mutate(mode_c = case_when(
#     mode_c == "schoolbus" ~ "other",
#     TRUE ~ mode_c)) %>%
#   mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
#   ungroup()


#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# HEALTHCARE
################################################################################

### Calculate proportions for subcategories for dining in MDT
detailed_health_mode_c_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt %>% 
                   # Keep only travelers assigned to Chicago, Suburban Cook, or
                   # Other suburban counties (for display purposes)
                   filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties")) %>% 
                   # Recode purposes
                   mutate(tpurp = recode_factor(
                     tpurp,
                     "Health care visit for someone else" = "For someone else",
                     "Visited a person staying at the hospital" = "For someone else",
                     "Health care visit for self" = "Personal")),
                 subset = "health",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of healthcare sub-purposes by mode
################################################################################

modes_of_tpurps_p1 <-
  # Get data
  detailed_health_mode_c_mdt %>%
  # Order for graph
  mutate(tpurp = factor(tpurp,levels = c("For someone else",
                                         "Personal"))) %>%
  # Order geographies
  mutate(geog = fct_rev(geog)) %>% 
  # Categorize low-percentage modes into "Other modes"
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "transit" = "Transit",
                                "walk" = "Walking",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  # Calculate new values based on collapsed groups
  group_by(mode_c,tpurp,geog) %>%
  summarize(pct = sum(pct)) %>%
  # Filter out to just personal health care
  filter(tpurp == "Personal") %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(geog,18),
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",legend.max.columns = 4, vline = 0,
             xlab = "Mode share for personal health care trips by home jurisdiction",
             axis.title.x.bottom = element_text(hjust = .8 )) +
  scale_fill_discrete(type = c("#00665c","#6d8692","#36d8ca","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent())


# Export plot
finalize_plot(modes_of_tpurps_p1,
              "Although driving was most common, transit played an 
              important role for personal health care visits, especially for 
              Chicago residents.",
              caption = 
              paste0(
              "Note: Includes trips made by residents aged 5 or older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb.
              'By car' includes trips as either a driver of a passenger of a personal 
              vehicle (not including services like taxis or TNCs). 'Other modes' 
              includes all other modes, but is predominantly composed of 
              paratransit, private shuttles, and personal bicycles. Unlabeled 
              bars have less than five percent mode share.
              <br><br>
              Sample size:
              <br>- Chicago (",
                     detailed_health_mode_c_mdt %>%
                       ungroup() %>% 
                       filter(geog == "Chicago",tpurp == "Personal") %>% 
                       select(total_n) %>% distinct(),
                     "); 
              <br>- Suburban Cook (",
                     detailed_health_mode_c_mdt %>%
                       ungroup() %>% 
                       filter(geog == "Suburban Cook",tpurp == "Personal") %>% 
                       select(total_n) %>% distinct(),
                     ");
              <br>- Other suburban counties (",
                     detailed_health_mode_c_mdt %>%
                       ungroup() %>% 
                       filter(geog == "Other suburban counties",tpurp == "Personal") %>% 
                       select(total_n) %>% distinct(),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. "),
              # width = 8,
              # height = 4.5,
              # sidebar_width = 2.6,
              filename = "modes_of_tpurps_p1",
              mode = "png",
              overwrite = TRUE)


################################################################################
# Backup - health mode share by race and ethnicity
################################################################################

# Calculate health mode share by race and ethnicity
detailed_health_race_mode_c_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt,
                 subset = "Health care visit for self",
                 subset_of = "tpurp",
                 breakdown_by = "mode_c",
                 second_breakdown = "race_eth",
                 weight = "wtperfin",
                 survey = "mdt")

# Breakdown by race and ethnicity
modes_of_tpurps_p1a <-
  # Get data
  detailed_health_race_mode_c_mdt %>%
  # Exclude those without race and ethnicity
  filter(race_eth != "missing") %>%
  # Categorize low-percentage modes into "Other modes"
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "transit" = "Transit",
                                "walk" = "Other modes",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  # Calculate new values based on collapsed groups
  group_by(mode_c,race_eth) %>%
  summarize(pct = sum(pct)) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = race_eth,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",legend.max.columns = 3, vline = 0,
             xlab = "Mode share") +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent())

# Generate finalized graphic
finalize_plot(modes_of_tpurps_p1a,
              "Mode share of health trips by race and ethnicity, 2019.",
              "Note: 'By car' includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              'Health care visit for someone else' includes the small number of 
              trips that were recorded as visiting another person in the 
              hospital; both categories had very similar modal splits. Unlabeled 
              bars have less than five percent mode share.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. ",
              # width = 11.3,
              # height = 6.3,
              filename = "modes_of_tpurps_p1a",
              # mode = "png",
              overwrite = TRUE)


################################################################################
# Backup - health mode share by household vehicle ownership
################################################################################

# Calculations to understand the relationship between transit use for healthcare
# and household vehicle ownership
health_mode_c_vehs_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt %>% 
                   mutate(hhveh = case_when(
                     hhveh == 0 ~ 0,
                     hhveh == 1 ~ 1,
                     TRUE ~ 2
                   )),
                 subset = "Health care visit for self",
                 subset_of = "tpurp",
                 breakdown_by = "hhveh",
                 second_breakdown = "mode_c",
                 # # Note that you can instead break down by mode to see more
                 # # nuanced patterns; sample size is a constraint. Paratransit
                 # # has a particularly high 0-vehicle household, but the sample
                 # # is very small. 
                 # second_breakdown = "mode",
                 weight = "wtperfin",
                 survey = "mdt")

# Display outputs
health_mode_c_vehs_mdt %>% arrange(hhveh,pct) %>% 
  View()


################################################################################
# Backup - detail on "other" mode share for Chicago healthcare
################################################################################

pct_calculator(modes_of_tpurps_base_mdt %>% 
                 filter(geog %in% c("Chicago","Suburban Cook"),
                        tpurp == "Health care visit for self"),
               breakdown_by = "mode",
               second_breakdown = "geog",
               weight = "wtperfin",
               survey = "mdt") %>% 
  filter(mode %in% c("paratransit", "private shuttle",
                     "taxi", "private limo", "private car", "rideshare",
                     "shared rideshare", "airplane", "other",
                     "school bus",
                     "personal bike", "bike share")) %>%
  arrange(geog,-pct)


################################################################################
#
# DINING
################################################################################

### Calculate proportions for subcategories for dining in MDT
detailed_dining_mode_c_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt %>% filter(geog != "Other") %>% 
                   # Keep only travelers assigned to Chicago, Suburban Cook, or
                   # Rest of region
                   filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties")) %>% 
                   mutate(tpurp = recode_factor(tpurp,
                                                "Ate / dined out" = "Ate or dined out",
                                                "Drive thru / take-out dining" = "Drive-thru or take-out")),
                 subset = "dining",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of dining sub-purposes by mode
################################################################################

modes_of_tpurps_p2 <-
  # Get data
  detailed_dining_mode_c_mdt %>%
  # Reformat for publication
  mutate(tpurp = factor(tpurp,levels = c("Drive-thru or take-out",
                                         "Ate or dined out"))) %>%
  # Categorize low-percentage modes into "Other modes"
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Transit",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  
  # Calculate summary based on new mode breakdown
  group_by(mode_c,tpurp,geog) %>%
  summarize(pct = sum(pct)) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(tpurp,17),
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Mode share",
             strip.text = element_text(hjust = 0.5,face = "bold")) +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent()) +
  
  # Add faceting
  facet_wrap(~geog, ncol = 1)

finalize_plot(modes_of_tpurps_p2,
              "Walking and transit were more important modes for eating in person 
              than for picking up take-out.",
              caption = 
              paste0(
                "Note: Includes trips made by residents aged 5 or older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb.
              'By car' includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              'Other modes' includes transit, biking, and all other modes.
              Unlabeled bars have less than 5 percent mode share.
              <br><br>
              Sample size (Chicago/Suburban Cook/Other suburban counties):
              <br>- Ate or dined out (",
                     paste(detailed_dining_mode_c_mdt %>%
                             ungroup() %>% 
                             filter(geog == "Chicago",tpurp == "Ate or dined out") %>%
                             select(total_n) %>% distinct(),
                           detailed_dining_mode_c_mdt %>%
                             ungroup() %>% 
                             filter(geog == "Suburban Cook",tpurp == "Ate or dined out") %>%
                             select(total_n) %>% distinct(),
                           detailed_dining_mode_c_mdt %>%
                             ungroup() %>% 
                             filter(geog == "Other suburban counties",tpurp == "Ate or dined out") %>%
                             select(total_n) %>% distinct(),
                           sep = "/"),
                           ");              
              - Drive-thru or take-out (",
              paste(detailed_dining_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Chicago",tpurp == "Drive-thru or take-out") %>%
                      select(total_n) %>% distinct(),
                    detailed_dining_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Suburban Cook",tpurp == "Drive-thru or take-out") %>%
                      select(total_n) %>% distinct(),
                    detailed_dining_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Other suburban counties",tpurp == "Drive-thru or take-out") %>%
                      select(total_n) %>% distinct(),
                    sep = "/"),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              # width = 11.3,
              # height = 6.3,
              # sidebar_width = 2.6,
              filename = "modes_of_tpurps_p2",
              mode = "png",
              overwrite = T)


################################################################################
#
# COMMUNITY
################################################################################

### Calculate proportions for subcategories for community in MDT

detailed_community_mode_c_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt %>% 
                   # Keep only travelers assigned to Chicago, Suburban Cook, or
                   # Rest of region
                   filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties")) %>% 
                   mutate(tpurp = recode_factor(tpurp,
                                                "Socialized with friends" = "Friends",
                                                "Socialized with relatives" = "Relatives")),
                 # Optional filter to compare like trips with like
                 # %>% filter(distance_pg < 1.25 & distance_pg > 0.75),
                 subset = "community",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of community sub-purposes by mode
################################################################################

modes_of_tpurps_p3 <-
  # Get data
  detailed_community_mode_c_mdt %>%
  # Keep only the trip types of interest
  filter(tpurp %in% c("Relatives","Friends")) %>% 
  # Order factors
  mutate(tpurp = factor(tpurp,
                        levels = c("Relatives",
                                   "Friends"
                                   ))) %>%
  # Collapse low-percentage modes
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Transit",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  # Calculate new totals
  group_by(mode_c,tpurp,geog) %>%
  summarize(pct = sum(pct)) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(tpurp,15),
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",legend.max.columns = 4, vline = 0,
             xlab = "Mode share for socializing trips by home jurisdiction",
             strip.text = element_text(hjust = 0.5,face = "bold")) +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent()) +
  
  # Add faceting
  facet_wrap(~geog,ncol = 1)

# Export graphic
finalize_plot(modes_of_tpurps_p3,
              "Walking and other non-car modes were significantly more common for 
              trips to socialize with friends than with relatives.",
              paste0(
              "Note: Includes trips made by residents aged 5 or older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb.              
              'By car' includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              <br><br>
              Sample size (Chicago/Suburban Cook/Other suburban counties): 
              <br>- Friends (",
              paste(detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Chicago",tpurp == "Friends") %>%
                      select(total_n) %>% distinct(),
                    detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Suburban Cook",tpurp == "Friends") %>%
                      select(total_n) %>% distinct(),
                    detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Other suburban counties",tpurp == "Friends") %>%
                      select(total_n) %>% distinct(),
                    sep = "/"),");
              <br>- Relatives (",
              paste(detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Chicago",tpurp == "Relatives") %>%
                      select(total_n) %>% distinct(),
                    detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Suburban Cook",tpurp == "Relatives") %>%
                      select(total_n) %>% distinct(),
                    detailed_community_mode_c_mdt %>%
                      ungroup() %>% 
                      filter(geog == "Other suburban counties",tpurp == "Relatives") %>%
                      select(total_n) %>% distinct(),
                    sep = "/"),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              # width = 6.5,
              # height = 4,
              # sidebar_width = 2.6,
              overwrite = T,
              mode = "png",
              filename = "modes_of_tpurps_p3")

################################################################################
# Median distances for community trips
################################################################################

# Median distance overall
  modes_of_tpurps_base_mdt %>%
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         geog != "Other") %>%
  group_by(tpurp) %>%
  summarize(distance = MetricsWeighted::weighted_median(distance_pg,wtperfin),
            n = n(),
            wt = sum(wtperfin)) 

# Median distance and proportion by geography
modes_of_tpurps_base_mdt %>%
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         geog != "Other") %>%
  group_by(tpurp,geog) %>%
  summarize(distance = MetricsWeighted::weighted_median(distance_pg,wtperfin),
            n = n(),
            wt = sum(wtperfin)) %>% 
  ungroup() %>% 
  group_by(tpurp) %>% 
  mutate(total = sum(wt),
         share = wt/total)


################################################################################
# Mode share for similar distances
################################################################################
pct_calculator(modes_of_tpurps_base_mdt %>% 
                 # Keep only travelers assigned to Chicago, Suburban Cook, or
                 # Rest of region
                 filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties"),
                        tpurp %in% c("Socialized with friends","Socialized with relatives")) %>% 
                 # Recode car vs non-car
                 mutate(by_car = case_when(
                   mode_c %in% c("driver","passenger") ~ "By car",
                   TRUE ~ "Not by car")) %>% 
                 # Optional filter to compare like trips with like
                 filter(distance_pg < 1.5 & distance_pg > 0.5),
               breakdown_by = "by_car",
               second_breakdown = "tpurp",
               weight = "wtperfin",
               survey = "mdt")


################################################################################
# Understanding number of fellow travelers for community trips
################################################################################

# Travel party size
modes_of_tpurps_base_mdt %>% 
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives")) %>% 
  group_by(sampno,locno_pg,tpurp,arrtime_pg,start_times_pg) %>% 
  summarize(n = n(),
            wt = sum(wtperfin)) %>% 
  group_by(tpurp) %>% 
  mutate(total = sum(wt)) %>% 
  group_by(tpurp,n,total) %>% 
  summarize(breakdown = sum(wt)) %>% 
  mutate(pct = breakdown / total)

# Travel party size and age
modes_of_tpurps_base_mdt %>% 
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         age > 0) %>% 
  group_by(sampno,locno_pg,tpurp,arrtime_pg,start_times_pg) %>% 
  summarize(n = n(),
            wt = sum(wtperfin),
            age = mean(age)) %>% 
group_by(tpurp) %>% 
  mutate(total = sum(wt)) %>% 
  group_by(tpurp,n,total) %>% 
  summarize(breakdown = sum(wt),
            age = weighted.mean(age,wt)) %>% 
  mutate(pct = breakdown / total)  

################################################################################
#
# ALL
################################################################################

### Calculate proportions for all trip purposes in MDT

detailed_allpurps_mode_c_mdt <-
  pct_calculator(modes_of_tpurps_base_mdt %>% filter(geog != "Other"),
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "wtperfin",
                 survey = "mdt")

# Identify trip purposes by car share
pct_calculator(modes_of_tpurps_base_mdt,
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 weight = "wtperfin") %>% 
  mutate(by_car = case_when(
    mode_c %in% c("driver","passenger") ~ "By car",
    TRUE ~ "Not by car")) %>% 
  group_by(by_car,tpurp) %>% 
  summarize(pct = sum(pct)) %>% 
  filter(by_car == "Not by car") %>% 
  arrange(-pct)

################################################################################
# Table of all trips by mode
################################################################################

modes_of_tpurps_t1 <-
  # Get data
  detailed_allpurps_mode_c_mdt %>%
  # Collapse low-percentage modes
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Transit",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  # Calculate new totals
  group_by(mode_c,tpurp,geog) %>%
  summarize(pct = sum(pct),
            n = first(total_n)) %>%
  pivot_wider(id_cols = c(tpurp,n,geog), values_from = pct, names_from = mode_c)

################################################################################
# ARCHIVE
#
################################################################################

# ################################################################################
# #
# # ARCHIVE - SHOPPING/ERRANDS
# ################################################################################
# 
# ### Calculate proportions for subcategories for shopping/errands in MDT
# 
# detailed_errands_mode_c_mdt <-
#   pct_calculator(modes_of_tpurps_base_mdt,
#                  subset = "shopping/errands",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  second_breakdown = "tpurp",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# ################################################################################
# # Chart of shopping/errands trips by mode
# ################################################################################
# 
# modes_of_tpurps_p4 <-
#   # Get data
#   detailed_errands_mode_c_mdt %>%
#   # Order factors
#   mutate(tpurp = factor(tpurp,
#                         levels = c("Serviced a vehicle (purchased gas, regular maintenance)",
#                                    "Drive-thru errands (ATM, dry cleaning, pharmacy, etc.)",
#                                    "Shopped (non-routine like for appliances, cars, home furnishings)",
#                                    "Shopped (routine like grocery, clothing)",
#                                    "Non-shopping errands (banking, post office, government, etc.)"
#                         ))) %>%
#   # Collapse low-percentage modes
#   mutate(mode_c = recode_factor(mode_c,
#                                 "driver" = "By car",
#                                 "passenger" = "By car",
#                                 "walk" = "Walking",
#                                 "transit" = "Transit",
#                                 "other" = "Other modes",
#                                 "bike" = "Other modes")) %>%
#   # Calculate new totals
#   group_by(mode_c,tpurp) %>%
#   summarize(pct = sum(pct)) %>%
# 
#   # Create ggplot object
#   ggplot(aes(x = pct, y = str_wrap_factor(tpurp,20),
#              # Only label bars that round to at least 5 percent
#              label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
#   geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
#   geom_text(position = position_stack(vjust = 0.5),
#             color = "white") +
# 
#   # Add CMAP style
#   theme_cmap(gridlines = "v",legend.max.columns = 4, vline = 0) +
#   scale_fill_discrete(type = c("#00665c","#3f0030","#36d8ca","#006b8c")) +
# 
#   # Adjust axis
#   scale_x_continuous(labels = scales::label_percent())
# 
# finalize_plot(modes_of_tpurps_p4,
#               "Mode share of shopping and errands trips, 2019.",
#               "Note: 'By car' includes trips as either a driver of a passenger
#               of a personal vehicle (not including services like taxis or
#               ride-share). 'Other modes' includes biking, ride-share, and all
#               other modes. Unlabeled bars have less than 5 percent mode share.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel data. ",
#               # width = 6.5,
#               # height = 4,
#               overwrite = T,
#               # mode = "png",
#               filename = "modes_of_tpurps_p4")
# 
# ################################################################################
# #
# # ARCHIVE - RECREATION/FITNESS
# ################################################################################
# 
# ### Calculate proportions for subcategories for recreation/fitness in MDT
# 
# detailed_recreation_mode_c_mdt <-
#   pct_calculator(modes_of_tpurps_base_mdt %>% filter(geog != "Other"),
#                  subset = "recreation/fitness",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  second_breakdown = "tpurp",
#                  third_breakdown = "geog",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# ################################################################################
# # Chart of recreation/fitness trips by mode
# ################################################################################
# 
# modes_of_tpurps_p5 <-
#   # Get data
#   detailed_recreation_mode_c_mdt %>%
#   # Order factors
#   mutate(tpurp = factor(tpurp,
#                         levels = c("Went to the gym",
#                                    "Other recreation",
#                                    "Exercised outdoors"
#                         ))) %>%
#   # Collapse low-percentage modes
#   mutate(mode_c = recode_factor(mode_c,
#                                 "driver" = "By car",
#                                 "passenger" = "By car",
#                                 "walk" = "Walking",
#                                 "transit" = "Transit",
#                                 "other" = "Other modes",
#                                 "bike" = "Other modes")) %>%
#   # Calculate new totals
#   group_by(mode_c,tpurp,geog) %>%
#   summarize(pct = sum(pct)) %>%
# 
#   # Create ggplot object
#   ggplot(aes(x = pct, y = str_wrap_factor(tpurp,18),
#              # Only label bars that round to at least 5 percent
#              label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
#   geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
#   geom_text(position = position_stack(vjust = 0.5),
#             color = "white") +
# 
#   # Add CMAP style
#   theme_cmap(gridlines = "v",legend.max.columns = 4, vline = 0,
#              xlab = "Mode share by home jurisdiction") +
#   scale_fill_discrete(type = c("#00665c","#3f0030","#36d8ca","#006b8c")) +
# 
#   # Adjust axis
#   scale_x_continuous(labels = scales::label_percent()) +
# 
#   # Add faceting
#   facet_wrap(~geog,ncol = 1)
# 
# finalize_plot(modes_of_tpurps_p5,
#               "Mode share of recreation and fitness trips, 2019.",
#               "Note: 'By car' includes trips as either a driver of a passenger
#               of a personal vehicle (not including services like taxis or
#               ride-share). 'Other modes' includes biking, ride-share, and all
#               other modes. Unlabeled bars have less than 5 percent mode share.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel data. ",
#               # width = 6.5,
#               # height = 4,
#               overwrite = T,
#               # mode = "png",
#               filename = "modes_of_tpurps_p5")
# 
# 
# ################################################################################
# # Archive - TT vs. MDT comparison of dining sub-purposes by mode
# ################################################################################
# 
# ## Create totals for trips by mode category (within universe of dining trips)
# 
# ### Calculate proportions for TT
# all_dining_mode_c_tt <-
#   pct_calculator(tt_base_2,
#                  subset = "dining",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
# 
# ### Calculate proportions for MDT
# all_dining_mode_c_mdt <-
#   pct_calculator(mdt_base_2,
#                  subset = "dining",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# ### Join MDT and TT
# total_dining_mode_c <-
#   rbind(all_dining_mode_c_tt,
#         all_dining_mode_c_mdt) %>%
#   mutate(tpurp = "Dining outside of home (all)") %>%
#   select(-total)
# 
# all_dining_mode_c <-
#   rbind(total_dining_mode_c,
#         detailed_dining_mode_c_mdt)
# 
# modes_of_tpurps_p1a <-
#   all_dining_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Dining outside of home (all)",
#                                          "Drive thru / take-out dining",
#                                          "Ate / dined out")),
#          survey = factor(survey, levels = c("tt","mdt"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-=pct)), x = =pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(~survey,ncol = 1) +
#   theme_cmap(gridlines = "v") +
#   scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
#   cmap_fill_discrete(palette = "friday")
# 
# finalize_plot(modes_of_tpurps_p1a,
#               "Mode share of dining trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               width = 11.3,
#               height = 6.3,
#               filename = "modes_of_tpurps_p1a",
#               mode = "png")
# 
# ################################################################################
# # Archive - TT vs. MDT comparison of healthcare sub-purposes by mode
# ################################################################################
# 
# ## Create totals for trips by mode category (within universe of health trips)
# 
# ### Calculate proportions for TT
# all_health_mode_c_tt <-
#   pct_calculator(modes_of_tpurps_base_tt,
#                  subset = "health",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
# 
# ### Calculate proportions for MDT
# all_health_mode_c_mdt <-
#   pct_calculator(modes_of_tpurps_base_mdt,
#                  subset = "health",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# ### Join MDT and TT
# total_health_mode_c <-
#   rbind(all_health_mode_c_tt,
#         all_health_mode_c_mdt) %>%
#   mutate(tpurp = "Healthcare (all)") %>%
#   select(-total)
# 
# all_health_mode_c <-
#   rbind(total_health_mode_c,
#         detailed_health_mode_c_mdt)
# 
# modes_of_tpurps_p2a <-
#   all_health_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Healthcare (all)",
#                                          "Health care visit for self",
#                                          "Health care visit for someone else",
#                                          "Visited a person staying at the hospital")),
#          survey = factor(survey, levels = c("tt","mdt"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(~survey,ncol = 1) +
#   theme_cmap(gridlines = "v",legend.max.columns = 3) +
#   scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
#   cmap_fill_discrete(palette = "governance")
# 
# finalize_plot(modes_of_tpurps_p2a,
#               "Mode share of health trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               width = 11.3,
#               height = 6.3,
#               filename = "modes_of_tpurps_p2a",
#               mode = "png",
#               overwrite = TRUE)
# 
# ################################################################################
# # Archive - TT vs. MDT comparison of community sub-purposes by mode
# ################################################################################
# 
# 
# # Create totals for trips by mode category (within universe of community trips)
# 
# ### Calculate proportions for TT
# all_community_mode_c_tt <-
#   pct_calculator(modes_of_tpurps_base_tt,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
# 
# ### Calculate proportions for MDT
# all_community_mode_c_mdt <-
#   pct_calculator(modes_of_tpurps_base_mdt,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# ### Join MDT and TT
# total_community_mode_c <-
#   rbind(all_community_mode_c_tt,
#         all_community_mode_c_mdt) %>%
#   mutate(tpurp = "Community (all)")
# 
# ### Calculate proportions for subcategories for community in TT
# 
# detailed_community_mode_c_tt <-
#   pct_calculator(modes_of_tpurps_base_tt,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  second_breakdown = "tpurp",
#                  weight = "weight",
#                  survey = "tt")
# 
# 
# all_community_mode_c <-
#   rbind(total_community_mode_c,
#         detailed_community_mode_c_mdt,
#         detailed_community_mode_c_tt)
# 
# modes_of_tpurps_p3a <-
#   all_community_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Community (all)",
#                                          "Visit friends/relatives",
#                                          "Socialized with friends",
#                                          "Socialized with relatives",
#                                          "Civic/religious activities",
#                                          "Attended a community event",
#                                          "Attended a religious event")),
#          survey = factor(survey, levels = c("tt","mdt")),
#          category = recode_factor(tpurp,
#                                   "Community (all)" = "Overall",
#                                   "Visit friends/relatives" = "Friends/Family",
#                                   "Socialized with friends" = "Friends/Family",
#                                   "Socialized with relatives" = "Friends/Family",
#                                   "Civic/religious activities" = "Civic/Religious",
#                                   "Attended a community event" = "Civic/Religious",
#                                   "Attended a religious event" = "Civic/Religious"
# 
#          )) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel",
#                                 tt = "Travel Tracker")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-pct)), x = pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(survey~category) +
#   theme_cmap(gridlines = "v",legend.max.columns = 3) +
#   scale_x_continuous(labels = scales::label_percent()) +
#   cmap_fill_discrete(palette = "environment")
# 
# finalize_plot(modes_of_tpurps_p3a,
#               "Mode share of community trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               title_width = 1.8,
#               width = 11.3,
#               height = 6.3,
#               overwrite = T,
#               filename = "modes_of_tpurps_p3a",
#               mode = "png")
# 
# ################################################################################
# # Examination of TNC school trips - not enough records for rigorous analysis
# ################################################################################
# 
# ### Filter data
# all_tnc_school_mdt <-
#   modes_of_tpurps_base_mdt %>%                         # 96,821 records
#   filter(age <= 18,                      # 15,495 records
#          schol %in% c(3,4),              # 13,879 records
#          mode %in% c("rideshare",
#                      "shared rideshare",
#                      "taxi"),            # 31 records
#          tpurp_c == "school")            # 11 records
# 
# all_tnc_school_mdt %>%
#   group_by(income_c, mode) %>%
#   summarize(trips = sum(wtperfin))
