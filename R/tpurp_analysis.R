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

tpurp_analysis_base_mdt <-
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

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# ALL
################################################################################

### Calculate proportions for all trip purposes in MDT

detailed_allpurps_mode_c_mdt <-
  pct_calculator(tpurp_analysis_base_mdt %>% filter(geog != "Other") %>% 
                 # Keep only travelers assigned to Chicago, Suburban Cook, or
                 # Other suburban counties (for display purposes)
                 filter(geog %in% c("Chicago","Suburban Cook","Other suburban counties")),
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "wtperfin",
                 survey = "mdt")

# Identify trip purposes by car share
pct_calculator(tpurp_analysis_base_mdt,
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

tpurp_analysis_t1 <-
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
#
# HEALTHCARE
################################################################################

################################################################################
# Chart of health care sub-purposes by mode
################################################################################

tpurp_analysis_p1 <-
  # Get data
  detailed_allpurps_mode_c_mdt %>%
  # Keep only health care
  filter(tpurp == "Health care visit for self") %>% 
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
  
  # Create ggplot object
  ggplot(aes(x = pct, y = str_wrap_factor(geog,18),
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
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

# Find sample sizes
tpurp_analysis_p1_samplesize <-
  detailed_allpurps_mode_c_mdt %>% 
  ungroup() %>% 
  filter(tpurp == "Health care visit for self") %>% 
  select(tpurp,geog,n = total_n) %>% 
  distinct()

# Export plot
finalize_plot(tpurp_analysis_p1,
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
              tpurp_analysis_p1_samplesize %>% 
                filter(geog == "Chicago") %>% 
                select(n),
                     "); 
              <br>- Suburban Cook (",
              tpurp_analysis_p1_samplesize %>% 
                filter(geog == "Suburban Cook") %>% 
                select(n),
                     ");
              <br>- Other suburban counties (",
              tpurp_analysis_p1_samplesize %>% 
                filter(geog == "Other suburban counties") %>% 
                select(n),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. "),
              # width = 8,
              # height = 4.5,
              # sidebar_width = 2.6,
              filename = "tpurp_analysis_p1",
              # mode = c("png","pdf"),
              overwrite = TRUE)


################################################################################
# Backup - health mode share by race and ethnicity
################################################################################

# Calculate health mode share by race and ethnicity
detailed_health_race_mode_c_mdt <-
  pct_calculator(tpurp_analysis_base_mdt,
                 subset = "Health care visit for self",
                 subset_of = "tpurp",
                 breakdown_by = "mode_c",
                 second_breakdown = "race_eth",
                 weight = "wtperfin",
                 survey = "mdt")

# Mode share by race and ethnicity, focusing on transit
detailed_health_race_mode_c_mdt %>% 
  filter(mode_c == "transit") %>% 
  arrange(-pct) 

################################################################################
# Backup - health mode share by household vehicle ownership
################################################################################

# Calculations to understand the relationship between transit use for healthcare
# and household vehicle ownership
health_mode_c_vehs_mdt <-
  pct_calculator(tpurp_analysis_base_mdt %>% 
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

pct_calculator(tpurp_analysis_base_mdt %>% 
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

################################################################################
# Chart of dining sub-purposes by mode
################################################################################

tpurp_analysis_p2 <-
  # Get data
  detailed_allpurps_mode_c_mdt %>%
  # Keep only dining and rename
  mutate(tpurp = recode_factor(tpurp,
                               "Drive thru / take-out dining" = "Drive-thru or take-out",
                               "Ate / dined out" = "Ate or dined out")) %>% 
  filter(tpurp %in% c("Drive-thru or take-out","Ate or dined out")) %>%
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
             # Only label bars that are at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Mode share",
             strip.text = element_text(hjust = 0.5,vjust = 1,face = "bold")) +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent()) +
  
  # Add faceting
  facet_wrap(~geog, ncol = 1)

tpurp_analysis_p2_samplesize <-
  detailed_allpurps_mode_c_mdt %>% 
  filter(tpurp %in% c("Ate / dined out",
                      "Drive thru / take-out dining")) %>% 
  ungroup() %>% 
  select(geog,tpurp,n = total_n) %>% 
  distinct()

finalize_plot(tpurp_analysis_p2,
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
                paste(tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Chicago",tpurp == "Ate / dined out") %>%
                        select(n),
                      tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Suburban Cook",tpurp == "Ate / dined out") %>%
                        select(n),
                      tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Other suburban counties",tpurp == "Ate / dined out") %>%
                        select(n),
                      sep = "/"),
                ");              
              - Drive-thru or take-out (",
                paste(tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Chicago",tpurp == "Drive thru / take-out dining") %>%
                        select(n),
                      tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Suburban Cook",tpurp == "Drive thru / take-out dining") %>%
                        select(n),
                      tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Other suburban counties",tpurp == "Drive thru / take-out dining") %>%
                        select(n),
                      sep = "/"),
                ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "tpurp_analysis_p2",
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
#
# COMMUNITY
################################################################################

################################################################################
# Chart of community sub-purposes by mode
################################################################################

tpurp_analysis_p3 <-
  # Get data
  detailed_allpurps_mode_c_mdt %>% 
  # Recode names
  mutate(tpurp = recode_factor(tpurp,
                               "Socialized with relatives" = "Relatives",
                               "Socialized with friends" = "Friends")) %>% 
  # Keep only the trip types of interest
  filter(tpurp %in% c("Relatives","Friends")) %>% 
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
             strip.text = element_text(hjust = 0.5,vjust = 1,face = "bold")) +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent()) +
  
  # Add faceting
  facet_wrap(~geog,ncol = 1)

tpurp_analysis_p3_samplesize <-
  detailed_allpurps_mode_c_mdt %>%
  ungroup() %>% 
  filter(tpurp %in% c("Socialized with friends",
                      "Socialized with relatives")) %>% 
  select(tpurp,geog,n = total_n) %>% 
  distinct()
  
# Export graphic
finalize_plot(tpurp_analysis_p3,
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
              paste(tpurp_analysis_p3_samplesize %>% 
                      filter(geog == "Chicago",tpurp == "Socialized with friends") %>%
                      select(n),
                    tpurp_analysis_p3_samplesize %>% 
                       filter(geog == "Suburban Cook",tpurp == "Socialized with friends") %>%
                       select(n),
                     tpurp_analysis_p3_samplesize %>% 
                        filter(geog == "Other suburban counties",tpurp == "Socialized with friends") %>%
                        select(n),
                    sep = "/"),
                    ");
              <br>- Relatives (",
              paste(tpurp_analysis_p3_samplesize %>% 
                      filter(geog == "Chicago",tpurp == "Socialized with relatives") %>%
                      select(n),
                    tpurp_analysis_p3_samplesize %>% 
                      filter(geog == "Suburban Cook",tpurp == "Socialized with relatives") %>%
                      select(n),
                    tpurp_analysis_p3_samplesize %>% 
                      filter(geog == "Other suburban counties",tpurp == "Socialized with relatives") %>%
                      select(n),
                    sep = "/"),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              overwrite = T,
              mode = c("png","pdf"),
              filename = "tpurp_analysis_p3")

################################################################################
# Median distances for community trips
################################################################################

# Median distance overall
tpurp_analysis_base_mdt %>%
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         geog != "Other") %>%
  group_by(tpurp) %>%
  summarize(distance = MetricsWeighted::weighted_median(distance_pg,wtperfin),
            n = n(),
            wt = sum(wtperfin)) 

# Median distance and proportion by geography
tpurp_analysis_base_mdt %>%
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
pct_calculator(tpurp_analysis_base_mdt %>% 
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
tpurp_analysis_base_mdt %>% 
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
tpurp_analysis_base_mdt %>% 
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
