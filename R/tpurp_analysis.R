# This script produces charts and graphs to better understand the modes used to
# travel for specific purposes (e.g., how do people get to healthcare). It is
# referenced in Policy Brief #1.

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

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
  # Keep only travelers >= 5 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 5 |                   # 125447
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8))) %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97365
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in TT (great circle vs. actual travel distance).
  # We chose to do this since the published graphics do not involve any
  # comparison between TT and MDT. However, if we instead filter out those trips
  # that have a nonzero haversine distance from MDT, the results are similar.
  filter(distance_pg > 0) %>%        # 97307
  # Exclude trips with no mode
  filter(mode_c != "missing") %>%    # 97270
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
                 # Collar and adjacent (for display purposes)
                 filter(geog %in% c("Chicago","Suburban Cook","Collar and adjacent")),
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 third_breakdown = "geog",
                 weight = "weight",
                 survey = "mdt")

# Identify trip purposes by car share
pct_calculator(tpurp_analysis_base_mdt,
               breakdown_by = "mode_c",
               second_breakdown = "tpurp",
               weight = "weight") %>% 
  mutate(by_car = case_when(
    mode_c %in% c("driver","passenger") ~ "By car",
    TRUE ~ "Not by car")) %>% 
  group_by(by_car,tpurp) %>% 
  summarize(pct = sum(pct)) %>% 
  filter(by_car == "Not by car") %>% 
  arrange(-pct) # %>% View()

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
                                "other" = "All other modes",
                                "bike" = "All other modes")) %>%
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
  scale_x_continuous(labels = scales::label_percent(),
                     expand = expansion(mult = c(.05,0)))

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
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              'By car' includes trips as either a driver of a passenger of a personal 
              vehicle (not including services like taxis or TNCs). Unlabeled bars 
              have less than five 
              percent mode share.
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
              <br>- Collar and adjacent (",
              tpurp_analysis_p1_samplesize %>% 
                filter(geog == "Collar and adjacent") %>% 
                select(n),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. "),
              filename = "tpurp_analysis_p1",
              mode = c("png","pdf"),
              height = 5,
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
                 weight = "weight",
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
                 weight = "weight",
                 survey = "mdt")

# Display outputs
health_mode_c_vehs_mdt %>% arrange(hhveh,pct) %>% 
  View()


################################################################################
# Backup - detail on "other" mode share for healthcare
################################################################################

pct_calculator(tpurp_analysis_base_mdt %>% 
                 filter(tpurp == "Health care visit for self"),
               breakdown_by = "mode",
               weight = "weight",
               survey = "mdt") %>% 
  filter(mode %in% c("paratransit", "private shuttle",
                     "taxi", "private limo", "private car", "rideshare",
                     "shared rideshare", "airplane", "other",
                     "school bus",
                     "personal bike", "bike share")) %>%
  arrange(-pct)

################################################################################
#
# COMMUNITY
################################################################################

################################################################################
# Chart of community sub-purposes by mode
################################################################################

tpurp_analysis_p2 <-
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
                                "other" = "All other modes",
                                "bike" = "All other modes")) %>%
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
             strip.text = element_text(hjust = 0.5,vjust = 1,
                                       family = "Whitney Semibold")) +
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#006b8c")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(),
                     expand = expansion(mult = c(.05,0))) +
  
  # Add faceting
  facet_wrap(~geog,ncol = 1)

tpurp_analysis_p2_samplesize <-
  detailed_allpurps_mode_c_mdt %>%
  ungroup() %>% 
  filter(tpurp %in% c("Socialized with friends",
                      "Socialized with relatives")) %>% 
  select(tpurp,geog,n = total_n) %>% 
  distinct()
  
# Export graphic
finalize_plot(tpurp_analysis_p2,
              "Walking and other non-car modes were significantly more common for 
              trips to socialize with friends than with relatives.",
              paste0(
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              'By car' includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              Unlabeled bars have less than five percent mode share.
              <br><br>
              Sample size (Chicago/Suburban Cook/Collar and adjacent): 
              <br>- Friends (",
              paste(tpurp_analysis_p2_samplesize %>% 
                      filter(geog == "Chicago",tpurp == "Socialized with friends") %>%
                      select(n),
                    tpurp_analysis_p2_samplesize %>% 
                       filter(geog == "Suburban Cook",tpurp == "Socialized with friends") %>%
                       select(n),
                     tpurp_analysis_p2_samplesize %>% 
                        filter(geog == "Collar and adjacent",tpurp == "Socialized with friends") %>%
                        select(n),
                    sep = "/"),
                    ");
              <br>- Relatives (",
              paste(tpurp_analysis_p2_samplesize %>% 
                      filter(geog == "Chicago",tpurp == "Socialized with relatives") %>%
                      select(n),
                    tpurp_analysis_p2_samplesize %>% 
                      filter(geog == "Suburban Cook",tpurp == "Socialized with relatives") %>%
                      select(n),
                    tpurp_analysis_p2_samplesize %>% 
                      filter(geog == "Collar and adjacent",tpurp == "Socialized with relatives") %>%
                      select(n),
                    sep = "/"),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              overwrite = T,
              height = 4.75,
              mode = c("png","pdf"),
              filename = "tpurp_analysis_p2")

################################################################################
# Backup behavior for employed vs. unemployed
################################################################################

community_employed_mdt <-
  pct_calculator(tpurp_analysis_base_mdt %>% filter(geog != "Other") %>% 
                   # Keep only travelers assigned to Chicago, Suburban Cook, or
                   # Collar and adjacent (for display purposes)
                   filter(geog %in% c("Chicago","Suburban Cook","Collar and adjacent"),
                          emply_ask == 1,
                          tpurp %in% c("Socialized with friends","Socialized with relatives")),
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 # third_breakdown = "geog",
                 weight = "weight",
                 survey = "employed")

community_unemployed_mdt <-
  pct_calculator(tpurp_analysis_base_mdt %>% filter(geog != "Other") %>% 
                   # Keep only travelers assigned to Chicago, Suburban Cook, or
                   # Collar and adjacent (for display purposes)
                   filter(geog %in% c("Chicago","Suburban Cook","Collar and adjacent"),
                          emply_ask == 2,
                          tpurp %in% c("Socialized with friends","Socialized with relatives")),
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 # third_breakdown = "geog",
                 weight = "weight",
                 survey = "unemployed")

community_with_employment_status_mdt <-
  rbind(community_employed_mdt,
        community_unemployed_mdt)

community_with_employment_status_mdt %>% 
  # Recode names
  mutate(tpurp = recode_factor(tpurp,
                               "Socialized with relatives" = "Relatives",
                               "Socialized with friends" = "Friends")) %>% 
  # Collapse low-percentage modes
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Transit",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  # Calculate new totals
  group_by(mode_c,tpurp,survey) %>%
  summarize(pct = sum(pct),
            n = sum(breakdown_n))


################################################################################
# Median distances for community trips
################################################################################

# Median distance overall
tpurp_analysis_base_mdt %>%
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         geog != "Other") %>%
  group_by(tpurp) %>%
  summarize(distance = MetricsWeighted::weighted_median(distance_pg,weight),
            n = n(),
            wt = sum(weight)) 

# Median distance and proportion by geography
tpurp_analysis_base_mdt %>%
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives"),
         geog != "Other") %>%
  group_by(tpurp,geog) %>%
  summarize(distance = MetricsWeighted::weighted_median(distance_pg,weight),
            n = n(),
            wt = sum(weight)) %>% 
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
                 filter(geog %in% c("Chicago","Suburban Cook","Collar and adjacent"),
                        tpurp %in% c("Socialized with friends","Socialized with relatives")) %>% 
                 # Recode car vs non-car
                 mutate(by_car = case_when(
                   mode_c %in% c("driver","passenger") ~ "By car",
                   TRUE ~ "Not by car")) %>% 
                 # Optional filter to compare like trips with like
                 filter(distance_pg < 1.5 & distance_pg > 0.5),
               breakdown_by = "by_car",
               second_breakdown = "tpurp",
               weight = "weight",
               survey = "mdt")


################################################################################
# Understanding number of fellow travelers for community trips
################################################################################

# Travel party size
tpurp_analysis_base_mdt %>% 
  filter(tpurp %in% c("Socialized with friends","Socialized with relatives")) %>% 
  group_by(sampno,locno_pg,tpurp,arrtime_pg,start_times_pg) %>% 
  summarize(n = n(),
            wt = sum(weight)) %>% 
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
            wt = sum(weight),
            age = mean(age)) %>% 
group_by(tpurp) %>% 
  mutate(total = sum(wt)) %>% 
  group_by(tpurp,n,total) %>% 
  summarize(breakdown = sum(wt),
            age = weighted.mean(age,wt)) %>% 
  mutate(pct = breakdown / total)  

