# This script analyzes TNC usage in MDT, using a combination of travel diary and
# survey entries

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(ggplot2)
library(cmapplot)
library(ggpattern)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")

# Calculate the number of trips taken by travelers on active modes like transit,
# biking, and walking, as well as TNC trips
active_travel <-
  mdt %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97374
  # Exclude trips with no travel distance.
  filter(distance_pg > 0) %>%        # 97316
  # Group data by the traveler level
  group_by(sampno,perno) %>%
  # Create flags for transit, biking, walking, and tnc trips
  summarize(takes_transit = sum(mode_c == "transit",na.rm = TRUE),
            takes_bike = sum(mode_c == "bike",na.rm = TRUE),
            takes_walk = sum(mode_c == "walk",na.rm = TRUE),
            takes_tnc = sum((mode == "rideshare") | 
                              (mode == "shared rideshare"),na.rm = TRUE)) %>%
  # Create a flag that is 1 if any of transit, biking, or walking is nonzero,
  # and zero otherwise
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))

# Age bins
age_breaks <- c(-1, 9, 18, 29, 39, 49, 59, 69, 150)
age_labels <- c("5 to 9", "10 to 18", "19 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 and above")

# Create working dataset based on variables pulled above and person/hh
# characteristics from other tables
tnc <-
  mdt_all_respondents %>%
  # Keep only travelers older than 18 (they are the ones that answered TNC survey
  # questions)
  filter(age > 18 |
           (age < 0 & aage %in% c(5,6,7)) |
           (age < 0 & age18 == 1)) %>% 
  # Add information on active travel from above
  left_join(active_travel, by = c("sampno","perno")) %>%
  # Add 0s for people with no trips (who otherwise are NAs)
  mutate(across(starts_with("takes_"),~replace(.,is.na(.),0))) %>%
  # Create dummy variables for analysis
  mutate(n = 1,
         county = as.character(home_county),
         white = case_when(
           race_eth == "white" ~ 1,
           TRUE ~ 0
         ),
         black = case_when(
           race_eth == "black" ~ 1,
           TRUE ~ 0
         ),
         asian = case_when(
           race_eth == "asian" ~ 1,
           TRUE ~ 0
         ),
         hispa = case_when(
           race_eth == "hispanic" ~ 1,
           TRUE ~ 0
         ),
         other = case_when(
           race_eth == "other" ~ 1,
           TRUE ~ 0
         ),
         high_income = case_when(
           income_c %in% c("high","middle-high") ~ 1,
           TRUE ~ 0
         )) %>% 
  mutate(age_bin = cut(age, breaks = age_breaks,
                       labels = age_labels))

# Reshape data to add flags for each county as individual columns
tnc_wide <- tnc %>%
  pivot_wider(
    names_from = county,
    id_cols = c(sampno:takes_active,white:age_bin),
    values_from = n,
    names_prefix = 'county_',
    values_fill = list(n = 0))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
# Regression
################################################################################

# Run linear regression
tnc_use_lm <-
  # Get data
  tnc_wide %>%
  # Filter out respondents who don't have an answer for...
    # TNC use
    filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
    # Household incomes
    filter(income_c != "missing") %>%
    # Household vehicles
    filter(!(hhveh %in% c(-9,-8,-7))) %>% 
    # Trips
    filter(pertrips != -1) %>% 
    # Race and ethnicity
    filter(race_eth != "missing") %>% 
    # Smartphone
    filter(!(smrtphn %in% c(-9,-8,-7,-1))) %>%
  # Recode smartphone to be a dummy with 1 as "yes" and 0 as "no" (previously
  # "no" was coded as 2)
  mutate(smrtphn = recode(smrtphn,
                          "1" = "1",
                          "2" = "0")) %>% 
  
  # Execute linear regression
  lm(tnc_use ~
       takes_transit + # How many times did they take transit on their travel day?
       takes_bike + # How many times did they bike on their travel day?
       takes_walk + # How many times did they walk on their travel day?
       county_31 + # Is their home in Cook County?
       high_income + # Is their household income above $60,000/year?
       hhveh + # How many household vehicles do they have?
       smrtphn + # Do they have a smartphone?
       pertrips + # How many trips did they take on their travel day?
       # Are they: 
       white + # White (non-Hispanic)?
       black + # Black?
       hispa + # Hispanic (of any race)?
       asian, # Asian?
      .,
      # We use the weights to better represent the regional trends (vs. raw
      # responses)
      weights = wtperfin
     )

summary(tnc_use_lm)

## There appears to be a positive correlation with TNC usage and transit use


################################################################################
# 
# Statistics about usage, cost, and purpose by age, race/ethnicity, and home 
################################################################################

# Helper for month conversions (from weeks)
w_to_m <- 
  (365/7) / # Number of weeks in a year
  12 # Divided by the number of months in a year

overall_usage <-
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>% 
  summarize(tnc_use = w_to_m*weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

overall_cost <-
  tnc_wide %>%
  filter(tnc_cost > 0) %>% 
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

################################################################################
# PLOT OF AGE CHARACTERISTICS | USAGE AND COST
################################################################################

## Look at usage by age - turned into a monthly figure
age_usage <-
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1)),
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_use = w_to_m*weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Cost by age
age_cost <- 
  tnc_wide %>%
  filter(tnc_cost > 0,
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

# Combine age data for cost and usage
age_cost_and_usage <-
  left_join(age_usage %>% select(-n),
            age_cost %>% select(-n),
            by = "age_bin") %>% 
  rbind(
    left_join(overall_cost %>% select(-n) %>% mutate(age_bin = "CMAP region"),
              overall_usage %>% select(-n) %>%  mutate(age_bin = "CMAP region"),
              by = "age_bin")) %>% 
  pivot_longer(cols = c("tnc_use","tnc_cost")) %>% 
  # Adjust factor ordering for CMAP region
  mutate(age_bin = relevel(age_bin,"CMAP region"))

# Generate output chart for age
tnc_p1 <-
  # Get data
  age_cost_and_usage %>% 
  # Reformat
  mutate(name = recode(factor(name,levels = c("tnc_use","tnc_cost")),
                       "tnc_cost" = "Average cost per trip",
                       "tnc_use" = "Average monthly trips")) %>% 
  mutate(blank = case_when(
    name == "Average cost per trip" ~ 20.5,
    TRUE ~ 4
  )) %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = factor(age_bin,levels = rev(levels(age_bin))), fill = age_bin)) +
  geom_col() +
  geom_label(aes(label = ifelse(name == "Average monthly trips",
                                scales::label_number(accuracy = 0.1)(value),
                                scales::label_dollar(accuracy = 1)(value))),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  geom_blank(aes(x = blank)) +
  facet_wrap(~name, scales = "free_x") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",hline = 0,legend.position = "none",
             panel.spacing.x = unit(30,"bigpts"),
             xlab = "TNC usage characteristics by age") +
  cmap_fill_highlight(age_cost_and_usage$age_bin,"CMAP region")

finalize_plot(tnc_p1,
              "Usage of Transportation Network Companies (TNCs) decreases by age, 
              while the average cost per trip increases with age.",
              "Note: Monthly TNC usage was extrapolated from responses to a 
              question that asked how frequently the respondent used Uber, Lyft, 
              or Via in the prior week. Average values from that question were 
              multiplied by approximately 4.3 to yield a monthly figure. 
              Excludes travelers 18 and younger, as they were not asked about 
              TNC usage.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "tnc_p1",
              # width = 8,
              # height = 4.5,
              # sidebar_width = 2.3,
              mode = "png",
              overwrite = T)

################################################################################
# PLOT OF HOME JURISDICTION USAGE
################################################################################

# Usage by home county - monthly
home_usage <- 
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county_chi) %>%
  summarize(tnc_use = w_to_m*weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>% 
  rbind(overall_usage %>% mutate(home_county_chi = "CMAP region"))

# Generate output chart for age
tnc_p2 <-
  # Get data
  home_usage %>% 
  
  # Create ggplot object
  ggplot(aes(x = tnc_use, y = reorder(home_county_chi,tnc_use), fill = home_county_chi)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(tnc_use)),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,legend.position = "none",
             xlab = "TNC usage per month by home jurisdiction") +
  cmap_fill_highlight(home_usage$home_county_chi,"CMAP region") +

  # Adjust axes
  scale_x_continuous(limits = c(0,3.5))

finalize_plot(tnc_p2,
              "Usage of Transportation Network Companies (TNCs) is greatest in 
              Chicago and Suburban Cook County.",
              "Note: Monthly TNC usage was extrapolated from responses to a 
              question that asked how frequently the respondent used Uber, Lyft, 
              or Via in the prior week. Average values from that question were 
              multiplied by approximately 4.3 to yield a monthly figure. 
              Excludes travelers 18 and younger, as they were not asked about 
              TNC usage.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "tnc_p2",
              mode = "png",
              overwrite = T)

################################################################################
# OTHER CHARACTERISTICS
################################################################################

# Usage by race and ethnicity - turned into a monthly figure
tnc_wide %>%
  filter(race_eth != "missing",
         !(tnc_use %in% c(-9,-8,-7,-1))) %>%
  group_by(race_eth) %>%
  summarize(tnc_use = w_to_m*weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Usage by home county - monthly
tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county_chi) %>%
  summarize(tnc_use = w_to_m*weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_use)

# Cost by home county
tnc_wide %>%
  filter(tnc_cost>0) %>%
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_cost)

################################################################################
# PLOT OF AGE | PURPOSE
################################################################################

# Age bins
age_breaks_large <- c(-1, 9, 18, 39, 59, 150)
age_labels_large <- c("5 to 9", "10 to 18", "19 to 39", "40 to 59",
                      "60 and above")

tnc_for_purposes <-
  tnc %>% 
  mutate(age_bin = cut(age, breaks = age_breaks_large,
                       labels = age_labels_large)) %>% 
  filter(tnc_purp > 0) %>%
  mutate(tnc_purp = recode(factor(tnc_purp,levels = c(1,2,3,5,4)),
                           "1" = "Commute (whole or part)",
                           "2" = "Commute (whole or part)",
                           "3" = "Daytime (work)",
                           "4" = "Daytime (non-work)",
                           "5" = "Late-night (non-work)"))

tnc_purpose_overall <-
  pct_calculator(tnc_for_purposes,
                 breakdown_by = "tnc_purp",
                 weight = "wtperfin")


tnc_purpose_age <-
  pct_calculator(tnc_for_purposes %>% 
                   filter(!is.na(age_bin)),
                 breakdown_by = "tnc_purp",
                 second_breakdown = "age_bin",
                 weight = "wtperfin") %>% 
  # Add baseline totals
  rbind(tnc_purpose_overall %>% mutate(age_bin = "CMAP region")) %>% 
  # Adjust factors
  mutate(age_bin = factor(age_bin,levels = c("60 and above","40 to 59","19 to 39","CMAP region")))

tnc_p3 <-
  # Get data
  tnc_purpose_age %>% 
  # Add flag for pattern
  mutate(type = case_when(age_bin == "CMAP region" ~ "1",
                          TRUE ~ "0" )) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = age_bin, fill = tnc_purp,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 30,
                              pattern_density = 0.05,
                              pattern_spacing = 0.0325,
                              pattern_key_scale_factor = 0.4,
                              position = position_stack(reverse = T),
                              width = 0.8) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  geom_text(position = position_stack(reverse = T,vjust = 0.5),
            color = "white") +
  
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  
  # Add CMAP themes
  theme_cmap(gridlines = "v",
             xlab = "Typical reason for using a Transportation Network Company") +
  scale_fill_discrete(type = c("#8c0000","#efa7a7","#00093f","#006b8c")) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 3,override.aes = list(pattern = "none")))


finalize_plot(tnc_p3,
              title = "Across age groups, a majority of travelers say they 
              typically use Transportation Network Companies for non-work trips, 
              although there are differences by time of day.",
              caption = "Source: Chicago Metropolitan Agency for Planning 
              Analysis of My Daily Travel data.",
              filename = "tnc_p3",
              # mode = "png",
              overwrite = T)

################################################################################
# PLOT OF RACE | PURPOSE
################################################################################

tnc_purpose_race <-
  pct_calculator(tnc_for_purposes %>% filter(race_eth != "missing"),
                 breakdown_by = "tnc_purp",
                 second_breakdown = "race_eth",
                 weight = "wtperfin") %>% 
  # Add baseline totals
  rbind(tnc_purpose_overall %>% mutate(race_eth = "CMAP region")) %>% 
  # Adjust factors
  mutate(race_eth = factor(race_eth,
                           levels = c("black","asian","other","hispanic","white","CMAP region")))


tnc_p4 <-
  # Get data
  tnc_purpose_race %>% 
  # Add flag for pattern
  mutate(type = case_when(race_eth == "CMAP region" ~ "1",
                          TRUE ~ "0" )) %>% 
  # Adjust values for shift around 0 axis
  mutate(pct = ifelse(!(tnc_purp %in% c("Late-night (non-work)","Daytime (non-work)")),
                      -1 * pct, pct)) %>% 
  # Capitalize
  mutate(race_eth = recode_factor(race_eth,
                                  "other" = "Other",
                                  "hispanic" = "Hispanic",
                                  "black" = "Black",
                                  "asian" = "Asian",
                                  "CMAP region" = "CMAP region",
                                  "white" = "White")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = race_eth, fill = tnc_purp,
             # Only label bars that round to at least 5 percent
             label = ifelse(!(tnc_purp %in% c("Late-night (non-work)","Daytime (non-work)")),
                            scales::label_percent(accuracy = 1)(-1 * pct),
                            scales::label_percent(accuracy = 1)(pct)))) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 30,
                              pattern_density = 0.05,
                              pattern_spacing = 0.0325,
                              pattern_key_scale_factor = 0.4,
                              position = position_stack(reverse = T),
                              width = 0.8) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  geom_text(position = position_stack(reverse = T,vjust = 0.5),
            color = "white") +
  
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-.75,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-.75,.75,by = .25))),
                     limits = c(-.75,.8)) +
  
  # Add CMAP themes
  theme_cmap(gridlines = "v",
             xlab = "Typical reason for using a Transportation Network Company") +
  scale_fill_discrete(type = c("#8c0000","#efa7a7","#00093f","#006b8c")) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 3,override.aes = list(pattern = "none")))

finalize_plot(tnc_p4,
              title = "White travelers are much more likely to report using 
              Transportation Network Companies for non-work trips.",
              caption = "Source: Chicago Metropolitan Agency for Planning 
              Analysis of My Daily Travel data.",
              filename = "tnc_p4",
              mode = "png",
              overwrite = T)


################################################################################
# ARCHIVE - PLOT OF INCOME | PURPOSE
################################################################################
# 
# tnc_purpose_income <-
#   pct_calculator(tnc_for_purposes %>% filter(income_c != "missing"),
#                  breakdown_by = "tnc_purp",
#                  second_breakdown = "income_c",
#                  weight = "wtperfin") %>% 
#   # Add baseline totals
#   rbind(tnc_purpose_overall %>% mutate(income_c = "CMAP region")) %>% 
#   # Adjust factors
#   mutate(income_c = factor(income_c,
#                            levels = c("high","middle-high","middle-low","low","CMAP region")))
# 
# 
# tnc_p4 <-
#   # Get data
#   tnc_purpose_income %>% 
#   # Add flag for pattern
#   mutate(type = case_when(income_c == "CMAP region" ~ "1",
#                           TRUE ~ "0" )) %>% 
#   # Capitalize
#   mutate(income_c = recode_factor(income_c,
#                                   "high" = "$100K or more",
#                                   "middle-high" = "$60K - $99K",
#                                   "middle-low" = "$35K - $59K",
#                                   "low" = "$35K or less")) %>% 
#   
#   # Create ggplot object
#   ggplot(aes(x = pct, y = income_c, fill = tnc_purp,
#              # Only label bars that round to at least 5 percent
#              label = ifelse(pct >.05,scales::label_percent(accuracy = 1)(pct),""))) +
#   # Use "geom_col_pattern" to add texture to a subset of columns
#   ggpattern::geom_col_pattern(aes(pattern = type),
#                               pattern_color = "white",
#                               pattern_fill = "white",
#                               pattern_angle = 30,
#                               pattern_density = 0.05,
#                               pattern_spacing = 0.0325,
#                               pattern_key_scale_factor = 0.4,
#                               position = position_stack(reverse = T),
#                               width = 0.8) +  
#   # Re-assign patterns manually
#   scale_pattern_manual(values = c("1" = "stripe",
#                                   "0" = "none"),
#                        guide = "none") +
#   
#   geom_text(position = position_stack(reverse = T,vjust = 0.5),
#             color = "white") +
#   
#   
#   # Adjust axis
#   scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
#   
#   # Add CMAP themes
#   theme_cmap(gridlines = "v",
#              xlab = "Typical reason for using a Transportation Network Company") +
#   scale_fill_discrete(type = c("#8c0000","#efa7a7","#00093f","#006b8c")) +
#   
#   # Adjust legend for formatting
#   guides(fill = guide_legend(ncol = 3,override.aes = list(pattern = "none")))
# 
# finalize_plot(tnc_p4,
#               title = "Higher-income travelers are much more likely to report using 
#               Transportation Network Companies for non-work trips.",
#               caption = "Source: Chicago Metropolitan Agency for Planning 
#               Analysis of My Daily Travel data.",
#               filename = "tnc_p3",
#               # mode = "png",
#               overwrite = T)
# 


