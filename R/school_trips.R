# This script produces analyses on school trips in the CMAP region. It is
# referenced in Policy Brief #2.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(lubridate)
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
school_base_mdt <-
  mdt %>%                            # 125463 records
  # Keep records of travelers enrolled in K-12
  filter(schol %in% c(3,4)) %>%      # 19044 records
  # Keep only...
  filter(                            # 19044 records
    # Those 5 or older
    age >= 5 |
      # or those in an age category from 5 to 44 (18-44 is the option that includes 18).
      (age < 0 & aage %in% c(2,3,4,5)) |
      # or those enrolled in 9th-12th grade
      (age < 0 & schol %in% c(4))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  # 14068 records
  # Keep only trips with nonzero distance (note this is network distance, not haversine)
  filter(distance_pg > 0) %>%        # 14062 records
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    # 14062 records
  # Keep only trips to school
  filter(tpurp_c == "school") %>%    # 4424 records
  # Keep only trips to identified school locations
  filter(loctype == 3) %>%           # 4002 records
  # Keep only trips that start or end between 7am and 9am (so those that have a
  # start time and/or an end time with 7 or 8 as their hour.)
  filter(lubridate::hour(arrtime_pg) %in% c(7,8) |  # 3531 records
           lubridate::hour(start_times_pg) %in% c(7,8)) %>%
  ungroup()

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################


################################################################################
#
# Travel times by race
################################################################################

## Note: This analysis focuses only on MDT because TT did not include
## individual-level data on race and ethnicity. Only one respondent per
## household was asked about that data.

# Filter data for MDT
school_time_race_person_level_mdt <-
  school_base_mdt %>%                # 3531 records
  # Only include trips that are more than 0 minutes
  filter(travtime_pg_calc > 0) %>%  # 3529 records
  # Only include trips that are less than 2.5 hours
  filter(travtime_pg_calc < 150) %>% # 3529 records
  # Exclude households with missing race and ethnicity information
  filter(race_eth != "missing") %>% # 3518 records
  # Add breakdown between K-8 and 9-12
  mutate(k12 = ifelse(schol == 3,
                      "Elementary and middle school",
                      "High school")) 

school_time_race_mdt <-
  school_time_race_person_level_mdt %>% # 3518
  filter(k12 != "High school") %>% # 2619
  group_by(race_eth) %>% 
  # Summarize travel time by race/ethnicity and school enrollment
  summarize(travtime25 = MetricsWeighted::weighted_quantile(x = travtime_pg_calc, probs = .25, w = weight),
            travtime50 = MetricsWeighted::weighted_median(x = travtime_pg_calc, w = weight),
            travtime_mean = weighted.mean(x = travtime_pg_calc,w = weight),
            travtime_mean_uw = mean(travtime_pg_calc),
            travtime75 = MetricsWeighted::weighted_quantile(x = travtime_pg_calc, probs = .75, w = weight),
            travtime90 = MetricsWeighted::weighted_quantile(x = travtime_pg_calc, probs = .9, w = weight),
            distance = MetricsWeighted::weighted_median(distance_pg, w = weight),
            n = n()) 

figure2_10_samplesize <-
  school_time_race_mdt %>% 
  ungroup() %>% 
  select(race_eth,n)

# Chart of travel time to school by household income
figure2_10 <-
  # Get data
  school_time_race_mdt %>%
  # Rename desired statistic
  rename(stat = travtime_mean) %>% 
  # Capitalize
  mutate(race_eth = recode_factor(factor(race_eth,
                                         levels = c("black","latino","asian","other",
                                                    "white","Non-white")),
                           "black" = "Black", 
                           "latino" = "Latino","asian" = "Asian",
                           "other" = "Other","white" = "White",
                           "Non-white" = "Non-white"
                           )) %>%
  
  # Create ggplot object
  ggplot(aes(x = race_eth, y = stat, fill = race_eth)) +
  geom_col(width = 0.85) +
  geom_label(aes(label = scales::label_number(accuracy = 1)(stat)),
             label.r = grid::unit(0,"lines"),
             vjust = -.03, label.size = 0, fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h",legend.position = "None",
             ylab = "Mean travel time to school (minutes)") +
  scale_fill_discrete(type = c("#84c87e", # Black
                               "#d8ba39", # Latino
                               "#e77272", # Asian
                               "#607b88", # Other
                               "#75a5d8"  # White
                               )) 

finalize_plot(figure2_10,
              "Black elementary and middle school students had longer trips to 
              school than those of other children.",
              caption = 
              paste0(
              "Note: Includes school trips for residents of the CMAP seven-county region,
              Grundy, and DeKalb who are enrolled in K-8 and age 5 or older. 
              Excludes trips to non-school locations, longer than two and a half 
              hours, and that did not start or end between 7:00am and 9:00am.
              See \"About the data\" for more information on race and ethnicity.
              <br><br>
              Sample size: 
              <br>- Black (",
              format(figure2_10_samplesize %>% 
                       filter(race_eth == "black") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Latino (",
              format(figure2_10_samplesize %>% 
                       filter(race_eth == "latino") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Asian (",
              format(figure2_10_samplesize %>% 
                       filter(race_eth == "asian") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Other (",
              format(figure2_10_samplesize %>% 
                       filter(race_eth == "other") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- White (",
              format(figure2_10_samplesize %>% 
                       filter(race_eth == "white") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure2_10",
              mode = c("png","pdf"),
              height = 5,
              overwrite = T)

# Backup - differences over a month (for prose)
time_disparity <- 
  school_time_race_person_level_mdt %>% # 3518 records
  filter(k12 != "High school") %>% # 2619 records
  mutate(race_eth = case_when(
    race_eth == "black" ~ "black",
    TRUE ~ "not black"
  )) %>% 
  group_by(race_eth) %>% 
  # Summarize travel time by race/ethnicity and school enrollment
  summarize(travtime_mean = weighted.mean(x = travtime_pg_calc,w = weight),
            n = n())

# Calculate the difference in mean travel times
(time_disparity %>% filter(race_eth == "black") %>% select(travtime_mean) - 
  time_disparity %>% filter(race_eth == "not black") %>% select(travtime_mean)) *
  2 * # Multiply by two trips per day
  5 * # Multiply by 5 days per week
  (52/12) / # Multiply by the number of weeks per month 
  60 # Divide by the number of minutes in an hour

# Backup - median trip times by race and mode (for prose)

school_time_race_mode_mdt <-
  school_time_race_person_level_mdt %>%
  # filter(k12 != "High school") %>% 
  group_by(race_eth,mode_c) %>% 
  # Commented code allows for graphing of high school
  mutate(race_eth = case_when(
    k12 == "High school" & race_eth != "white" ~ "Non-white",
    TRUE ~ race_eth
  )) %>%
  group_by(race_eth,k12) %>%
  # Summarize travel time by race/ethnicity and school enrollment
  summarize(travtime = as.numeric(weighted.mean(travtime_pg_calc, w = weight)),
            distance = weighted.mean(distance_pg, w = weight),
            n = n())

# Export school trips for examination in ArcGIS
school_trips_output <-
  school_time_race_person_level_mdt %>% 
  filter((k12 == "Elementary and middle school" & 
           (race_eth == "black" & travtime_pg_calc >= 14.5 & travtime_pg_calc < 15.5) |
           (race_eth == "asian" & travtime_pg_calc >= 10.5 & travtime_pg_calc < 11.5) | 
           (race_eth == "white" & travtime_pg_calc >= 10.5 & travtime_pg_calc < 11.5) |
           (race_eth == "latino" & travtime_pg_calc >= 9.5 & travtime_pg_calc < 10.5) |
           (race_eth == "other" & travtime_pg_calc >= 9.5 & travtime_pg_calc < 10.5)) |
           (k12 == "High school" &
              ((race_eth == "Non-white" & travtime_pg_calc >= 16.5 & travtime_pg_calc < 17.5) |
                 (race_eth == "white" & travtime_pg_calc >= 14.5 & travtime_pg_calc < 15.5)))) %>% 
  select(sampno,perno,placeGroup) %>% 
  mutate(include = 1) %>% 
  right_join(mdt, by = c("sampno","perno","placeGroup")) %>% 
  arrange(sampno,perno,placeGroup) %>% 
  mutate(include_lead = lead(include,1)) %>% 
  select(sampno,perno,placeGroup,include,include_lead,
         race_eth,latitude,longitude,mode_c,schol,tpurp_c,
         travtime_pg_calc,distance_pg) %>% 
  filter(include == 1 | include_lead == 1)

write.csv(school_trips_output ,
          "outputs/schooltrips.csv")

# Backup - mode share by race
pct_calculator(school_time_race_person_level_mdt,
               subset = "Elementary and middle school",
               subset_of = "k12",
               breakdown_by = "mode_c",
               second_breakdown = "race_eth",
               weight = "weight") %>% View()


################################################################################
#
# Regression on income, race, and ethnicity
################################################################################

##### Run regressions on income and race and ethnicity

school_base_mdt_lm <-
  school_base_mdt %>%
  mutate(travtime_lm = as.double(travtime_pg_calc)) %>%
  filter(travtime_lm < 150,
         travtime_lm > 0,
         schol == 3) %>%
  mutate(
    high_inc = case_when(
      income_c == "high" | income_c == "middle-high" ~ 1,
      TRUE ~ 0),
    car_trip = case_when(
      mode_c == "driver" | mode_c == "passenger" ~ 1,
      TRUE ~ 0),
    school_bus = case_when(
      mode_c == "schoolbus" ~ 1,
      TRUE ~ 0),
    transit = case_when(
      mode_c == "transit" ~ 1,
      TRUE ~ 0),
    walk = case_when(
      mode_c == "walk" ~ 1,
      TRUE ~ 0),
    bike = case_when(
      mode_c == "bike" ~ 1,
      TRUE ~ 0),
    chicago = case_when(
      home_county_chi == "Chicago" ~ 1,
      TRUE ~ 0)) %>% 
  filter(race_eth != "missing") %>% 
  mutate(race_eth = factor(race_eth,levels = c("other","white","black","asian","latino")))

school_trips_regression <-
  lm(travtime_lm ~ 
       race_eth +
       high_inc +
       distance_pg + chicago + car_trip + school_bus + transit +
       walk + bike ,
     school_base_mdt_lm,
     weights = weight)

summary(school_trips_regression)
