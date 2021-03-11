# This script produces analyses on school trips in the CMAP region.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)
library(sf)

source("helper_fns.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

### Filter data
all_school_mdt <-
  mdt %>%                            # 125091 records
  filter(
    # Keep records of travelers enrolled in K-12
    schol %in% c(3,4),               #
    # Keep only:                     #
    # Those 5 or older
    age >= 5 |
      # or those in an age category from 5 to 44
      age < 0 & aage %in% c(2,3,4,5) |
      # or those enrolled in 9th-12th grade
      age < 0 & schol %in% c(4)) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%  #
  # Keep only trips with nonzero distance
  filter(distance_pg > 0) %>%        #
  # Exclude missing mode trips
  filter(mode_c != "missing") %>%    #
  # Keep only trips to school
  filter(tpurp_c == "school") %>%    #
  # Keep only trips to identified school locations
  filter(loctype == 3) %>%               # 4001 records
  # Keep only trips that start or end between 7am and 9am
  filter(hour(arrtime_pg) %in% c(7,8) |  # 3530 records
           hour(start_times_pg) %in% c(7,8)) %>%
  # Exclude Kendall County (for validation purposes only)
  # filter(!(state_fips == 17 & county_fips == 93)) %>%
  # # Code to exclude high and low weight households, by zone
  left_join(zones, by = "sampno") %>%
  arrange(wtperfin) %>%
  group_by(cluster) %>%
  mutate(rank = row_number()) %>%
  mutate(max_rank = max(rank)) %>%
  mutate(pct_rank = rank / max_rank) %>%
  filter(pct_rank >= 0.05,
         pct_rank <= 0.95) %>%
  ungroup()



# Repeat same filtering and mutation for TT
all_school_tt <-
  tt %>%                            # 137491 records
  filter(
    # Keep records of travelers enrolled in K-12
    SCHOL %in% c(3,4),              #
    # Keep only those 5 or older. Note that 99 is DK/RF for AGE.
    (AGE >= 5 & AGE < 99) | (AGE == 99 & AGEB == 2)) %>%       #
  # Keep only trips with nonzero distance (note this is a different difference
  # calculation than the MDT one - TT used great circle)
  filter(DIST > 0) %>%              #
  # Keep only trips to school
  filter(tpurp_c == "school") %>%   #
  # Add location information for trip destinations
  left_join(tt_location %>% select(locno = LOCNO,
                                   school_county_fips = FIPS,
                                   tract_fips = TRACT),
            by = "locno") %>%
  # Exclude Kendall County (for validation purposes only)
  # filter(school_county_fips != 17093) %>%
  # Keep only trips that start or end between 7am and 9am
  filter(ARR_HR %in% c(7,8) | start_hr %in% c(7,8)) %>% # 2874 records
  # Remove home locations (there are none)
  filter(substr(locno,1,1) != "9")  # 2874 records

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################


################################################################################
#
# Survey responses - Mode share for school
################################################################################

all_respondents_school_mdt <-
  mdt_all_respondents %>%
  filter(smode >0,
         schol %in% c(3,4)) %>%
  filter(age >= 5 |
           # or those in an age category from 5 to 44
           aage %in% c(2,3,4,5) |
           # or those enrolled in 9th-12th grade
           schol %in% c(4) |
           # or those identified as attending school manually
           sampno %in% c(70038312,
                         70051607)) %>%
  mutate(smode = recode(smode,
                        "1" = "Walk",
                        "2" = "Bike",
                        "3" = "Driver",
                        "4" = "Driver",
                        "5" = "Passenger",
                        "6" = "Passenger",
                        "7" = "School bus",
                        "8" = "Transit",
                        "9" = "Transit",
                        "10" = "Transit",
                        "11" = "Other",
                        "12" = "Other",
                        "13" = "Other",
                        "14" = "Other",
                        "15" = "Other",
                        "16" = "Other",
                        "17" = "Other",
                        "18" = "Other",
                        "97" = "Other")) %>%
  mutate(survey = "My Daily Travel ('19)")

all_respondents_school_tt <-
  tt_all_respondents %>%
  filter(!is.na(SMODE),
         !(SMODE %in% c(98,99)),
         SCHOL %in% c(3,4),
         # Keep only those 5 or older
         AGE >= 5 | AGEB == 2) %>%
  filter(!is.na(WGTP)) %>%
  mutate(smode = recode(SMODE,
                        "1" = "Walk",
                        "2" = "Bike",
                        "3" = "Driver",
                        "4" = "Passenger",
                        "5" = "Transit",
                        "6" = "Transit",
                        "7" = "Transit",
                        "8" = "Transit",
                        "9" = "Other",
                        "10" = "Other",
                        "11" = "School bus",
                        "12" = "Other",
                        "14" = "Transit",
                        "97" = "Other")) %>%
  mutate(survey = "Travel Tracker ('09)")

################################################################################
# Chart of mode share, survey responses
################################################################################

survey_school_mode_mdt <-
  pct_calculator(all_respondents_school_mdt,
                 breakdown_by = "smode",
                 weight = "wtperfin",
                 survey = "mdt")

survey_school_mode_tt <-
  pct_calculator(all_respondents_school_tt,
                 breakdown_by = "smode",
                 weight = "WGTP",
                 survey = "tt")

survey_school_mode <-
  survey_school_mode_mdt %>%
  rbind(survey_school_mode_tt)

school_trips_p1 <-
  survey_school_mode %>%
  ggplot(aes(x = pct, y = reorder(smode,desc(-pct)))) +
  geom_col(aes(fill = survey),position = position_dodge2(reverse = T)) +
  theme_cmap(vline = 0, gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0,.36)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = survey),
             position = position_dodge2(reverse = T, width = 0.9),
             label.size = 0,
             fill = "white",
             hjust = 0) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p1,
              title = "Typical mode used to get to school, comparing 2008 to 2019 (survey).",
              caption = "Note: Mode share represents survey responses to the
              question, \"How do you usually get to school?\" Figures are not
              based on observed or recorded travel behavior.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p1",
              # mode = "png",
              overwrite = T,
              height = 4,
              width = 6)


################################################################################
# Chart of walk share by income, survey responses
################################################################################

survey_school_mode_inc_mdt <-
  pct_calculator(all_respondents_school_mdt %>%
                   filter(income_c != "missing"),
                 breakdown_by = "smode",
                 second_breakdown = "income_c",
                 weight = "wtperfin",
                 survey = "mdt")

survey_school_mode_inc_tt <-
  pct_calculator(all_respondents_school_tt %>%
                   filter(income_c != "missing"),
                 breakdown_by = "smode",
                 second_breakdown = "income_c",
                 weight = "WGTP",
                 survey = "tt")

survey_school_mode_inc <-
  survey_school_mode_inc_mdt %>%
  rbind(survey_school_mode_inc_tt)

school_trips_p1a <-
  survey_school_mode_inc %>%
  filter(smode == "Walk") %>%
  ggplot(aes(x = pct, y = income_c)) +
  geom_col(aes(fill = survey),position = position_dodge2(reverse = T)) +
  theme_cmap(vline = 0, gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0,.45)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = survey),
             position = position_dodge2(reverse = T, width = 0.9),
             label.size = 0,
             fill = "white",
             hjust = 0) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p1a,
              title = "Walk mode share by income, comparing 2008 to 2019 (survey).",
              caption = "Note: Mode share represents survey responses to the
              question, \"How do you usually get to school?\" Figures are not
              based on observed or recorded travel behavior.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p1a",
              # mode = "png",
              overwrite = T,
              height = 4,
              width = 6)

################################################################################
#
# Mode share for school
################################################################################

## Create totals for trips by mode category (within universe of school trips)

### Calculate proportions for TT
all_school_mode_c_tt <-
  pct_calculator(all_school_tt,
                 breakdown_by = "mode_c",
                 weight = "weight",
                 survey = "tt")


### Calculate proportions for MDT
all_school_mode_c_mdt <-
  pct_calculator(all_school_mdt,
                 breakdown_by = "mode_c",
                 weight = "wtperfin",
                 survey = "mdt")

### Join MDT and TT
total_school_mode_c <-
  rbind(all_school_mode_c_tt,
        all_school_mode_c_mdt)

# Chart of mode share for K-12 trips, MDT vs TT
school_trips_p2 <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c,desc(-pct)), x = pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = survey),
            position = position_dodge2(0.9,reverse = T),
            hjust = 0,
            label.size = 0,
            fill = "white") +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),n.breaks = 6, limits = c(0,.45)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p2,
              "Mode share of K-12 school trips, 2008 vs. 2019.",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p2",
              # mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T)

################################################################################
#
# School walk trips by income
################################################################################

## Create totals for trips by mode category (within universe of school trips)

### Calculate proportions for TT

all_school_inc_mode_c_tt <-
  pct_calculator(all_school_tt %>%
                   filter(income_c != "missing"),
                 breakdown_by = "mode_c",
                 second_breakdown = "income_c",
                 weight = "weight",
                 survey = "tt")

### Calculate proportions for MDT (repeat from TT)

all_school_inc_mode_c_mdt <-
  pct_calculator(all_school_mdt %>%
                   filter(income_c != "missing"),
                 breakdown_by = "mode_c",
                 second_breakdown = "income_c",
                 weight = "wtperfin",
                 survey = "mdt")

### Join MDT and TT
total_school_inc_mode_c <-
  rbind(all_school_inc_mode_c_tt,
        all_school_inc_mode_c_mdt)

# Chart of walking mode share
school_trips_p3 <-
  total_school_inc_mode_c %>%
  # mutate(unweighted = breakdown_n/total_n) %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  filter(income_c != "missing",
         mode_c == "walk"
  ) %>%
  mutate(income_c = recode_factor(income_c,
                "low" = "$34,999 or less",
                "middle-low" = "$35,000 to $59,999",
                "middle-high" = "$60,000 to $99,999",
                "high" = "$100,000 or more")) %>%
  ggplot(aes(y = income_c, x = pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = survey),
             position = position_dodge2(0.9,reverse = T),
             hjust = 0, label.size = 0, fill = "white") +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),n.breaks = 6,
                     limits = c(0,.44)
  ) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p3,
              "Walk mode-share of K-12 school trips by household income
              category, 2008 vs. 2019.",
              "Note: Includes trips starting or ending between 7:00 A.M. and
              9:00 A.M. to a school location for travelers enrolled in K-12 and
              at least 5 years old. Excludes highest and lowest 5 percent of
              weighted records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              # height = 6.3,
              # width = 11.3,
              filename = "school_trips_p3",
              mode = "png",
              overwrite = T)

################################################################################
#
# Travel times by race
################################################################################

## What about travel times by race for school trips

# Filter data for MDT
school_time_race_mdt <-
  all_school_mdt %>%
  filter(
    # Only include trips that are more than 0 minutes and less than 2.5 hours
    travtime_pg_calc < 150 & travtime_pg_calc > 0,
    # Exclude households with missing race and ethnicity information
    race_eth != "missing") %>%
  # Add breakdown between K-8 and 9-12
  mutate(k12 = ifelse(schol == 3,
                      "Elementary and middle school",
                      "High school")) %>%
  group_by(race_eth,k12) %>%
  summarize(travtime = as.numeric(weighted.mean(travtime_pg_calc, w = wtperfin))) %>%
  mutate(survey = "My Daily Travel (2019)")

# Chart of travel time to school by household income
school_trips_p4 <-
  school_time_race_mdt %>%
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
  facet_wrap(~k12) +
  cmap_fill_race(white = "White",black = "Black",hispanic = "Hispanic",asian = "Asian",other = "Other")

finalize_plot(school_trips_p4,
              "Average travel time to school by race and ethnicity (minutes).",
              "Note: Includes trips starting or ending between 7:00 A.M. and
              9:00 A.M. to a school location for travelers enrolled in K-12 and
              at least 5 years old. Excludes highest and lowest 5 percent of
              weighted records. Trips with no travel time or lasting 150 minutes
              or more are also excluded as outliers.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "school_trips_p4",
              sidebar_width = 2,
              mode = "png",
              # # height = 6.3,
              # width = 11.3,
              overwrite = T)

################################################################################
#
# Regression on income, race, and ethnicity
################################################################################

##### Run regressions on income and race and ethnicity

all_school_mdt_lm <-
  all_school_mdt %>%
  mutate(travtime_lm = as.double(travtime_pg_calc)) %>%
  filter(travtime_lm < 150,
         travtime_lm > 0) %>%
  mutate(
    white = case_when(
      race_eth == "white" ~ 1,
      TRUE ~ 0),
    black = case_when(
      race_eth == "black" ~ 1,
      TRUE ~0),
    asian = case_when(
      race_eth == "asian" ~ 1,
      TRUE ~ 0),
    hispa = case_when(
      race_eth == "hispanic" ~ 1,
      TRUE ~ 0),
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
    cook = case_when(
      home_county == 31 ~ 1,
      TRUE ~ 0),
    k8 = case_when(
      schol == 3 ~ 1,
      TRUE ~ 0)
  )

school_trips_regression <-
  lm(travtime_lm ~ white + black + hispa + asian + high_inc +
       distance_pg + cook + car_trip + school_bus + transit +
       walk + bike + k8,
     all_school_mdt_lm,
     weights = wtperfin)

summary(school_trips_regression)


all_school_mdt_lm %>%
  ggplot(aes(x = travtime_lm, y = race_eth)) +
  geom_boxplot()


################################################################################
#
# Geographic distribution of schools
################################################################################


## MDT

### Tracts
tract_sf <- sf::read_sf("V:/Demographic_and_Forecast/Census/2010/Geography/CMAP_Region_Projected/Tracts_CMAP_TIGER2010.shp")
county_sf <- sf::read_sf("V:/Administrative_and_Political_Boundaries/Counties/Cnty7_NIPC_05.shp")


# Identify the locations of schools
school_locations_mdt <-
  all_school_mdt %>%
  filter(state_fips == 17 & county_fips %in% cmap_seven_counties) %>%
  group_by(county_fips,tract_fips) %>%
  summarize(uw_school_trips = n(),
            w_school_trips = sum(wtperfin))


tract_schools_mdt <-
  tract_sf %>%
  mutate(TRACTCE10 = as.integer(TRACTCE10),
         COUNTYFP10 = as.integer(COUNTYFP10)) %>%
  left_join(school_locations_mdt, by = c("TRACTCE10" = "tract_fips",
                                         "COUNTYFP10" = "county_fips")) %>%
  select(TRACTCE10,COUNTYFP10,w_school_trips,uw_school_trips,geometry)

number_of_tracts_with_schools_mdt <-
  school_locations_mdt %>%
  filter(uw_school_trips > 0) %>%
  group_by(county_fips) %>%
  summarize(n = n()) %>%
  arrange(-n)

school_trips_map1 <-
  tract_schools_mdt %>%
  ggplot(aes(fill = w_school_trips)) +
  geom_sf() +
  theme_cmap(axis.text = element_blank(),
             legend.position = "right",
             legend.direction = "vertical",
             legend.key.height = grid::unit(20,"bigpts")) +
  cmap_fill_continuous(palette = "seq_blues")
# scale_fill_binned(breaks = c(5,10,25,50,100))
# scale_fill_binned(type = "viridis")

finalize_plot(school_trips_map1,
              title = "Number of school trips per tract (weighted).",
              caption = "Source: CMAP analysis of MDT data.",
              title_width = 1.5,
              legend_shift = FALSE,
              height = 6.3,
              width = 5.65,
              filename = "school_trips_map1",
              mode = "png",
              overwrite = T
)

### Counties

school_county_locations_mdt <-
  all_school_mdt %>%
  group_by(county_fips) %>%
  summarize(uw_school_trips = n(),
            w_school_trips = sum(wtperfin)) %>%
  ungroup() %>%
  group_by() %>%
  mutate(total_w = sum(w_school_trips),
         total_uw = sum(uw_school_trips)) %>%
  mutate(w_school_trips_pct = w_school_trips / total_w,
         uw_school_trips_pct = uw_school_trips / total_uw) %>%
  pivot_longer(cols = ends_with("school_trips_pct"))

county_schools_mdt <-
  county_sf %>%
  mutate(FIPSCNTY = as.integer(FIPSCNTY)) %>%
  left_join(school_county_locations_mdt,
            by = c("FIPSCNTY" = "county_fips"))


school_trips_map2 <-
  county_schools_mdt %>%
  mutate(name = recode(name,
                       "uw_school_trips_pct" = "Unweighted trips",
                       "w_school_trips_pct" = "Weighted trips")) %>%
  ggplot(aes(fill = value)) +
  geom_sf() +
  theme_cmap(axis.text = element_blank(),
             legend.position = "right",
             legend.direction = "vertical",
             legend.key.height = grid::unit(30,"bigpts")) +
  facet_wrap(~name) +
  # scale_fill_binned(breaks = c(5,10,25,50,100)) +
  scale_fill_binned(type = "viridis",
                    label = scales::label_percent(accuracy = 1),
                    n.breaks = 10)

finalize_plot(school_trips_map2,
              title = "Number of school trips per county.",
              caption = "Source: CMAP analysis of MDT data.",
              legend_shift = FALSE,
              height = 6.3,
              width = 11.3,
              filename = "school_trips_map2",
              mode = "png",
              overwrite = T
)


## TT

### Tracts
tract_sf_tt <- sf::read_sf("V:/Demographic_and_Forecast/Census/2000/Geography/TractILne11co_Census_2000.shp") %>%
  filter(FIPSSTCO %in% cmap_state_seven_counties)


# Identify the locations of schools
school_locations_tt <-
  all_school_tt %>%
  group_by(school_county_fips,tract_fips) %>%
  summarize(uw_school_trips = n(),
            w_school_trips = sum(weight))


tract_schools_tt <-
  tract_sf_tt %>%
  mutate(FIPSSTCO = as.integer(FIPSSTCO),
         TRACTID = as.numeric(TRACTID)) %>%
  left_join(school_locations_tt, by = c("FIPSSTCO" = "school_county_fips",
                                        "TRACTID" = "tract_fips")) %>%
  select(FIPSSTCO,TRACTID,w_school_trips,uw_school_trips,geometry)

number_of_tracts_with_schools_tt <-
  school_locations_tt %>%
  filter(uw_school_trips > 0) %>%
  mutate(school_county_fips = as.integer(sub("17","",school_county_fips))) %>%
  group_by(school_county_fips) %>%
  summarize(n = n()) %>%
  arrange(-n)


school_trips_map3 <-
  tract_schools_tt %>%
  ggplot(aes(fill = uw_school_trips)) +
  geom_sf() +
  theme_cmap(axis.text = element_blank(),
             legend.position = "right",
             legend.direction = "vertical",
             legend.key.height = grid::unit(20,"bigpts")) +
  # scale_fill_binned(breaks = c(5,10,25,50,100)) +
  scale_fill_binned(type = "viridis")

finalize_plot(school_trips_map3,
              title = "Number of school trips per tract.",
              caption = "Source: CMAP analysis of MDT data.",
              legend_shift = FALSE,
              height = 9,
              filename = "school_trips_map3",
              # mode = "png"
)

### Counties

school_county_locations_tt <-
  all_school_tt %>%
  mutate(school_county_fips = as.integer(sub("17","",school_county_fips))) %>%
  group_by(school_county_fips) %>%
  summarize(uw_school_trips = n(),
            w_school_trips = sum(weight)) %>%
  ungroup() %>%
  group_by() %>%
  mutate(total_w = sum(w_school_trips),
         total_uw = sum(uw_school_trips)) %>%
  mutate(w_school_trips_pct = w_school_trips / total_w,
         uw_school_trips_pct = uw_school_trips / total_uw) %>%
  pivot_longer(cols = ends_with("school_trips_pct"))

county_schools_tt <-
  county_sf %>%
  mutate(FIPSCNTY = as.integer(FIPSCNTY)) %>%
  left_join(school_county_locations_tt,
            by = c("FIPSCNTY" = "school_county_fips"))


school_trips_map4 <-
  county_schools_tt %>%
  mutate(name = recode(name,
                       "uw_school_trips_pct" = "Unweighted trips",
                       "w_school_trips_pct" = "Weighted trips")) %>%
  ggplot(aes(fill = value)) +
  geom_sf() +
  theme_cmap(axis.text = element_blank(),
             legend.position = "right",
             legend.direction = "vertical",
             legend.key.height = grid::unit(30,"bigpts")) +
  facet_wrap(~name) +
  # scale_fill_binned(breaks = c(5,10,25,50,100)) +
  scale_fill_binned(type = "viridis",
                    label = scales::label_percent(accuracy = 1),
                    n.breaks = 10)

finalize_plot(school_trips_map4,
              title = "Number of school trips per county.",
              caption = "Source: CMAP analysis of TT data.",
              legend_shift = FALSE,
              filename = "school_trips_map4",
              # mode = "png"
)



#### Chart of total represented tracts with school trips, MDT vs TT
county_schools_comparison <-
  school_county_locations_mdt %>%
  rename(school_county_fips = county_fips) %>%
  filter(school_county_fips %in% cmap_seven_counties) %>%
  mutate(survey = "mdt") %>%
  rbind(school_county_locations_tt %>%
          mutate(survey = "tt")) %>%
  select(-name,-value,-total_w,-total_uw) %>%
  distinct() %>%
  pivot_longer(cols = ends_with("school_trips")) %>%
  rbind(
    rbind(number_of_tracts_with_schools_mdt %>%
            mutate(survey = "mdt") %>%
            rename(school_county_fips = county_fips),
          number_of_tracts_with_schools_tt %>%
            mutate(survey = "tt")) %>%
      mutate(name = "Tracts with schools") %>%
      rename(value = n)) %>%
  filter(school_county_fips %in% cmap_seven_counties) %>%
  mutate(school_county_fips = recode(school_county_fips,
                                     "31" = "Cook",
                                     "43" = "DuPage",
                                     "89" = "Kane",
                                     "93" = "Kendall",
                                     "97" = "Lake",
                                     "111" = "McHenry",
                                     "197" = "Will"))


school_trips_p5 <-
  county_schools_comparison %>%
  mutate(xmax =
           case_when(
             name == "uw_school_trips" ~ 2500,
             name == "w_school_trips" ~ 1000000,
             TRUE ~ 600),
         name = recode(name,
                       "uw_school_trips" = "School trips (unweighted)",
                       "w_school_trips" = "School trips (weighted)"),
         survey = recode_factor(survey,
                                "mdt" = "My Daily Travel ('19)",
                                "tt" = "Travel Tracker ('08)")) %>%
  ggplot(aes(y = school_county_fips, x = value, fill = survey)) +
  geom_col(position = position_dodge2(reverse = T)) +
  theme_cmap(gridlines = "v",
             axis.text.x = element_text(angle = 90)) +
  geom_label(aes(label = scales::label_comma(accuracy = 1)(value),
                 group = survey),
             position = position_dodge2(reverse = T, width = 0.9),
             label.size = 0,
             hjust = 0,
             label.padding = unit(.05,"lines"),
             fill = "white") +
  scale_x_continuous(label = scales::label_comma()) +
  geom_blank(aes(x = xmax)) +
  facet_wrap(~name,ncol = 3,scales = "free_x")

finalize_plot(school_trips_p5,
              title = "Overview of school trip data distribution.",
              caption = "Source: CMAP analysis of MDT and TT data.",
              width = 11.3,
              title_width = 1.5,
              height = 6.3,
              mode = "png",
              filename = "school_trips_p5",
              overwrite = T)

################################################################################
#
# Archived chart - Overall school trip mode share by home county
################################################################################
#
# ### Calculate proportions for TT
# all_school_county_mode_c_tt <-
#   all_school_tt %>%
#   filter(home_county %in% cmap_seven_counties) %>%
#   group_by(mode_c,home_county) %>%
#   summarize(mode_c_total = sum(weight)) %>%
#   group_by(home_county) %>%
#   mutate(mode_c_pct = mode_c_total / sum(mode_c_total),
#          survey = "tt")
#
# ### Calculate proportions for MDT
# all_school_county_mode_c_mdt <-
#   all_school_mdt %>%
#   filter(home_county %in% cmap_seven_counties) %>%
#   group_by(mode_c,home_county) %>%
#   summarize(mode_c_total = sum(wtperfin)) %>%
#   group_by(home_county) %>%
#   mutate(mode_c_pct = mode_c_total / sum(mode_c_total),
#          survey = "mdt")
#
# ### Join MDT and TT
# total_school_county_mode_c <-
#   rbind(all_school_county_mode_c_tt,
#         all_school_county_mode_c_mdt)
#
# # Chart of mode share for K-12 trips, MDT vs TT by county
# school_trips_p1a <-
#   total_school_county_mode_c %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   # geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
#   #                group = survey),
#   #            position = position_dodge2(0.9,reverse = T),
#   #            hjust = 0,
#   #            label.size = 0,
#   #            fill = "white") +
#   theme_cmap(gridlines = "v") +
#   facet_wrap(~home_county) +
#   scale_x_continuous(labels = scales::label_percent()) +
#   cmap_fill_discrete(palette = "friday")
#
# finalize_plot(school_trips_p1a,
#               "Mode share of K-12 school trips, 2008 vs. 2019.",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old. Travel Tracker had two school-related trip categories
#               (both included) while My Daily Travel had only one.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               filename = "school_trips_p1a",
#               # mode = "png",
#               width = 11.3,
#               height = 6.3,
#               overwrite = T)

################################################################################
#
# Archived chart - total number of school trips
################################################################################
#
# # Chart of absolute numbers of school trips, MDT vs TT
# school_trips_p2 <-
#   total_school_mode_c %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-mode_c_total)),
#              x = mode_c_total,
#              fill = survey)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   geom_label(aes(label = scales::label_comma(accuracy = 1)(mode_c_total),
#                  group = survey),
#              position = position_dodge2(0.9,reverse = T),
#              hjust = 0, label.size = 0, fill = "white") +
#   theme_cmap(gridlines = "v") +
#   scale_x_continuous(labels = scales::label_comma(scale = .001),
#                      n.breaks = 6,
#                      limits=c(0,600000)) +
#   cmap_fill_discrete(palette = "friday")
#
# finalize_plot(school_trips_p2,
#               "Total number of K-12 school trips by mode, 2008 vs. 2019 (in thousands).",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old. Travel Tracker had two school-related trip categories
#               (both included) while My Daily Travel had only one.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               filename = "school_trips_p2",
#               mode = "plot")
################################################################################
#
# Archived chart - all mode share by income
################################################################################
#
# # Chart of mode share by income category
# school_trips_p4 <-
#   total_school_inc_mode_c %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   filter(income_c != "missing") %>%
#   ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)),
#              x = mode_c_pct,
#              fill = survey)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
#                  group = survey),
#              position = position_dodge2(0.9,reverse = T),
#              hjust = 0, label.size = 0, fill = "white",
#              label.padding = unit(.1,"lines")) +
#   theme_cmap(gridlines = "v") +
#   facet_wrap(~income_c) +
#   scale_x_continuous(labels = scales::label_percent(),
#                      n.breaks = 4,
#                      limits = c(0,.6)) +
#   cmap_fill_discrete(palette = "friday")
#
# finalize_plot(school_trips_p4,
#               "Mode-share of K-12 school trips by household income category,
#               2008 vs. 2019.",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               height = 8,
#               filename = "school_trips_p4",
#               mode = "plot")

################################################################################
#
# Archive - Travel times by income
################################################################################
#
# ## What about travel times by income for school trips
#
# # Filter data for MDT
# school_time_mdt <-
#   all_school_mdt %>%
#   filter(
#     # Only include trips that are more than 0 minutes and less than 2.5 hours
#     travtime_pg_calc < 150 & travtime_pg_calc > 0,
#     # Exclude households with missing income information
#     income_c != "missing") %>%
#   group_by(income_c) %>%
#   summarize(travtime = as.numeric(weighted.mean(travtime_pg_calc, w = wtperfin))) %>%
#   mutate(survey = "My Daily Travel (2019)")
#
# # Repeat for TT
# school_time_tt <-
#   all_school_tt %>%
#   filter(TRPDUR < 150 & TRPDUR > 0,
#          income_c != "missing") %>%
#   group_by(income_c) %>%
#   summarize(travtime = weighted.mean(TRPDUR, w = weight)) %>%
#   mutate(survey = "Travel Tracker (2008)")
#
# # Chart of travel time to school by household income
# school_trips_p5 <-
#   school_time_mdt %>%
#   rbind(.,
#         school_time_tt) %>%
#   ggplot(aes(x = income_c, y = travtime, fill = reorder(survey,desc(survey)))) +
#   geom_col(position = position_dodge2()) +
#   theme_cmap(gridlines = "h",
#              legend.position = "None",
#              axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~survey,ncol = 2) +
#   cmap_fill_discrete(palette = "prosperity")
#
# finalize_plot(school_trips_p5,
#               "Travel time to school by household income (minutes).",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old. Trips with no travel time or lasting 150 minutes or
#               more are excluded as outliers.
#               <br><br>
#               Source: CMAP analysis of MDT and TT.",
#               filename = "school_trips_p5",
#               mode = "plot")

################################################################################
#
# Archive - Trip distances by income
################################################################################
#
# ## What about trip distances by income for school trips
#
# # Repeat filtering from above (travel time) for trip distances
# school_distance_mdt <-
#   all_school_mdt %>%
#   filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
#          income_c != "missing") %>%
#   group_by(income_c) %>%
#   summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
#   mutate(survey = "My Daily Travel (2019)")
#
# school_distance_tt <-
#   all_school_tt %>%
#   filter(DIST > 0,
#          income_c != "missing") %>%
#   group_by(income_c) %>%
#   summarize(tripdist = weighted.mean(DIST, w = weight)) %>%
#   mutate(survey = "Travel Tracker (2008)")
#
# # Chart of trip distances by household income
# school_trips_p6 <-
#   school_distance_mdt %>%
#   rbind(.,
#         school_distance_tt) %>%
#   ggplot(aes(x = income_c, y = tripdist, fill = reorder(survey,desc(survey)))) +
#   geom_col(position = position_dodge2()) +
#   theme_cmap(gridlines = "h",
#              legend.position = "None",
#              axis.text.x = element_text(angle = 45, hjust = 1)) +
#   facet_wrap(~survey,ncol = 2) +
#   cmap_fill_discrete(palette = "legislation")
#
# finalize_plot(school_trips_p6,
#               "Trip distance to school by household income (miles).",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old that are greater than 0 miles.
#               <br><br>
#               Source: CMAP analysis of MDT and TT.",
#               filename = "school_trips_p6",
#               mode = "plot")

################################################################################
#
# Archive - Travel distance by income
################################################################################
#
# ## What about trip distances by income for school trips
#
# # Repeat filtering from above (travel time) for trip distances
# school_distance_race_mdt <-
#   all_school_mdt %>%
#   filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
#          race_eth != "missing") %>%
#   group_by(race_eth) %>%
#   summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
#   mutate(survey = "My Daily Travel (2019)")
#
# # Chart of trip distances by household income
# school_trips_p8 <-
#   school_distance_race_mdt %>%
#   ggplot(aes(x = reorder(race_eth,desc(tripdist)), y = tripdist, fill = race_eth)) +
#   geom_col(position = position_dodge2()) +
#   theme_cmap(gridlines = "h",
#              legend.position = "None") +
#   cmap_fill_race()
#
# finalize_plot(school_trips_p8,
#               "Trip distance to school by race and ethnicity (miles).",
#               "Note: Includes trips for travelers enrolled in K-12 and at least
#               5 years old that are greater than 0 miles.
#               <br><br>
#               Source: CMAP analysis of MDT and TT.",
#               filename = "school_trips_p8",
#               mode = "plot")

