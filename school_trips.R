library(ggplot2)
library(tidyverse)
library(cmapplot)
library(sf)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

# Load location file
location_mdt <- read.csv("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data/location.csv")
location_tt <- read.csv("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2008 survey/loc_public.csv")

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################



## Create totals for trips by mode category (within universe of school trips)

### Filter data
all_school_mdt <-
  mdt %>%                        # 125,103 records
  filter(
    # Keep records of travelers enrolled in K-12
    schol %in% c(3,4),           # 19,018 records
    # Keep only:                   19,018 records
    # Those 5 or older
    age >= 5 |
      # or those in an age category from 5 to 44
      aage %in% c(2,3,4,5) |
      # or those enrolled in 9th-12th grade
      schol %in% c(4) |
      # or those identified as attending school manually
      sampno %in% c(70038312,
                    70051607),
    mode_c != "beginning",       # 14,042 records
    distance_pg > 0,             # 14,036 records
    mode_c != "missing",         # 14,036 records
    tpurp_c == "school"          # 4,424 records
    ) %>%
  # Mutate to character to allow case_when modification
  mutate(mode_c_school = as.character(mode_c)) %>%
  # Separate out school buses from "other"
  mutate(mode_c_school = case_when(
    mode == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
    )) %>%
  # Reconvert to factor
  mutate(mode_c_school = factor(mode_c_school)) %>%
  # Add location information for trip destinations
  left_join(location_mdt %>% select(sampno,locno,loctype,tract_fips,school_county_fips = county_fips),
          by = c("sampno","locno")) %>%
  # Keep only trips to identified school locations
  filter(loctype == 3)          # 4,001 records



# Repeat same filtering and mutation for TT
all_school_tt <-
  tt %>%                      # 140,751 records
  filter(
    # Keep records of travelers enrolled in K-12
    SCHOL %in% c(3,4),        # 18,645 records
    # Keep only those 5 or older
    AGE >= 5 | AGEB == 2,     # 4,421 records
    DIST > 0,                 # 4,421 records
    tpurp_c == "school"       # 3,335 records
    ) %>%
  mutate(mode_c_school = as.character(mode_c)) %>%
  mutate(mode_c_school = case_when(
    MODE == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
  )) %>%
  mutate(mode_c_school = factor(mode_c_school)) %>%
  # Add location information for trip destinations
  left_join(location_tt %>% select(locno = LOCNO,school_county_fips = FIPS,tract_fips = TRACT), by = "locno") %>%
  # Remove home locations (there are none)
  filter(substr(locno,1,1) != "9") %>%                # 3,335 records
  # Keep only school trips to schools in the seven counties
  filter(school_county_fips %in% cmap_state_counties) # 3,291 records


### Calculate proportions for TT
all_school_mode_c_tt <-
  all_school_tt %>%
  group_by(mode_c_school) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_school_mode_c_mdt <-
  all_school_mdt %>%
  group_by(mode_c_school) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_school_mode_c <-
  rbind(all_school_mode_c_tt,
        all_school_mode_c_mdt)

# Chart of mode share for K-12 trips, MDT vs TT
school_trips_p1 <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
                 group = survey),
            position = position_dodge2(0.9,reverse = T),
            hjust = 0,
            label.size = 0,
            fill = "white") +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6, limits = c(0,.45)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p1,
              "Mode share of K-12 school trips, 2008 vs. 2019.",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p1",
              mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T)

### Calculate proportions for TT
all_school_county_mode_c_tt <-
  all_school_tt %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(mode_c_school,home_county) %>%
  summarize(mode_c_total = sum(weight)) %>%
  group_by(home_county) %>%
  mutate(mode_c_pct = mode_c_total / sum(mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_school_county_mode_c_mdt <-
  all_school_mdt %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(mode_c_school,home_county) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  group_by(home_county) %>%
  mutate(mode_c_pct = mode_c_total / sum(mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_school_county_mode_c <-
  rbind(all_school_county_mode_c_tt,
        all_school_county_mode_c_mdt)

# Chart of mode share for K-12 trips, MDT vs TT by county
school_trips_p1a <-
  total_school_county_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  # geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
  #                group = survey),
  #            position = position_dodge2(0.9,reverse = T),
  #            hjust = 0,
  #            label.size = 0,
  #            fill = "white") +
  theme_cmap(gridlines = "v") +
  facet_wrap(~home_county) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p1a,
              "Mode share of K-12 school trips, 2008 vs. 2019.",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p1a",
              # mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T)

# Chart of absolute numbers of school trips, MDT vs TT
school_trips_p2 <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_total)), x = mode_c_total, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_comma(accuracy = 1)(mode_c_total),
                 group = survey),
             position = position_dodge2(0.9,reverse = T),
             hjust = 0, label.size = 0, fill = "white") +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_comma(scale = .001),n.breaks = 6, limits=c(0,600000)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p2,
              "Total number of K-12 school trips by mode, 2008 vs. 2019 (in thousands).",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "school_trips_p2",
              mode = "plot")

### Look at it by income



## Create totals for trips by mode category (within universe of school trips)

### Calculate proportions for TT

# Total trips by income bucket
all_school_inc_total_tt <-
  all_school_tt %>%
  # filter(home_county == 31) %>%
  group_by(income_c) %>%
  summarize(total = sum(weight),
            n_tt = n())

all_school_inc_mode_c_tt <-
  all_school_tt %>%
  # filter(home_county == 31) %>%
  group_by(mode_c_school,income_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  # join with total trips by income bucket to allow for percentages
  left_join(.,
            all_school_inc_total_tt,
            by = c("income_c")) %>%
  # Calculate mode share percents
  mutate(mode_c_pct = mode_c_total / total,
         survey = "tt")

### Calculate proportions for MDT (repeat from TT)
all_school_inc_total_mdt <-
  all_school_mdt %>%
  # filter(home_county == 31) %>%
  group_by(income_c) %>%
  summarize(total = sum(wtperfin),
            n_mdt = n())

all_school_inc_mode_c_mdt <-
  all_school_mdt %>%
  # filter(home_county == 31) %>%
  group_by(mode_c_school,income_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  left_join(.,
            all_school_inc_total_mdt,
            by = c("income_c")) %>%
  mutate(mode_c_pct = mode_c_total / total,
         survey = "mdt")

### Join MDT and TT
total_school_inc_mode_c <-
  rbind(all_school_inc_mode_c_tt,
        all_school_inc_mode_c_mdt)

# Chart of walking mode share
school_trips_p3 <-
  total_school_inc_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  filter(income_c != "missing",
         mode_c_school == "walk"
         ) %>%
  ggplot(aes(y = income_c, x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
                group = survey),
            position = position_dodge2(0.9,reverse = T),
            hjust = 0, label.size = 0, fill = "white") +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6,
                     limits = c(0,.45)
                     ) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p3,
              "Walk mode-share of K-12 school trips by household income category, 2008 vs. 2019.",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              height = 6.3,
              width = 11.3,
              filename = "school_trips_p3",
              mode = "png",
              overwrite = T
              )

# Chart of mode share by income category
school_trips_p4 <-
  total_school_inc_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  filter(income_c != "missing") %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_c_pct),
                 group = survey),
             position = position_dodge2(0.9,reverse = T),
             hjust = 0, label.size = 0, fill = "white",
             label.padding = unit(.1,"lines")) +
  theme_cmap(gridlines = "v") +
  facet_wrap(~income_c) +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 4, limits = c(0,.6)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_trips_p4,
              "Mode-share of K-12 school trips by household income category, 2008 vs. 2019.",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              height = 8,
              filename = "school_trips_p4",
              mode = "plot")

## What about travel times by income for school trips

# Filter data for MDT
school_time_mdt <-
  all_school_mdt %>%
  filter(
    # Only include trips that are more than 0 minutes and less than 2.5 hours
    travtime_pg < 150 & travtime_pg > 0,
    # Exclude households with missing income information
    income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(travtime = weighted.mean(travtime_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2019)")

# Repeat for TT
school_time_tt <-
  all_school_tt %>%
  filter(TRPDUR < 150 & TRPDUR > 0,
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(travtime = weighted.mean(TRPDUR, w = weight)) %>%
  mutate(survey = "Travel Tracker (2008)")

# Chart of travel time to school by household income
school_trips_p5 <-
  school_time_mdt %>%
  rbind(.,
        school_time_tt) %>%
  ggplot(aes(x = income_c, y = travtime, fill = reorder(survey,desc(survey)))) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None",
             axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~survey,ncol = 2) +
  cmap_fill_discrete(palette = "prosperity")

finalize_plot(school_trips_p5,
              "Travel time to school by household income (minutes).",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Trips with no travel time or lasting 150 minutes or
              more are excluded as outliers.
              <br><br>
              Source: CMAP analysis of MDT and TT.",
              filename = "school_trips_p5",
              mode = "plot")


## What about trip distances by income for school trips

# Repeat filtering from above (travel time) for trip distances
school_distance_mdt <-
  all_school_mdt %>%
  filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2019)")

school_distance_tt <-
  all_school_tt %>%
  filter(DIST > 0,
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(tripdist = weighted.mean(DIST, w = weight)) %>%
  mutate(survey = "Travel Tracker (2008)")

# Chart of trip distances by household income
school_trips_p6 <-
  school_distance_mdt %>%
  rbind(.,
        school_distance_tt) %>%
  ggplot(aes(x = income_c, y = tripdist, fill = reorder(survey,desc(survey)))) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None",
             axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~survey,ncol = 2) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(school_trips_p6,
              "Trip distance to school by household income (miles).",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old that are greater than 0 miles.
              <br><br>
              Source: CMAP analysis of MDT and TT.",
              filename = "school_trips_p6",
              mode = "plot")


## What about travel times by race for school trips

# Filter data for MDT
school_time_race_mdt <-
  all_school_mdt %>%
  filter(
    # Only include trips that are more than 0 minutes and less than 2.5 hours
    travtime_pg < 150 & travtime_pg > 0,
    # Exclude households with missing race and ethnicity information
    race_eth != "missing") %>%
  # Add breakdown between K-8 and 9-12
  mutate(k12 = ifelse(schol == 3,"Elementary and middle school","High school")) %>%
  group_by(race_eth,k12) %>%
  summarize(travtime = weighted.mean(travtime_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2019)")

# Chart of travel time to school by household income
school_trips_p7 <-
  school_time_race_mdt %>%
  mutate(label = round(travtime)) %>%
  mutate(race_eth = factor(race_eth, levels = c("black","hispanic","other","white","asian"))) %>%
  ggplot(aes(x = race_eth, y = travtime, fill = race_eth)) +
  geom_col() +
  theme_cmap(gridlines = "h",
             legend.position = "None") +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(travtime)),
             vjust = 0, label.size = 0, fill = "white") +
  facet_wrap(~k12) +
  cmap_fill_race()

finalize_plot(school_trips_p7,
              "Travel time to school by race and ethnicity (minutes).",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old. Trips with no travel time or lasting 150 minutes or
              more are excluded as outliers.
              <br><br>
              Source: CMAP analysis of MDT.",
              filename = "school_trips_p7",
              mode = "png",
              height = 6.3,
              width = 11.3,
              overwrite = T)


## What about trip distances by income for school trips

# Repeat filtering from above (travel time) for trip distances
school_distance_race_mdt <-
  all_school_mdt %>%
  filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
         race_eth != "missing") %>%
  group_by(race_eth) %>%
  summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2019)")

# Chart of trip distances by household income
school_trips_p8 <-
  school_distance_race_mdt %>%
  ggplot(aes(x = reorder(race_eth,desc(tripdist)), y = tripdist, fill = race_eth)) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None") +
  cmap_fill_race()

finalize_plot(school_trips_p8,
              "Trip distance to school by race and ethnicity (miles).",
              "Note: Includes trips for travelers enrolled in K-12 and at least
              5 years old that are greater than 0 miles.
              <br><br>
              Source: CMAP analysis of MDT and TT.",
              filename = "school_trips_p8",
              mode = "plot")

##### Run regressions on income and race and ethnicity

all_school_mdt_lm <-
  all_school_mdt %>%
  filter(travtime_pg < 150) %>%
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
      mode_c_school == "driver" | mode_c == "passenger" ~ 1,
      TRUE ~ 0),
    school_bus = case_when(
      mode_c_school == "school bus" ~ 1,
      TRUE ~ 0),
    transit = case_when(
      mode_c_school == "transit" ~ 1,
      TRUE ~ 0),
    walk = case_when(
      mode_c_school == "walk" ~ 1,
      TRUE ~ 0),
    bike = case_when(
      mode_c_school == "bike" ~ 1,
      TRUE ~ 0),
    cook = case_when(
      home_county == 31 ~ 1,
      TRUE ~ 0),
    k8 = case_when(
      schol == 3 ~ 1,
      TRUE ~ 0)
    )

school_trips_regression <-
  lm(travtime_pg ~ white + black + hispa + asian + high_inc + distance_pg + cook +
       car_trip + school_bus + transit + walk + bike + k8,
     all_school_mdt_lm,
     weights = wtperfin)

summary(school_trips_regression)


all_school_mdt_lm %>%
  ggplot(aes(x = travtime_pg, y = race_eth)) +
  geom_boxplot()



#######################################
# Look at geographic distribution of schools

## MDT

### Tracts
tract_sf <- sf::read_sf("V:/Demographic_and_Forecast/Census/2010/Geography/CMAP_Region_Projected/Tracts_CMAP_TIGER2010.shp")
county_sf <- sf::read_sf("V:/Administrative_and_Political_Boundaries/Counties/Cnty7_NIPC_05.shp")


# Identify the locations of schools
school_locations_mdt <-
  all_school_mdt %>%
  filter(school_county_fips %in% cmap_counties) %>%
  group_by(school_county_fips,tract_fips) %>%
  summarize(uw_school_trips = n(),
            w_school_trips = sum(wtperfin))


tract_schools_mdt <-
  tract_sf %>%
  mutate(TRACTCE10 = as.integer(TRACTCE10),
         COUNTYFP10 = as.integer(COUNTYFP10)) %>%
  left_join(school_locations_mdt, by = c("TRACTCE10" = "tract_fips", "COUNTYFP10" = "school_county_fips")) %>%
  select(TRACTCE10,COUNTYFP10,w_school_trips,uw_school_trips,geometry)

number_of_tracts_with_schools_mdt <-
  school_locations_mdt %>%
  filter(uw_school_trips > 0) %>%
  group_by(school_county_fips) %>%
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
  # scale_fill_binned(breaks = c(5,10,25,50,100)) +
  scale_fill_binned(type = "viridis")

finalize_plot(school_trips_map1,
              title = "Number of school trips per tract.",
              caption = "Source: CMAP analysis of MDT data.",
              legend_shift = FALSE,
              height = 9,
              filename = "school_trips_map1",
              # mode = "png"
)

### Counties

school_county_locations_mdt <-
  all_school_mdt %>%
  group_by(school_county_fips) %>%
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
  left_join(school_county_locations_mdt, by = c("FIPSCNTY" = "school_county_fips"))


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
              filename = "school_trips_map2",
              # mode = "png"
)


## TT

### Tracts
tract_sf_tt <- sf::read_sf("V:/Demographic_and_Forecast/Census/2000/Geography/TractILne11co_Census_2000.shp") %>%
  filter(FIPSSTCO %in% cmap_state_counties)


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
  left_join(school_locations_tt, by = c("FIPSSTCO" = "school_county_fips", "TRACTID" = "tract_fips")) %>%
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
  left_join(school_county_locations_tt, by = c("FIPSCNTY" = "school_county_fips"))


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
  filter(school_county_fips %in% cmap_counties) %>%
  mutate(school_county_fips = as.integer(sub("17","",school_county_fips))) %>%
  mutate(survey = "mdt") %>%
  rbind(school_county_locations_tt %>% mutate(survey = "tt")) %>%
  select(-name,-value,-total_w,-total_uw) %>%
  distinct() %>%
  pivot_longer(cols = ends_with("school_trips")) %>%
  rbind(
    rbind(number_of_tracts_with_schools_mdt %>%
            mutate(survey = "mdt"),
          number_of_tracts_with_schools_tt %>%
            mutate(survey = "tt")) %>%
      mutate(name = "Tracts with schools") %>%
      rename(value = n)) %>%
  mutate(school_county_fips = recode(school_county_fips,
                       "31" = "Cook",
                       "43" = "DuPage",
                       "89" = "Kane",
                       "93" = "Kendall",
                       "97" = "Lake",
                       "111" = "McHenry",
                       "197" = "Will"))


school_trips_p9 <-
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
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "v",
             axis.text.x = element_text(angle = 90)) +
  geom_label(aes(label = scales::label_comma(accuracy = 1)(value),
                 group = survey),
             position = position_dodge2(width = 0.9),
             label.size = 0,
             hjust = 0,
             label.padding = unit(.05,"lines"),
             fill = "white") +
  scale_x_continuous(label = scales::label_comma()) +
  geom_blank(aes(x = xmax)) +
  facet_wrap(~name,ncol = 3,scales = "free_x")

finalize_plot(school_trips_p9,
              title = "Overview of school trip data distribution.",
              caption = "Source: CMAP analysis of MDT and TT data.",
              width = 11.3,
              title_width = 1.5,
              height = 6.3,
              mode = "png",
              filename = "school_trips_p9",
              overwrite = T)
