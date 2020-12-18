
library(ggplot2)
library(tidyverse)
library(cmapplot)



#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Load My Daily Travel
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, distance, arrtime,
         deptime, travtime, tpurp)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, hisp, race, pertrips, wtperfin)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region, county_fips, home)

# home location flag
home_wip <- region %>%
  filter(home == 1) %>% # identify home locations
  select(sampno,
         home_county = county_fips) %>%
  distinct() # keep distinct home locations based on sample

# Some households are coded as having home locations in multiple counties. Identify them.
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace multi-county samples with a county_fips of 999
home <- home_wip %>%
  mutate(home_county = case_when(
    sampno %in% two_homes ~ 999,
    TRUE ~ home_county)) %>%
  distinct()

# merge datasets
mdt <- trips %>%
  inner_join(ppl, by = c("sampno", "perno")) %>%
  inner_join(hh, by = "sampno") %>%
  inner_join(region, by = c("sampno", "locno")) %>%
  inner_join(home, by = c("sampno")) %>%
  filter(out_region==0 & distance<100)

# add combined duration and distance for placeGroup trips
placeGroupStats <- mdt %>%
  filter(hdist >= 0,
         distance >= 0,
         travtime >= 0) %>%
  group_by(sampno,perno,placeGroup) %>%
  summarize(hdist_pg = sum(hdist, na.rm = TRUE),
            distance_pg = sum(distance, na.rm = TRUE),
            travtime_pg = sum(travtime, na.rm = TRUE))

# take care of collapsed trips with placeGroup
mdt <- mdt %>%
  # distinct takes the first row for duplicates, so order by distance to get right mode
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>%
  # add combined distance and time values calculated above
  left_join(.,placeGroupStats, by = c("sampno","perno","placeGroup"))


# Load Travel Tracker
# Downloaded from CMAP data portal; exported from Microsoft Access database to csv.
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2008 survey")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(SAMPN, SURVEY, ASSN, DAY, HHVEH, INCOM)

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN, PERNO, SURVEY, AGE, HISP, RACE, WGTP)

# trips
#   day beginning/place #1 already null in mode var
tt_place <- read_csv("place_public.csv") %>%
  select(MPO, SAMPN, PERNO, DAYNO, PLANO, locno, TPURP, MODE, DIST, TRPDUR)

# Combine datasets
tt <- tt_place %>%
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>%
  inner_join(tt_hh, by = c("SAMPN", "SURVEY"))

# Flag weekend trips and adjust weights accordingly
tt <- tt %>%
  mutate(weekend = if_else(SURVEY==2 & DAY==5 & DAYNO==2, 1,
                           if_else(SURVEY==2 & DAY==7 & DAYNO==1, 1, 0)),
         weekdays2 = if_else(SURVEY==2 & DAY==1 |
                               SURVEY==2 & DAY==2 |
                               SURVEY==2 & DAY==3 |
                               SURVEY==2 & DAY==4, 1, 0),
         # If respondent recorded two weekdays, divide weight in half
         weight = if_else(weekdays2==1, WGTP/2, WGTP))


#   Remove trips ending outside the region, over 100 miles, and/or on weekends
tt <- tt %>%
  filter(MPO==1 & DIST<100 & weekend==0)


# recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode = factor(mode),
         mode = recode_factor(mode,
                              !!!recode_mode_detailed_mdt))%>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt))

tt <- tt %>%
  mutate(MODE = factor(MODE),
         MODE = recode_factor(MODE,
                              !!!recode_mode_detailed_tt)) %>%
  mutate(mode_c = fct_collapse(MODE,
                               !!!recode_mode_buckets_tt))



# Recode trip purposes and group into buckets for comparison
mdt <- mdt %>%
  mutate(tpurp = factor(tpurp)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_mdt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_mdt))

tt <- tt %>%
  mutate(tpurp = factor(TPURP)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_tt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_tt))


# Recode incomes and group into buckets for comparison
mdt <- mdt %>%
  mutate(income = factor(hhinc),
         income = recode_factor(income,!!!recode_income_detailed_mdt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt <- tt %>%
  mutate(income = factor(INCOM),
         income = recode_factor(income,!!!recode_income_detailed_tt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_tt))



# Recode into race and ethnicity groups
mdt <- mdt %>%
  mutate(race_eth = recode(race,
                           "1" = "white",
                           "2" = "black",
                           "3" = "asian",
                           "4" = "other",
                           "5" = "other",
                           "6" = "other",
                           "97" = "other",
                           "-8" = "missing",
                           "-7" = "missing")) %>%
  mutate(race_eth = case_when(
    hisp == 1 ~ "hispanic",
    hisp == -7 | hisp == -8 ~ "missing",
    TRUE ~ race_eth))


setwd("~/GitHub/mydailytravel")


#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################



## Create totals for trips by mode category (within universe of school trips)

### Filter data
all_school_mdt <- mdt %>%
  filter(
    # Keep only trips by school-aged travelers
    age <= 18 & age >= 5,
    # Remove trips with modes either beginning or missing
    !(mode_c %in% c("missing","beginning")),
    # Keep only school trips
    tpurp_c == "school") %>%
  # Mutate to character to allow case_when modification
  mutate(mode_c_school = as.character(mode_c)) %>%
  # Separate out school buses from "other"
  mutate(mode_c_school = case_when(
    mode == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
  )) %>%
  # Reconvert to factor
  mutate(mode_c_school = factor(mode_c_school))

# Repeat same filtering and mutation for TT
all_school_tt <- tt %>%
  filter(AGE <= 18 & AGE >= 5,
         mode_c != "missing",
         tpurp_c == "school") %>%
mutate(mode_c_school = as.character(mode_c)) %>%
  mutate(mode_c_school = case_when(
    MODE == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
  )) %>%
  mutate(mode_c_school = factor(mode_c_school))

### Calculate proportions for TT
all_school_mode_c_tt <- all_school_tt %>%
  group_by(mode_c_school) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_school_mode_c_mdt <- all_school_mdt %>%
  group_by(mode_c_school) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_school_mode_c <-
  rbind(all_school_mode_c_tt,
        all_school_mode_c_mdt)

# Chart of mode share for K-12 trips, MDT vs TT
school_pct_plot <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2018)",
                                tt = "Travel Tracker (2008)"),
         label = paste0(as.character(round(mode_c_pct*100,1)),"%")) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6, limits = c(0,.45)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_pct_plot,
              "Mode share of K-12 school trips, 2018 vs. 2018.",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive). Overall school trips increased ~3% between the two
              surveys. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.")

# Chart of absolute numbers of school trips, MDT vs TT
school_abs_plot <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2018)",
                                tt = "Travel Tracker (2008)"),
         label = as.character(round(mode_c_total / 100))) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_total)), x = mode_c_total, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_comma(scale = .001),n.breaks = 6, limits=c(0,600000)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_abs_plot,
              "Total number of K-12 school trips by mode, 2018 vs. 2018 (in thousands).",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive). Overall school trips increased ~3% between the two
              surveys. Travel Tracker had two school-related trip categories
              (both included) while My Daily Travel had only one.
              <br><br>
              Source: CMAP analysis of MDT and TT data.")

### Look at it by income



## Create totals for trips by mode category (within universe of school trips)

### Calculate proportions for TT

# Total trips by income bucket
all_school_inc_total_tt <- all_school_tt %>%
  group_by(income_c) %>%
  summarize(total = sum(weight))

all_school_inc_mode_c_tt <- all_school_tt %>%
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
all_school_inc_total_mdt <- all_school_mdt %>%
  group_by(income_c) %>%
  summarize(total = sum(wtperfin))

all_school_inc_mode_c_mdt <- all_school_mdt %>%
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
school_pct_inc_plot1 <-
  total_school_inc_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2018)",
                                tt = "Travel Tracker (2008)"),
         label = paste0(as.character(round(mode_c_pct*100,1)),"%")) %>%
  filter(income_c != "missing",
         mode_c_school == "walk"
         ) %>%
  ggplot(aes(y = income_c, x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6, limits = c(0,.45)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_pct_inc_plot1,
              "Walk mode-share of K-12 school trips by household income category, 2018 vs. 2018.",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive).
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              height = 5)

# Chart of mode share by income category
school_pct_inc_plot2 <-
  total_school_inc_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2018)",
                                tt = "Travel Tracker (2008)"),
         label = paste0(as.character(round(mode_c_pct*100,1)),"%")) %>%
  filter(income_c != "missing") %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  facet_wrap(~income_c) +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 4, limits = c(0,.55)) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(school_pct_inc_plot2,
              "Mode-share of K-12 school trips by household income category, 2018 vs. 2018.",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive).
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              height = 8)

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
  mutate(survey = "My Daily Travel (2018)")

# Repeat for TT
school_time_tt <-
  all_school_tt %>%
  filter(TRPDUR < 150 & TRPDUR > 0,
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(travtime = weighted.mean(TRPDUR, w = weight)) %>%
  mutate(survey = "Travel Tracker (2008)")

# Chart of travel time to school by household income
school_time_plot <-
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

finalize_plot(school_time_plot,
              "Travel time to school by household income (minutes).",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive). Trips with no travel time or lasting 150 minutes or
              more are excluded as outliers.
              <br><br>
              Source: CMAP analysis of MDT and TT.")


## What about trip distances by income for school trips

# Repeat filtering from above (travel time) for trip distances
school_distance_mdt <-
  all_school_mdt %>%
  filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2018)")

school_distance_tt <-
  all_school_tt %>%
  filter(DIST > 0,
         income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(tripdist = weighted.mean(DIST, w = weight)) %>%
  mutate(survey = "Travel Tracker (2008)")

# Chart of trip distances by household income
school_distance_plot <-
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

finalize_plot(school_distance_plot,
              "Trip distance to school by household income (miles).",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive) that are greater than 0 miles.
              <br><br>
              Source: CMAP analysis of MDT and TT.")


## What about travel times by race for school trips

# Filter data for MDT
school_time_race_mdt <-
  all_school_mdt %>%
  filter(
    # Only include trips that are more than 0 minutes and less than 2.5 hours
    travtime_pg < 150 & travtime_pg > 0,
    # Exclude households with missing race and ethnicity information
    race_eth != "missing") %>%
  group_by(race_eth) %>%
  summarize(travtime = weighted.mean(travtime_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2018)")

# Chart of travel time to school by household income
school_time_race_plot <-
  school_time_race_mdt %>%
  ggplot(aes(x = reorder(race_eth,desc(travtime)), y = travtime)) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None")

finalize_plot(school_time_race_plot,
              "Travel time to school by race and ethnicity (minutes).",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive). Trips with no travel time or lasting 150 minutes or
              more are excluded as outliers.
              <br><br>
              Source: CMAP analysis of MDT.")


## What about trip distances by income for school trips

# Repeat filtering from above (travel time) for trip distances
school_distance_race_mdt <-
  all_school_mdt %>%
  filter(distance_pg > 0, # use distance_pg as it is the consolidated distance from the placegroup
         race_eth != "missing") %>%
  group_by(race_eth) %>%
  summarize(tripdist = weighted.mean(distance_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2018)")

# Chart of trip distances by household income
school_distance_race_plot <-
  school_distance_race_mdt %>%
  ggplot(aes(x = reorder(race_eth,desc(tripdist)), y = tripdist)) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None")

finalize_plot(school_distance_race_plot,
              "Trip distance to school by race and ethnicity (miles).",
              "Note: Includes trips for travelers between 5 and 18 years old
              (inclusive) that are greater than 0 miles.
              <br><br>
              Source: CMAP analysis of MDT and TT.")

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
      TRUE ~ 0))

school_trips_regression <-
  lm(travtime_pg ~ white + high_inc + distance_pg + cook +
       car_trip + school_bus + transit + walk + bike,
     all_school_mdt_lm,
     weights = wtperfin)

summary(school_trips_regression)


all_school_mdt_lm %>%
  ggplot(aes(x = travtime_pg, y = race_eth)) +
  geom_boxplot()
