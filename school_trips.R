
library(ggplot2)
library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

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
    distance_pg > 0,             # 14,032 records
    mode_c != "missing",         # 14,032 records
    mode_c != "beginning",       # 14,032 records
    tpurp_c == "school"          # 4,421 records
    ) %>%
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
all_school_tt <-
  tt %>%                      # 140,751 records
  filter(
    # Keep records of travelers enrolled in K-12
    SCHOL %in% c(3,4),        # 18,645 records
    # Keep only those 5 or older
    AGE >= 5,                 # 4,421 records
    DIST > 0,                 # 4,421 records
    tpurp_c == "school"       # 3,335 records
    ) %>%
  mutate(mode_c_school = as.character(mode_c)) %>%
  mutate(mode_c_school = case_when(
    MODE == "school bus" ~ "school bus",
    TRUE ~ mode_c_school
  )) %>%
  mutate(mode_c_school = factor(mode_c_school))


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
                                tt = "Travel Tracker (2008)"),
         label = paste0(format(round(mode_c_pct*100,1),nsmall = 1),"%")) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
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

# Chart of absolute numbers of school trips, MDT vs TT
school_trips_p2 <-
  total_school_mode_c %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)"),
         label = round(mode_c_total / 100)) %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_total)), x = mode_c_total, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
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
  group_by(income_c) %>%
  summarize(total = sum(weight))

all_school_inc_mode_c_tt <-
  all_school_tt %>%
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
  group_by(income_c) %>%
  summarize(total = sum(wtperfin))

all_school_inc_mode_c_mdt <-
  all_school_mdt %>%
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
                                tt = "Travel Tracker (2008)"),
         label = paste0(format(round(mode_c_pct*100,1),nsmall = 1),"%")) %>%
  filter(income_c != "missing",
         mode_c_school == "walk"
         ) %>%
  ggplot(aes(y = income_c, x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6, limits = c(0,.45)) +
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
                                tt = "Travel Tracker (2008)"),
         label = paste0(format(round(mode_c_pct*100,1),nsmall = 1),"%")) %>%
  filter(income_c != "missing") %>%
  ggplot(aes(y = reorder(mode_c_school,desc(-mode_c_pct)), x = mode_c_pct, fill = survey)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
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
  group_by(race_eth) %>%
  summarize(travtime = weighted.mean(travtime_pg, w = wtperfin)) %>%
  mutate(survey = "My Daily Travel (2019)")

# Chart of travel time to school by household income
school_trips_p7 <-
  school_time_race_mdt %>%
  mutate(label = round(travtime)) %>%
  ggplot(aes(x = reorder(race_eth,desc(travtime)), y = travtime, fill = race_eth)) +
  geom_col(position = position_dodge2()) +
  theme_cmap(gridlines = "h",
             legend.position = "None") +
  geom_text(aes(label = label),vjust = 0) +
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
      TRUE ~ 0))

school_trips_regression <-
  lm(travtime_pg ~ white + black + hispa + asian + high_inc + distance_pg + cook +
       car_trip + school_bus + transit + walk + bike,
     all_school_mdt_lm,
     weights = wtperfin)

summary(school_trips_regression)


all_school_mdt_lm %>%
  ggplot(aes(x = travtime_pg, y = race_eth)) +
  geom_boxplot()
