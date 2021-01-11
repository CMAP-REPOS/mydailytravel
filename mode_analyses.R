

library(ggplot2)
library(tidyverse)
library(slider)
library(cmapplot)


#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

#### Mode breakdown of different dining trips


## Create totals for trips by mode category (within universe of dining trips)

### Filter data
all_dining_mdt <- mdt %>%
  filter(age < 90 & age >= 5, distance_pg > 0,
         !(mode_c %in% c("missing","beginning")),
         tpurp_c == "dining")

all_dining_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5, DIST > 0,
         mode_c != "missing",
         tpurp_c == "dining")

### Calculate proportions for TT
all_dining_mode_c_tt <- all_dining_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_dining_mode_c_mdt <- all_dining_mdt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_dining_mode_c <-
  rbind(all_dining_mode_c_tt,
        all_dining_mode_c_mdt) %>%
  mutate(tpurp = "Dining outside of home (all)")


### Calculate proportions for subcategories for dining in MDT
detailed_dining_totals_mdt <- all_dining_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_dining_mode_c_mdt <- all_dining_mdt %>%
  group_by(tpurp,mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_dining_totals_mdt,by = "tpurp") %>%
  mutate(mode_c_pct = mode_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_dining_mode_c <-
  rbind(total_dining_mode_c,
        detailed_dining_mode_c_mdt)


dining_plot <-
  all_dining_mode_c %>%
  mutate(tpurp = factor(tpurp,levels = c("Dining outside of home (all)",
                                         "Drive thru / take-out dining",
                                         "Ate / dined out")),
         survey = factor(survey, levels = c("tt","mdt"))) %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = tpurp)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  facet_wrap(~survey,ncol = 1) +
  theme_cmap(gridlines = "v") +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(dining_plot,
              "Mode share of dining trips, 2008 vs. 2019.",
              "Source: CMAP analysis of MDT and TT data.")





#### Mode breakdown of different healthcare trips


## Create totals for trips by mode category (within universe of health trips)

### Filter data
all_health_mdt <- mdt %>%
  filter(age < 90 & age >= 5,
         !(mode_c %in% c("missing","beginning")),
         tpurp_c == "health")

all_health_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5,
         mode_c != "missing",
         tpurp_c == "health")

### Calculate proportions for TT
all_health_mode_c_tt <- all_health_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_health_mode_c_mdt <- all_health_mdt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_health_mode_c <-
  rbind(all_health_mode_c_tt,
        all_health_mode_c_mdt) %>%
  mutate(tpurp = "Healthcare (all)")


### Calculate proportions for subcategories for health in MDT
detailed_health_totals_mdt <- all_health_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_health_mode_c_mdt <- all_health_mdt %>%
  group_by(tpurp,mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_health_totals_mdt,by = "tpurp") %>%
  mutate(mode_c_pct = mode_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_health_mode_c <-
  rbind(total_health_mode_c,
        detailed_health_mode_c_mdt)


health_plot <-
  all_health_mode_c %>%
  mutate(tpurp = factor(tpurp,levels = c("Healthcare (all)",
                                         "Health care visit for self",
                                         "Health care visit for someone else",
                                         "Visited a person staying at the hospital")),
         survey = factor(survey, levels = c("tt","mdt"))) %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel (2019)",
                                tt = "Travel Tracker (2008)")) %>%
  ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = tpurp)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  facet_wrap(~survey,ncol = 1) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(health_plot,
              "Mode share of health trips, 2008 vs. 2019.",
              "Source: CMAP analysis of MDT and TT data.")





#### Mode breakdown of different community trips


## Create totals for trips by mode category (within universe of community trips)

### Filter data
all_community_mdt <- mdt %>%
  filter(age < 90 & age >= 5, distance_pg > 0,
         !(mode_c %in% c("missing","beginning")),
         tpurp_c == "community")

all_community_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5, DIST > 0,
         mode_c != "missing",
         tpurp_c == "community")

### Calculate proportions for TT
all_community_mode_c_tt <- all_community_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_community_mode_c_mdt <- all_community_mdt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "mdt")

### Join MDT and TT
total_community_mode_c <-
  rbind(all_community_mode_c_tt,
        all_community_mode_c_mdt) %>%
  mutate(tpurp = "Community (all)")


### Calculate proportions for subcategories for community in TT
detailed_community_totals_tt <- all_community_tt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(weight))

detailed_community_mode_c_tt <- all_community_tt %>%
  group_by(tpurp,mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  left_join(.,detailed_community_totals_tt,by = "tpurp") %>%
  mutate(mode_c_pct = mode_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "tt")

### Calculate proportions for subcategories for community in MDT
detailed_community_totals_mdt <- all_community_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_community_mode_c_mdt <- all_community_mdt %>%
  group_by(tpurp,mode_c) %>%
  summarize(mode_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_community_totals_mdt,by = "tpurp") %>%
  mutate(mode_c_pct = mode_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_community_mode_c <-
  rbind(total_community_mode_c,
        detailed_community_mode_c_mdt,
        detailed_community_mode_c_tt)


community_plot <-
  all_community_mode_c %>%
  mutate(tpurp = factor(tpurp,levels = c("Community (all)",
                                         "Visit friends/relatives",
                                         "Socialized with friends",
                                         "Socialized with relatives",
                                         "Civic/religious activities",
                                         "Attended a community event",
                                         "Attended a religious event")),
         survey = factor(survey, levels = c("tt","mdt")),
         category = recode_factor(tpurp,
                                  "Community (all)" = "Overall",
                                  "Visit friends/relatives" = "Friends/Family",
                                  "Socialized with friends" = "Friends/Family",
                                  "Socialized with relatives" = "Friends/Family",
                                  "Civic/religious activities" = "Civic/Religious",
                                  "Attended a community event" = "Civic/Religious",
                                  "Attended a religious event" = "Civic/Religious"

         )) %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel",
                                tt = "Travel Tracker")) %>%
  ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = tpurp)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  facet_wrap(survey~category) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(community_plot,
              "Mode share of community trips, 2008 vs. 2019.",
              "Source: CMAP analysis of MDT and TT data.",
              title_width = 1.8,
              width = 10)



#### Purpose breakdown of carpooling vs. passenger


## Create totals for trips by purpose category (within universe of passenger trips)

### Filter data
all_passenger_mdt <- mdt %>%
  filter(age < 90 & age >= 5, distance_pg > 0,
         mode_c == "passenger",
         tpurp_c != "Missing")

all_passenger_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5, DIST > 0,
         mode_c == "passenger",
         tpurp_c != "Missing")

### Calculate proportions for TT
all_passenger_tpurp_c_tt <- all_passenger_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_passenger_tpurp_c_mdt <- all_passenger_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "mdt")

### Join MDT and TT
total_passenger_tpurp_c <-
  rbind(all_passenger_tpurp_c_tt,
        all_passenger_tpurp_c_mdt) %>%
  mutate(mode = "Passenger (all)")


### Calculate proportions for subcategories for community in MDT
detailed_passenger_totals_mdt <- all_passenger_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin))

detailed_passenger_tpurp_c_mdt <- all_passenger_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_passenger_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_passenger_tpurp_c <-
  rbind(total_passenger_tpurp_c,
        detailed_passenger_tpurp_c_mdt)


passenger_plot <-
  all_passenger_tpurp_c %>%
  filter(tpurp_c != "missing") %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Passenger (all)","carpool","personal auto (passenger)"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  facet_wrap(~survey,ncol = 1) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(passenger_plot,
              "Trip purposes of passenger trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              title_width = 1.8,
              width = 10,
              height = 8)


passenger_totals_plot <-
  all_passenger_tpurp_c %>%
  filter(!(survey == "mdt" & mode == "Passenger (all)")) %>%
  group_by(survey,mode) %>%
  summarize(total = sum(tpurp_c_total)) %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Passenger (all)","carpool","personal auto (passenger)"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = total, fill = mode)) +
  geom_col() +
  theme_cmap() +
  cmap_fill_discrete(palette = "governance",reverse = TRUE) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))

finalize_plot(passenger_totals_plot,
              "Change in daily automobile passenger trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.")




#### Purpose breakdown of bike trips (shared vs. personal)


## Create totals for trips by purpose category (within universe of bike trips)

### Filter data
all_bike_mdt <- mdt %>%
  filter(age < 90 & age >= 5, distance_pg > 0,
         mode_c == "bike",
         tpurp_c != "Missing")

all_bike_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5, DIST > 0,
         mode_c == "bike",
         tpurp_c != "Missing")

### Calculate proportions for TT
all_bike_tpurp_c_tt <- all_bike_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_bike_tpurp_c_mdt <- all_bike_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "mdt")

### Join MDT and TT
total_bike_tpurp_c <-
  rbind(all_bike_tpurp_c_tt,
        all_bike_tpurp_c_mdt) %>%
  mutate(mode = "Bike (all)")


### Calculate proportions for subcategories for community in MDT
detailed_bike_totals_mdt <- all_bike_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin))

detailed_bike_tpurp_c_mdt <- all_bike_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_bike_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_bike_tpurp_c <-
  rbind(total_bike_tpurp_c,
        detailed_bike_tpurp_c_mdt)


bike_plot <-
  all_bike_tpurp_c %>%
  filter(tpurp_c != "missing") %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Bike (all)","bike share","personal bike"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  facet_wrap(~survey,ncol = 1) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(bike_plot,
              "Trip purposes of bike trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'bike share' category, and so
              'Bike (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              title_width = 1.8,
              width = 10,
              height = 8)


bike_totals_plot <-
  all_bike_tpurp_c %>%
  filter(!(survey == "mdt" & mode == "Bike (all)")) %>%
  group_by(survey,mode) %>%
  summarize(total = sum(tpurp_c_total)) %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Bike (all)","bike share","personal bike"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = total, fill = mode)) +
  geom_col() +
  theme_cmap() +
  cmap_fill_discrete(palette = "governance",reverse = TRUE) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))

finalize_plot(bike_totals_plot,
              "Change in daily bike trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'bike share' category, and so
              'Bike (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.")



#### Purpose breakdown of rideshare vs. shared rideshare


## Create totals for trips by purpose category (within universe of rideshare trips)

### Filter data
all_tnc_mdt <- mdt %>%
  filter(age < 90 & age >= 5, distance_pg > 0,
         mode %in% c("rideshare","shared rideshare","taxi"),
         tpurp_c != "Missing")

all_tnc_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5, DIST > 0,
         MODE == "taxi",
         tpurp_c != "Missing")

### Calculate proportions for TT
all_tnc_tpurp_c_tt <- all_tnc_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_tnc_tpurp_c_mdt <- all_tnc_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "mdt")

### Join MDT and TT
total_tnc_tpurp_c <-
  rbind(all_tnc_tpurp_c_tt,
        all_tnc_tpurp_c_mdt) %>%
  mutate(mode = case_when(
    survey == "mdt" ~ "tnc (all)",
    TRUE ~ "taxi"))


### Calculate proportions for subcategories for community in MDT
detailed_tnc_totals_mdt <- all_tnc_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin))

detailed_tnc_tpurp_c_mdt <- all_tnc_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_tnc_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_tnc_tpurp_c <-
  rbind(total_tnc_tpurp_c,
        detailed_tnc_tpurp_c_mdt)


tnc_plot <-
  all_tnc_tpurp_c %>%
  filter(tpurp_c != "missing",
         survey == "mdt") %>%
  mutate(mode = factor(mode, levels = c("tnc (all)","taxi","shared rideshare","rideshare"))) %>%
  ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(tnc_plot,
              "Trip purposes of TNC and taxi trips, 2019.",
              "Source: CMAP analysis of MDT and TT data.",
              title_width = 1.8,
              width = 10)


tnc_totals_plot <-
  all_tnc_tpurp_c %>%
  filter(!(survey == "mdt" & mode == "tnc (all)")) %>%
  group_by(survey,mode) %>%
  summarize(total = sum(tpurp_c_total)) %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("shared rideshare","rideshare","taxi"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = total, fill = mode)) +
  geom_col() +
  theme_cmap() +
  cmap_fill_discrete(palette = "governance",reverse = TRUE) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))

finalize_plot(tnc_totals_plot,
              "Change in daily TNC and taxi trips, 2008 vs. 2019.",
              "Source: CMAP analysis of MDT and TT data.")



##### Examination of TNC school trips

### Filter data
all_tnc_school_mdt <- mdt %>%
  filter(age < 18 & age >= 5,  distance_pg > 0,
         mode %in% c("rideshare","shared rideshare","taxi"),
         tpurp_c == "school")

all_tnc_school_mdt %>%
  group_by(income_c, mode) %>%
  summarize(trips = sum(wtperfin))

