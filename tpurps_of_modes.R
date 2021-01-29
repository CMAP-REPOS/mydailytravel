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

# Create base dataset for mode analyses

mdt_base_2 <-
  mdt %>%                        # 125,103 records
  filter(age < 90,               # 125,006 records
         age >= 5 |              # 125,002 records
           aage %in% c(2,3,4,5,6,7) |
           schol %in% c(4,5,6,7,8) |
           sampno %in% c(70038312,
                         70051607),
         distance_pg > 0,        # 96,857 records
         tpurp_c != "missing"   # 96,788 records
         ) %>%
  mutate(under18 = ifelse(age >= 18, "18 and over", "Under 18"))


tt_base_2 <-
  tt %>%                       # 140,751 records
  filter(AGE < 90,             # 137,844 records
         AGE >= 5 |            # 131,082 records
           SCHOL %in% c(4,5,6,7,8),
         DIST > 0,             # 98,800 records
         tpurp_c != "missing"  # 98,800 records
         ) %>%
  mutate(under18 = ifelse(AGE >= 18, "18 and over", "Under 18"))

#####################################################
# Purpose breakdown of carpooling vs. passenger

## Create totals for trips by purpose category (within universe of passenger trips)

### Filter data
all_passenger_mdt <-
  mdt_base_2 %>%                # 96,788 records
  filter(mode_c == "passenger") # 14,860 records

all_passenger_tt <-
  tt_base_2 %>%                 # 98,800 records
  filter(mode_c == "passenger") # 16,991 records

### Calculate proportions for TT
all_passenger_tpurp_c_tt <-
  all_passenger_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_passenger_tpurp_c_mdt <-
  all_passenger_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "mdt")

### Join MDT and TT
total_passenger_tpurp_c <-
  rbind(all_passenger_tpurp_c_tt,
        all_passenger_tpurp_c_mdt) %>%
  mutate(mode = "Passenger (all)")


### Calculate proportions for subcategories for driver/passenger in MDT
detailed_passenger_totals_mdt <-
  all_passenger_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin))

detailed_passenger_tpurp_c_mdt <-
  all_passenger_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_passenger_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_passenger_tpurp_c <-
  rbind(total_passenger_tpurp_c,
        detailed_passenger_tpurp_c_mdt)


tpurps_of_modes_p1 <-
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

finalize_plot(tpurps_of_modes_p1,
              "Trip purposes of passenger trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              title_width = 1.8,
              width = 11.3,
              height = 6.3,
              filename = "tpurps_of_modes_p1")


tpurps_of_modes_p2 <-
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

finalize_plot(tpurps_of_modes_p2,
              "Change in daily automobile passenger trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "tpurps_of_modes_p2",
              height = 6.3,
              width = 11.3,
              mode = "png"
              )

### Repeat calculations but with flags for under 18

### Calculate proportions for subcategories for driver/passenger in MDT
detailed_passenger_totals_under18_mdt <-
  all_passenger_mdt %>%
  group_by(mode,under18) %>%
  summarize(trip_total = sum(wtperfin)) %>%
  mutate(survey = "mdt")

passenger_totals_under18_tt <-
  all_passenger_tt %>%
  mutate(MODE = "Passenger (all)") %>%
  rename(mode = MODE) %>%
  group_by(mode,under18) %>%
  summarize(trip_total = sum(weight)) %>%
  mutate(survey = "tt")


all_passenger_under18 <-
  rbind(passenger_totals_under18_tt,
        detailed_passenger_totals_under18_mdt)


tpurps_of_modes_p2a <-
  all_passenger_under18 %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Passenger (all)","carpool","personal auto (passenger)"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = trip_total, fill = mode)) +
  geom_col() +
  theme_cmap() +
  facet_wrap(~under18, ncol = 1) +
  cmap_fill_discrete(palette = "governance",reverse = TRUE) +
  scale_y_continuous(labels = scales::label_comma(scale = 1))

finalize_plot(tpurps_of_modes_p2a,
              "Change in daily automobile passenger trips, 2008 vs. 2019, by age.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "tpurps_of_modes_p2a",
              height = 6.3,
              width = 11.3,
              mode = "png"
)


#########################################################
# Purpose breakdown of bike trips (shared vs. personal)


## Create totals for trips by purpose category (within universe of bike trips)

### Filter data
all_bike_mdt <-
  mdt_base_2 %>%           # 96,788 records
  filter(mode_c == "bike") # 1,689 records

all_bike_tt <-
  tt_base_2 %>%            # 98,800 records
  filter(mode_c == "bike") # 942 records

### Calculate proportions for TT
all_bike_tpurp_c_tt <-
  all_bike_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_bike_tpurp_c_mdt <-
  all_bike_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "mdt")

### Join MDT and TT
total_bike_tpurp_c <-
  rbind(all_bike_tpurp_c_tt,
        all_bike_tpurp_c_mdt) %>%
  mutate(mode = "Bike (all)")


### Calculate proportions for subcategories for biking in MDT
detailed_bike_totals_mdt <-
  all_bike_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin),
            n = n())

detailed_bike_tpurp_c_mdt <-
  all_bike_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_bike_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_bike_tpurp_c <-
  rbind(total_bike_tpurp_c,
        detailed_bike_tpurp_c_mdt)


tpurps_of_modes_p3 <-
  all_bike_tpurp_c %>%
  filter(tpurp_c != "missing") %>%
  filter(survey == "mdt",
         mode != "Bike (all)") %>%
  # mutate(survey = factor(survey,levels = c("tt","mdt")),
  #        mode = factor(mode, levels = c("Bike (all)","bike share","personal bike"))) %>%
  # mutate(survey = recode_factor(survey,
  #                               "tt" = "Travel Tracker ('08)",
  #                               "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  # facet_wrap(~survey,ncol = 1) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(tpurps_of_modes_p3,
              "Trip purposes of bike trips in 2019, personal vs. bike share",
              "Source: CMAP analysis of MDT data.",
              title_width = 1.8,
              width = 11.3,
              height = 6.3,
              filename = "tpurps_of_modes_p3",
              mode = "png",
              overwrite = T)


tpurps_of_modes_p4 <-
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

finalize_plot(tpurps_of_modes_p4,
              "Change in daily bike trips, 2008 vs. 2019.",
              "Note: Travel Tracker did not have a 'bike share' category, and so
              'Bike (all)' includes both types of trips.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              filename = "tpurps_of_modes_p4",
              height = 6.3,
              width = 11.3,
              mode = "png")


### Bike mode share of all travel


### Calculate total number of trips
all_purp_mdt <-
  mdt_base_2 %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin))

all_purp_tt <-
  tt_base_2 %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight))


# Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
bike_purp_mdt <-
  all_bike_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_count = sum(wtperfin)) %>%
  inner_join(., all_purp_mdt, by = "tpurp_c") %>%
  mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
         survey = "mdt") %>% # add identifier
  arrange(tpurp_c_pct) # sort by mode share

bike_purp_tt <-
  all_bike_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_count = sum(weight)) %>%
  inner_join(., all_purp_tt, by = "tpurp_c") %>%
  mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
         survey = "tt") %>% # add identifier
  arrange(tpurp_c_pct) # sort by mode share

# Combine data from MDT and TT
bike_purp <-
  rbind(bike_purp_mdt,bike_purp_tt) %>%
  select(-tpurp_c_total) # Remove total by purpose (now that bike share is calculated)

# Graph mode share for the two surveys
tpurps_of_modes_p5 <-
  bike_purp %>%
  filter(tpurp_c != "other") %>%
  mutate(survey = recode_factor(survey,
                                "mdt" = "My Daily Travel (2019)",
                                "tt" = "Travel Tracker (2008)"
                                )) %>%
  ggplot(aes(x = reorder(tpurp_c,desc(tpurp_c)), y = tpurp_c_pct, fill = survey)) +
  geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(tpurps_of_modes_p5,
              title = "Bicycle mode share by category of trips, 2008 vs. 2019.",
              caption = "Source: CMAP analysis of MDT and TT data. Note that
              categories are not precise comparisons.",
              filename = "tpurps_of_modes_p5",
              height = 6.3,
              width = 11.3,
              mode = "png")


# Generate output table
bike_purp %>%
  mutate(tpurp_c_pct = paste0(round(tpurp_c_pct * 100,1),"%")) %>%
  pivot_wider(names_from = "survey",
              values_from = c("tpurp_c_count","tpurp_c_pct"))




#### Purpose breakdown of rideshare vs. shared rideshare vs. taxi


## Create totals for trips by purpose category (within universe of rideshare trips)

### Filter data
all_tnc_mdt <-
  mdt_base_2 %>%                # 96,788 records
  filter(mode %in%              # 1,007 records
           c("rideshare",
             "shared rideshare",
             "taxi"))

all_tnc_tt <-
  tt_base_2 %>%                 # 98,800 records
  filter(MODE == "taxi")        # 336 records - potentially not enough for analysis

### Calculate proportions for TT
all_tnc_tpurp_c_tt <-
  all_tnc_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight)) %>%
  mutate(tpurp_c_pct = tpurp_c_total / sum(.$tpurp_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_tnc_tpurp_c_mdt <-
  all_tnc_mdt %>%
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
detailed_tnc_totals_mdt <-
  all_tnc_mdt %>%
  group_by(mode) %>%
  summarize(trip_total = sum(wtperfin))

detailed_tnc_tpurp_c_mdt <-
  all_tnc_mdt %>%
  group_by(tpurp_c,mode) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  left_join(.,detailed_tnc_totals_mdt,by = "mode") %>%
  mutate(tpurp_c_pct = tpurp_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "mdt")

all_tnc_tpurp_c <-
  rbind(total_tnc_tpurp_c,
        detailed_tnc_tpurp_c_mdt)


tpurps_of_modes_p6 <-
  all_tnc_tpurp_c %>%
  filter(tpurp_c != "missing",
         survey == "mdt") %>%
  mutate(mode = factor(mode, levels = c("tnc (all)","taxi","shared rideshare","rideshare"))) %>%
  ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v",legend.max.columns = 3) +
  scale_x_continuous(labels = scales::label_percent()) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(tpurps_of_modes_p6,
              "Trip purposes of TNC and taxi trips, 2019.",
              "Source: CMAP analysis of MDT and TT data.",
              width = 11.3,
              height = 6.3,
              filename = "tpurps_of_modes_p6",
              mode = "png",
              overwrite = T)


tpurps_of_modes_p7 <-
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

finalize_plot(tpurps_of_modes_p7,
              "Change in daily TNC and taxi trips, 2008 vs. 2019.",
              "Source: CMAP analysis of MDT and TT data.",
              filename = "tpurps_of_modes_p7",
              mode = "png",
              height = 6.3,
              width = 11.3,
              overwrite = T)



