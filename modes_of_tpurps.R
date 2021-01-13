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

mdt_base_1 <-
  mdt %>%                        # 125,103 records
  filter(age < 90,               # 125,006 records
         age >= 5 |              # 125,002 records
           aage %in% c(2,3,4,5,6,7) |
           schol %in% c(4,5,6,7,8) |
           sampno %in% c(70038312,
                         70051607),
         distance_pg > 0,        # 96,857 records
         mode_c != "missing",    # 96,821 records
         mode_c != "beginning")  # 96,821 records

tt_base_1 <-
  tt %>%                      # 140,751 records
  filter(AGE < 90,            # 137,844 records
         AGE >= 5 |           # 131,082 records
           SCHOL %in% c(4,5,6,7,8),
         DIST > 0,            # 98,800 records
         mode_c != "missing") # 98,800 records

#### Mode breakdown of different dining trips


## Create totals for trips by mode category (within universe of dining trips)

### Filter data
all_dining_mdt <-
  mdt_base_1 %>%              # 96,821 records
  filter(tpurp_c == "dining") # 6,523 records

all_dining_tt <-
  tt_base_1 %>%               # 98,800 records
  filter(tpurp_c == "dining") # 5,769 records

### Calculate proportions for TT
all_dining_mode_c_tt <-
  all_dining_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_dining_mode_c_mdt <-
  all_dining_mdt %>%
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
detailed_dining_totals_mdt <-
  all_dining_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_dining_mode_c_mdt <-
  all_dining_mdt %>%
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
all_health_mdt <-
  mdt_base_1 %>%              # 96,821 records
  filter(tpurp_c == "health") # 2,033 records

all_health_tt <-
  tt_base_1 %>%               # 98,800 records
  filter(tpurp_c == "health") # 2,145 records

### Calculate proportions for TT
all_health_mode_c_tt <-
  all_health_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_health_mode_c_mdt <-
  all_health_mdt %>%
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
detailed_health_totals_mdt <-
  all_health_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_health_mode_c_mdt <-
  all_health_mdt %>%
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
all_community_mdt <-
  mdt_base_1 %>%                 # 96,821 records
  filter(tpurp_c == "community") # 3,924 records

all_community_tt <-
  tt_base_1 %>%                  # 98,800 records
  filter(tpurp_c == "community") # 4,687 records

### Calculate proportions for TT
all_community_mode_c_tt <-
  all_community_tt %>%
  group_by(mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  mutate(mode_c_pct = mode_c_total / sum(.$mode_c_total),
         survey = "tt")

### Calculate proportions for MDT
all_community_mode_c_mdt <-
  all_community_mdt %>%
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
detailed_community_totals_tt <-
  all_community_tt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(weight))

detailed_community_mode_c_tt <-
  all_community_tt %>%
  group_by(tpurp,mode_c) %>%
  summarize(mode_c_total = sum(weight)) %>%
  left_join(.,detailed_community_totals_tt,by = "tpurp") %>%
  mutate(mode_c_pct = mode_c_total / trip_total) %>%
  select(-trip_total) %>%
  mutate(survey = "tt")

### Calculate proportions for subcategories for community in MDT
detailed_community_totals_mdt <-
  all_community_mdt %>%
  group_by(tpurp) %>%
  summarize(trip_total = sum(wtperfin))

detailed_community_mode_c_mdt <-
  all_community_mdt %>%
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



##### Examination of TNC school trips - not enough records for rigorous analysis

### Filter data
all_tnc_school_mdt <-
  mdt_base_1 %>%                         # 96,821 records
  filter(age < 18,                       # 14,445 records
         mode %in% c("rideshare",
                     "shared rideshare",
                     "taxi"),            # 28 records
         tpurp_c == "school")            # 11 records

all_tnc_school_mdt %>%
  group_by(income_c, mode) %>%
  summarize(trips = sum(wtperfin))
