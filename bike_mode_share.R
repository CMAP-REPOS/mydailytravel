library(tidyverse)
library(cmapplot)
library(forcats)




#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

#################################################
#                                               #
#            Trip purpose of bike trips         #
#                                               #
#################################################


## Create totals for trips by purpose category

### Filter data
all_mdt <- mdt %>%
  filter(age < 90, age >= 5,
         mode_c != "missing",
         distance_pg > 0)

all_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5,
         mode_c != "missing",
         DIST > 0)

### Calculate total number of trips
all_purp_mdt <- all_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  filter(tpurp_c != "missing")

all_purp_tt <- all_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight))


# Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
bike_purp_mdt <- all_mdt %>%
  filter(mode_c == "bike") %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_count = sum(wtperfin)) %>%
  inner_join(., all_purp_mdt, by = "tpurp_c") %>%
  mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
         survey = "mdt") %>% # add identifier
  arrange(tpurp_c_pct) # sort by mode share

bike_purp_tt <- all_tt %>%
  filter(mode_c == "bike") %>%
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
bike_mode_share <-
  bike_purp %>%
  filter(tpurp_c != "other") %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker (2008)",
                                "mdt" = "My Daily Travel (2019)")) %>%
  ggplot(aes(x = reorder(tpurp_c,desc(tpurp_c)), y = tpurp_c_pct, fill = survey)) +
  geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(bike_mode_share,
              title = "Bicycle mode share by category of trips, 2008 vs. 2019.",
              caption = "Source: CMAP analysis of MDT and TT data. Note that categories are not precise comparisons.")


# Generate output table
bike_purp %>%
  mutate(tpurp_c_pct = paste0(round(tpurp_c_pct * 100,1),"%")) %>%
  pivot_wider(names_from = "survey",
              values_from = c("tpurp_c_count","tpurp_c_pct"))

