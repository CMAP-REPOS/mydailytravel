# This script produces analyses and charts on the share of car trips taken as
# passengers by travelers in the CMAP region.


#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
# # Run this script once per machine to add the ggpattern package
# devtools::install_github("coolbutuseless/ggpattern")
library(ggpattern)

source("pct_calculator.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

# Age bins
breaks <- c(-1, 9, 17, 24, 29, 49, 69, 89)
age_labels <- c("5 to 9", "10 to 17", "18 to 24", "25 to 29",
                "30 to 49", "50 to 69","70 to 89")

driver_pax_mdt <-
  mdt %>%                        # 125091 records
  # Keep only travelers from age 18 to 89
  filter(age < 90,               #
         age >= 18 |             #
           # Keep those who are 18 or older, OR
         aage %in% c(5,6,7)) %>%
           # age buckets that are > 18
  # Keep only trips with > 0 distance
  filter(distance_pg > 0) %>%    #
  # Keep only driver or passenger trips, but exclude motorcycles
  filter(mode_c %in% c("driver", #
                       "passenger"),
         mode != "motorcycle"    # 60077 records
         ) %>%
  # Add age bins
  mutate(age_bin = cut(age, breaks = breaks,
                     labels = age_labels)) %>%
  ungroup()


driver_pax_tt <-
  tt %>%                         # 137491 records
  # Keep only travelers from age 18 to 89
  filter(AGE < 90,               #
         AGE >= 18) %>%          #
  # Keep only trips with > 0 distance
  filter(DIST > 0) %>%           #
  # Keep only driver and passenger trips
  filter(mode_c %in% c("driver", # 71322 records
                       "passenger")) %>%
  # Add age bins
  mutate(age_bin = cut(AGE, breaks = breaks,
                       labels = age_labels)) %>%
  # Note: we do not include anyone without a specific age from TT because they
  # can't be put into the bins we want, and since the AGEB variable only has <16
  # or 16+, we don't know if they are 18+
  ungroup()


#################################################
#                                               #
#                   Analysis                    #
#                                               #
#################################################

################################################################################
#
# PASSENGER BEHAVIOR BY AGE
################################################################################

# Create age bucket totals and mode shares for driving and passenger trips
driver_pax_age_mdt <-
  pct_calculator(driver_pax_mdt,
                 breakdown_by = "mode_c",
                 second_breakdown = "age_bin",
                 weight = "wtperfin",
                 survey = "mdt")

driver_pax_age_tt <-
  pct_calculator(driver_pax_tt,
                 breakdown_by = "mode_c",
                 second_breakdown = "age_bin",
                 weight = "weight",
                 survey = "tt")

################################################################################
# Chart of passenger share by age
################################################################################

# Create age bin chart
driver_pax_p1 <-
  # Combine data
  rbind(driver_pax_age_mdt %>% select(age_bin,mode_c,pct,survey),
      driver_pax_age_tt  %>% select(age_bin,mode_c,pct,survey)) %>%
  # Factor age bin into desired order
  mutate(age_bin = factor(age_bin,
                          levels = c("70 to 89","50 to 69","30 to 49","25 to 29",
                                     "18 to 24","10 to 17","5 to 9"
                                     ))) %>%
  # Create "type" for increasing vs. decreasing
  mutate(type = case_when(
    age_bin %in% c("18 to 24","25 to 29") ~ "Increasing passenger share",
    TRUE ~ "Decreasing passenger share"
    )) %>%
  # Factor "type"
  mutate(type = factor(type, levels = c("Increasing passenger share",
                                        "Decreasing passenger share"))) %>%
  # Rename and factor survey
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel ('19)",
                                tt = "Travel Tracker ('08)")) %>%
  # Remove <18 travelers (since their share is so much higher due to non-drivers)
  filter(mode_c == "passenger" & age_bin != "5 to 9" & age_bin != "10 to 17") %>%
  # Create ggplot object
  ggplot(aes(y = age_bin, x = pct, pattern = type)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  geom_col_pattern(aes(fill = survey),
                   color = "white",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.05,
                   pattern_spacing = 0.0125,
                   pattern_key_scale_factor = 0.6,
                   position = position_dodge2(width = 0.8, reverse = T),
                   width = 0.8) +
  # Re-assign patterns manually
  scale_pattern_manual(values = c("Increasing passenger share" = "stripe",
                                  "Decreasing passenger share" = "none")) +
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = .1)(pct),
                 group = survey),
             position = position_dodge2(width = 0.9,reverse = T),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  # Call CMAP style and palette
  theme_cmap(gridlines = "v", vline = 0) +
  cmap_fill_discrete(palette = "mobility") +
  # Adjust x axis labels
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),limits = c(0,.25)) +
  # Adjust legend for formatting
  guides(pattern = guide_legend(override.aes = list(fill = "white", color = "black")),
         fill = guide_legend(override.aes = list(pattern = "none")))

# Export plot
finalize_plot(driver_pax_p1,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time and by age.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 89.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "driver_pax_p1",
              mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T
              )

################################################################################
# Summary statistics - overall percentage of passenger, TT vs MDT
################################################################################

# Quick output table of totals
rbind(driver_pax_age_mdt,
      driver_pax_age_tt) %>%
  group_by(mode_c,survey) %>%
  summarize(mode_count = sum(breakdown_total)) %>%
  pivot_wider(id_cols = c("survey"),names_from = "mode_c",values_from = c("mode_count")) %>%
  mutate(pax_share = passenger/(passenger + driver))

################################################################################
#
# PASSENGER BEHAVIOR BY INCOME
################################################################################

driver_pax_inc_mdt <-
  pct_calculator(driver_pax_mdt,
                 breakdown_by = "mode_c",
                 second_breakdown = "income_c",
                 weight = "wtperfin",
                 survey = "mdt")

driver_pax_inc_tt <-
  pct_calculator(driver_pax_tt,
                 breakdown_by = "mode_c",
                 second_breakdown = "income_c",
                 weight = "weight",
                 survey = "tt")


################################################################################
# Chart of passenger share by income
################################################################################

driver_pax_p2 <- rbind(driver_pax_inc_mdt %>%
                                   select(income_c,mode_c,pct,survey),
                                 driver_pax_inc_tt  %>%
                                   select(income_c,mode_c,pct,survey)) %>%
  filter(mode_c == "passenger", income_c != "missing") %>%
  mutate(survey = recode_factor(survey,
                                mdt = "My Daily Travel ('19)",
                                tt = "Travel Tracker ('08)"),
         income_c = recode_factor(income_c,
                                  "high" = "High",
                                  "middle-high" = "Middle-high",
                                  "middle-low" = "Middle-low",
                                  "low" = "Low")) %>%
  ggplot(aes(y = income_c, x = pct, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v", vline = 0) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(pct),
                 group = survey),
             position = position_dodge2(0.9,reverse = T),
             hjust = 0,
             fill = "white",
             label.size = 0) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),limits = c(0,.25))

finalize_plot(driver_pax_p2,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time and by income.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 89.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "driver_pax_p2",
              mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T

)

################################################################################
#
# PASSENGER BEHAVIOR BY RACE
################################################################################

### Same analysis, looking at race and ethnicity instead - only looking at MDT
### since TT only asked about race and ethnicity for primary household
### responder.

driver_pax_race_mdt <-
  pct_calculator(driver_pax_mdt,
                 breakdown_by = "mode_c",
                 second_breakdown = "race_eth",
                 weight = "wtperfin",
                 survey = "mdt")


################################################################################
# Chart of passenger share by race
################################################################################

driver_pax_p3 <-
  driver_pax_race_mdt %>%
  select(race_eth,mode_c,pct,survey) %>%
  filter(mode_c == "passenger", race_eth != "missing") %>%
  ggplot(aes(y = reorder(race_eth,desc(pct)), x = pct)) +
  geom_col(aes(fill = race_eth)) +
  theme_cmap(gridlines = "v", legend.position = "none", vline = 0) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(pct)),
             label.size = 0,
             hjust = 0) +
  cmap_fill_race() +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0,.21))

finalize_plot(driver_pax_p3,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver,  by race and ethnicity.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 89. \"Hispanic\" includes
              all travelers who identified as Hispanic. Other groups (e.g.,
              \"White\") are non-Hispanic.
              <br><br>
              Source: CMAP analysis of My Daily Travel surveys.",
              filename = "driver_pax_p3",
              mode = "png",
              height = 6.3,
              width = 11.3,
              overwrite = T
)

################################################################################
#
# PASSENGER BEHAVIOR BY RACE AND INCOME
################################################################################

driver_pax_inc_race_mdt <-
  driver_pax_mdt %>%
  # Calculate totals for race and income
  group_by(income_c,race_eth) %>%
  mutate(total = sum(wtperfin)) %>%
  ungroup() %>%
  # Calculate percentages by race, income, and mode
  group_by(income_c,race_eth,mode_c) %>%
  summarise(mode_count = sum(wtperfin),
            total = median(total)) %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "mdt")

################################################################################
# Chart of passenger share by age and income
################################################################################

# Chart of drivers and passengers by income and race/ethnicity
driver_pax_p4 <-
  driver_pax_inc_race_mdt %>%
  filter(mode_c == "passenger", income_c != "missing", race_eth != "missing") %>%
  mutate(income_c = recode_factor(income_c,
                                  "low" = "Low",
                                  "middle-low" = "Middle-low",
                                  "middle-high" = "Middle-high",
                                  "high" = "High"),
         race_eth = factor(race_eth, levels = c("white","asian","black","hispanic","other"))) %>%
  ggplot(aes(y = income_c, x = mode_share, fill = race_eth)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v",
             axis.text.y = element_blank()) +
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_share),
                 group = race_eth),
             position = position_dodge2(0.9,reverse = T),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  cmap_fill_race() +
  facet_wrap(~income_c,scales = "free_y") +
  scale_x_continuous(labels = scales::label_percent(),limits = c(0,.65))


finalize_plot(driver_pax_p4,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time, by race and income.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 89.\"Hispanic\" includes
              all travelers who identified as Hispanic. Other groups (e.g.,
              \"White\") are non-Hispanic.
              <br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "driver_pax_p4",
              mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T
)
################################################################################
# ARCHIVE - Explaration of "Other" category
################################################################################
#
# driver_pax_other_mdt <-
#   driver_pax_mdt %>%
#   filter(race_eth == "other") %>%
#   ungroup() %>%
#   mutate(total = sum(wtperfin)) %>%
#   group_by(race, mode_c) %>%
#   summarise(mode_count = sum(wtperfin),
#             total = median(total)) %>%
#   mutate(mode_share = (mode_count / total)) %>%
#   mutate(survey = "2019 - My Daily Travel")
#
#
# driver_pax_p4 <-
#   driver_pax_other_mdt %>%
#   select(race,mode_c,mode_share,survey) %>%
#   filter(mode_c == "passenger") %>%
#   mutate(foo = "foo") %>%
#   ggplot(aes(y = reorder(race,desc(mode_share)), x = mode_share, fill = foo)) +
#   geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
#   theme_cmap(gridlines = "v", legend.position = "none") +
#   cmap_fill_discrete(palette = "legislation") +
#   scale_x_continuous(labels = scales::label_percent())

################################################################################
# Cleanup
################################################################################
# Remove objects from the environment
rm(driver_pax_race_mdt,driver_pax_total_inc_mdt,driver_pax_total_inc_tt,
   driver_pax_total_mdt,driver_pax_total_race_mdt,driver_pax_total_tt,
   driver_pax_tt,driver_pax_age_mdt,driver_pax_age_tt,driver_pax_inc_mdt,
   driver_pax_inc_tt,driver_pax_mdt,age_labels,breaks,driver_pax_p1,
   driver_pax_p2,driver_pax_p3,driver_pax_other_mdt,driver_pax_total_other_mdt,
   driver_pax_p4,driver_pax_total_inc_race_mdt,driver_pax_inc_race_mdt,
   driver_pax_p5)

