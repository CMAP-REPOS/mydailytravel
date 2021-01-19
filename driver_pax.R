library(cmapplot)
library(tidyverse)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

#################################################
#                                               #
#            Analysis of driver vs. passenger   #
#                                               #
#################################################
# Age bins
breaks <- c(-1, 10, 18, 30, 40, 50, 60, 70, 80, 90)
age_labels <- c("5 to 9", "10 to 17", "18 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

driver_pax_mdt <-
  mdt %>%                        # 125,103 records
  filter(age < 90,               # 125,006 records
         age >= 5,               # 124,758 records
         distance_pg > 0,        # 96,674 records
         mode_c %in% c("driver", # 69,165 records
                       "passenger")) %>%
  mutate(age_bin = cut(age, breaks = breaks,
                     labels = age_labels))

driver_pax_tt <-
  tt %>%                         # 140,751 records
  filter(AGE < 90,               # 137,844 records
         AGE >= 5,               # 131,082 records
         DIST > 0,               # 98,800 records
         mode_c %in% c("driver", # 80,473 records
                       "passenger")) %>%
  mutate(age_bin = cut(AGE, breaks = breaks,
                       labels = age_labels))

driver_pax_total_mdt <-
  driver_pax_mdt %>%
  group_by(age_bin) %>%
  summarise(total = sum(wthhfin))

driver_pax_age_mdt <-
  driver_pax_mdt %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(driver_pax_total_mdt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")

driver_pax_total_tt <-
  driver_pax_tt %>%
  group_by(age_bin) %>%
  summarise(total = sum(weight))

driver_pax_age_tt <-
  driver_pax_tt %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(driver_pax_total_tt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2008 - Travel Tracker")

driver_pax_p1 <-
  rbind(driver_pax_age_mdt %>% select(age_bin,mode_c,mode_share,survey),
      driver_pax_age_tt  %>% select(age_bin,mode_c,mode_share,survey)) %>%
  mutate(label = paste0(format(round(mode_share*100,1),nsmall = 1),"%")) %>%
  filter(mode_c == "passenger" & age_bin != "5 to 9" & age_bin != "10 to 17") %>%
  ggplot(aes(y = age_bin, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent(),limits = c(0,.30))


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

rbind(driver_pax_age_mdt, driver_pax_age_tt) %>%
  group_by(mode_c,survey) %>%
  summarize(mode_count = sum(mode_count)) %>%
  pivot_wider(id_cols = c("survey"),names_from = "mode_c",values_from = c("mode_count")) %>%
  mutate(pax_share = passenger/driver)


### Same analysis, looking at income instead

driver_pax_total_inc_mdt <-
  driver_pax_mdt %>%
  group_by(income_c) %>%
  summarise(total = sum(wthhfin))

driver_pax_inc_mdt <-
  driver_pax_mdt %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(driver_pax_total_inc_mdt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")

driver_pax_total_inc_tt <- driver_pax_tt %>%
  group_by(income_c) %>%
  summarise(total = sum(weight))

driver_pax_inc_tt <- driver_pax_tt %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(driver_pax_total_inc_tt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2008 - Travel Tracker")


# Chart of drivers and passengers by income
driver_pax_p2 <- rbind(driver_pax_inc_mdt %>%
                                   select(income_c,mode_c,mode_share,survey),
                                 driver_pax_inc_tt  %>%
                                   select(income_c,mode_c,mode_share,survey)) %>%
  filter(mode_c == "passenger", income_c != "missing") %>%
  mutate(label = paste0(format(round(mode_share*100,1),nsmall = 1),"%")) %>%
  ggplot(aes(y = income_c, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v") +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent(),limits = c(0,.35))


finalize_plot(driver_pax_p2,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time and by income.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 89.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "driver_pax_p2",
              mode = "png",
              width = 11.3,
              height = 6.3,
              overwrite = T

)


### Same analysis, looking at race and ethnicity instead - only looking at MDT
### since TT only asked about race and ethnicity for primary household
### responder.

driver_pax_total_race_mdt <-
  driver_pax_mdt %>%
  group_by(race_eth) %>%
  summarise(total = sum(wthhfin))

driver_pax_race_mdt <-
  driver_pax_mdt %>%
  group_by(race_eth, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(driver_pax_total_race_mdt, by = "race_eth") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")


driver_pax_p3 <-
  driver_pax_race_mdt %>%
  select(race_eth,mode_c,mode_share,survey) %>%
  filter(mode_c == "passenger", race_eth != "missing") %>%
  ggplot(aes(y = reorder(race_eth,desc(mode_share)), x = mode_share, fill = race_eth)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v", legend.position = "none") +
  cmap_fill_race() +
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(driver_pax_p3,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver,  by race and ethnicity.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 89. \"Hispanic\" includes
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

rm(driver_pax_race_mdt,driver_pax_total_inc_mdt,driver_pax_total_inc_tt,
   driver_pax_total_mdt,driver_pax_total_race_mdt,driver_pax_total_tt,
   driver_pax_tt,driver_pax_age_mdt,driver_pax_age_tt,driver_pax_inc_mdt,
   driver_pax_inc_tt,driver_pax_mdt,age_labels,breaks,driver_pax_p1,
   driver_pax_p2,driver_pax_p3)

