
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
breaks <- c(-1, 10, 25, 30, 40, 50, 60, 70, 80, 90)
age_labels <- c("5 to 9", "10 to 17", "18 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

mdt_d_and_p <- mdt %>%
  mutate(age_bin = cut(age, breaks = breaks,
                       labels = age_labels)) %>%
  filter(age < 90,age>=5,
         mode_c %in% c("driver","passenger"),
         distance_pg > 0)

tt_d_and_p <- tt %>%
  mutate(age_bin = cut(AGE, breaks = breaks,
                       labels = age_labels)) %>%
  filter(AGE < 90,AGE>=5,PLANO!=1,
         mode_c %in% c("driver","passenger"),
         DIST > 0)

d_and_p_total_mdt <- mdt_d_and_p %>%
  group_by(age_bin) %>%
  summarise(total = sum(wthhfin))

d_vs_p_age_mdt <- mdt_d_and_p %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_mdt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")

d_and_p_total_tt <- tt_d_and_p %>%
  group_by(age_bin) %>%
  summarise(total = sum(weight))

d_vs_p_age_tt <- tt_d_and_p %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(d_and_p_total_tt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2008 - Travel Tracker")

chart1 <- rbind(d_vs_p_age_mdt %>% select(age_bin,mode_c,mode_share,survey),
      d_vs_p_age_tt  %>% select(age_bin,mode_c,mode_share,survey)) %>%
  mutate(label = paste0(format(round(mode_share*100,1),nsmall = 1),"%")) %>%
  filter(mode_c == "passenger" & age_bin != "5 to 9" & age_bin != "10 to 17") %>%
  ggplot(aes(y = age_bin, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent(),limits = c(0,.30))


finalize_plot(chart1,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 90.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "dp_foo_1",
              mode = "png"
              )

rbind(d_vs_p_age_mdt, d_vs_p_age_tt) %>%
  group_by(mode_c,survey) %>%
  summarize(mode_count = sum(mode_count)) %>%
  pivot_wider(id_cols = c("survey"),names_from = "mode_c",values_from = c("mode_count")) %>%
  mutate(pax_share = passenger/driver)


### Same analysis, looking at income instead

d_and_p_total_inc_mdt <- mdt_d_and_p %>%
  group_by(income_c) %>%
  summarise(total = sum(wthhfin))

d_vs_p_inc_mdt <- mdt_d_and_p %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_inc_mdt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")

d_and_p_total_inc_tt <- tt_d_and_p %>%
  group_by(income_c) %>%
  summarise(total = sum(weight))

d_vs_p_inc_tt <- tt_d_and_p %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(d_and_p_total_inc_tt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2008 - Travel Tracker")

chart_passengers_income <- rbind(d_vs_p_inc_mdt %>%
                                   select(income_c,mode_c,mode_share,survey),
                                 d_vs_p_inc_tt  %>%
                                   select(income_c,mode_c,mode_share,survey)) %>%
  filter(mode_c == "passenger", income_c != "missing") %>%
  mutate(label = paste0(format(round(mode_share*100,1),nsmall = 1),"%")) %>%
  ggplot(aes(y = income_c, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v") +
  geom_text(aes(label = label),position = position_dodge2(0.9,reverse = T), hjust = 0) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent(),limits = c(0,.35))


finalize_plot(chart_passengers_income,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 90.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "dp_foo_2",
              mode = "png"
)


### Same analysis, looking at race and ethnicity instead - only looking at MDT
### since TT only asked about race and ethnicity for primary household
### responder.

d_and_p_total_race_mdt <- mdt_d_and_p %>%
  group_by(race_eth) %>%
  summarise(total = sum(wthhfin))

d_vs_p_race_mdt <- mdt_d_and_p %>%
  group_by(race_eth, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_race_mdt, by = "race_eth") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2019 - My Daily Travel")


chart_passengers_race <- d_vs_p_race_mdt %>%
  select(race_eth,mode_c,mode_share,survey) %>%
  filter(mode_c == "passenger", race_eth != "missing") %>%
  ggplot(aes(y = reorder(race_eth,desc(mode_share)), x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v", legend.position = "none") +
  cmap_fill_discrete(palette = "legislation") +
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(chart_passengers_race,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 90.<br><br>
              Source: CMAP analysis of My Daily Travel surveys.",
              filename = "dp_foo_3",
              mode = "png"
)
