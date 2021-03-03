# This file produces overall charts on mode share in the CMAP region

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)

source("helper_fns.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

# Create base dataset for mode analyses

mdt_base_3 <-
  mdt %>%                              # 125103 records
  # Keep only travelers >= 16 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 16 |                   # 108293
           aage %in% c(4,5,6,7) |
           age < 0 & schol %in% c(5,6,7,8)) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%    # 84960
  # Exclude trips with zero distance
  filter(distance_pg > 0) %>%          # 84637
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%      # 84600
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

#
# tt_base_3 <-
#   tt %>%                               # 140751 records
#   # Keep only travelers >= 16 years old, either through age, age bucket, or
#   # school enrollment
#   filter(AGE >= 16 |                   # 118886
#            SCHOL %in% c(5,6,7,8) |
#            AGEB == 2) %>%
#   # Keep only trips with nonzero distance
#   filter(DIST > 0) %>%                 # 89784
#   # Exclude missing modes
#   filter(mode_c != "missing") %>%      # 89784
#   # Put school bus back into "other" category
#   mutate(mode_c = as.character(mode_c)) %>%
#   mutate(mode_c = case_when(
#     mode_c == "schoolbus" ~ "other",
#     TRUE ~ mode_c)) %>%
#   mutate(mode_c = factor(mode_c,levels = mode_c_levels))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# MDT-only analyses
################################################################################

# Create baseline totals for percentage calculations
mdt_mode_all <-
  pct_calculator(mdt_base_3,
                 breakdown_by = "mode_c",
                 weight = "wtperfin")

mdt_mode_counties <-
  pct_calculator(mdt_base_3 %>%
                   filter(home_county %in% cmap_counties) %>%
                   rbind(mdt_base_3 %>%
                           filter(home_county %in% cmap_counties) %>%
                           mutate(home_county = "CMAP region")),
                 breakdown_by = "mode_c",
                 second_breakdown = "home_county",
                 weight = "wtperfin") %>%
  mutate(home_county = recode(home_county,
                              "31" = "Cook",
                              "CMAP region" = "CMAP region",
                              "97" = "Lake",
                              "43" = "DuPage",
                              "89" = "Kane",
                              "93" = "Kendall",
                              "111" = "McHenry",
                              "197" = "Will"))

mdt_mode_income <-
  pct_calculator(mdt_base_3 %>% filter(hhinc > 0) %>%
                   rbind(mdt_base_3 %>% filter(hhinc > 0) %>%
                           mutate(hhinc = 99)),
                 breakdown_by = "mode_c",
                 second_breakdown = "hhinc",
                 weight = "wtperfin") %>%
  mutate(hhinc = recode_factor(factor(hhinc),
                             "10" = "$150,000 or more",
                             "9" = "$100,000 to $149,999",
                             "8" = "$75,000 to $99,999",
                             "7" = "$60,000 to $74,999",
                             "6" = "$50,000 to $59,999",
                             "5" = "$35,000 to $49,999",
                             "4" = "$30,000 to $34,999",
                             "3" = "$25,000 to $29,999",
                             "2" = "$15,000 to $24,999",
                             "1" = "Less than $15,000",
                             "99" = "CMAP region average"))

mdt_mode_race <-
  pct_calculator(mdt_base_3 %>% filter(race_eth != "missing") %>%
                   rbind(mdt_base_3 %>% filter(race_eth != "missing") %>%
                           mutate(race_eth = "CMAP region average")),
                 breakdown_by = "mode_c",
                 second_breakdown = "race_eth",
                 weight = "wtperfin") %>%
  mutate(race_eth = recode_factor(factor(race_eth),
                                  "other" = "Other",
                                  "black" = "Black or African-American",
                                  "asian" = "Asian",
                                  "hispanic" = "Hispanic or Latino",
                                  "white" = "White (non-Hispanic)"))


# Age bins
breaks <- c(-1,29, 49, 69, 150)
age_labels <- c("16 to 29", "30 to 49",  "50 to 69", "70 and above")

mdt_mode_age <-
  pct_calculator(mdt_base_3 %>% filter(age > 0) %>%
                   mutate(age_bin = cut(age, breaks = breaks,
                                        labels = age_labels)) %>%
                   rbind(mdt_base_3 %>% filter(age > 0) %>% mutate(age_bin = "CMAP region average")),
                 breakdown_by = "mode_c",
                 second_breakdown = "age_bin",
                 weight = "wtperfin") %>%
  mutate(age_bin = fct_relevel(age_bin,"CMAP region average")) %>%
  mutate(age_bin = fct_rev(factor(age_bin)))

################################################################################
# Chart of mode share by home county
################################################################################

mode_share_p1_labels <-
  mdt_mode_counties %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(home_county) %>%
  summarize(label = sum(pct))

mode_share_p1 <-
  mdt_mode_counties %>%
  left_join(mode_share_p1_labels, by = "home_county") %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         pct = ifelse(mode_c %in% c("driver","passenger"),-1 *pct,pct)
  ) %>%

  ggplot(aes(x = pct, y = reorder(home_county,label))) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p1_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = home_county),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.4))

finalize_plot(mode_share_p1,
              title = "Mode share for trips in the CMAP region by home county,
              highlighting non-car mode share.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 and older.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p1",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by income
################################################################################

mode_share_p2_labels <-
  mdt_mode_income %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(hhinc) %>%
  summarize(label = sum(pct))

mode_share_p2 <-
  mdt_mode_income %>%
  left_join(mode_share_p2_labels, by = "hhinc") %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         pct = ifelse(mode_c %in% c("driver","passenger"),-1 *pct,pct)
  ) %>%

  ggplot(aes(x = pct, y = hhinc)) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p2_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = hhinc),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.55))

finalize_plot(mode_share_p2,
              title = "Mode share for trips in the CMAP region by household income,
              highlighting non-car mode share.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 and older.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p2",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by race
################################################################################

mode_share_p3_labels <-
  mdt_mode_race %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(race_eth) %>%
  summarize(label = sum(pct))

mode_share_p3 <-
  mdt_mode_race %>%
  left_join(mode_share_p3_labels, by = "race_eth") %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         pct = ifelse(mode_c %in% c("driver","passenger"),-1 *pct,pct)
  ) %>%

  ggplot(aes(x = pct, y = reorder(str_wrap(race_eth,18),label))) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p3_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = str_wrap(race_eth,18)),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.55))

finalize_plot(mode_share_p3,
              title = "Mode share for trips in the CMAP region by race and ethnicity,
              highlighting non-car mode share.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 and older.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p3",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by age
################################################################################

mode_share_p4_labels <-
  mdt_mode_age %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(age_bin) %>%
  summarize(label = sum(pct))

mode_share_p4 <-
  mdt_mode_age %>%
  left_join(mode_share_p4_labels, by = "age_bin") %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         pct = ifelse(mode_c %in% c("driver","passenger"),-1 *pct,pct)
  ) %>%

  ggplot(aes(x = pct, y = age_bin)) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p4_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = age_bin),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.55))

finalize_plot(mode_share_p4,
              title = "Mode share for trips in the CMAP region by age,
              highlighting non-car mode share.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 and older.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p4",
              mode = "png",
              overwrite = T)
################################################################################
# Archive - Chart of overall mode share
################################################################################
#
# mode_share_p2 <-
#   mdt_mode_all %>%
#   mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
#                                            "transit","bike","other")),
#          survey = "mdt") %>%
#
#   ggplot(aes(y = mdt_share, x = survey, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_y_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p2,
#               title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive).
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p2",
#               # mode = "png",
#               overwrite = T)

################################################################################
#
# ARCHIVE - Comparisons of MDT and TT (note data problems)
################################################################################
#
# tt_mode_all <-
#   tt_base_3 %>%
#   mutate(total = sum(weight)) %>%
#   group_by(mode_c) %>%
#   summarize(count = sum(weight),
#             tt_share = round((count/median(total))*100, digits = 2)) %>%
#   select(mode_c, tt_share, count)
#
# mode_all <- tt_mode_all %>%
#   select(-count) %>%
#   left_join(mdt_mode_all, by = "mode_c") %>%
#   select(-count) %>%
#   pivot_longer(cols = c("tt_share":"mdt_share"))
#
# mdt_mode_dist_all <-
#   mdt_base_3 %>%
#   mutate(total = sum(wthhfin*hdist_pg)) %>%
#   group_by(mode_c) %>%
#   summarise(distance = sum(wthhfin*hdist_pg),
#             mdt_share = round((distance/median(total))*100, digits = 2)) %>%
#   select(mode_c, mdt_share, distance)
#
# tt_mode_dist_all <-
#   tt_base_3 %>%
#   mutate(total = sum(weight*DIST)) %>%
#   group_by(mode_c) %>%
#   summarize(distance = sum(weight*DIST),
#             tt_share = round((distance/median(total))*100, digits = 2)) %>%
#   select(mode_c, tt_share, distance)
#
# mode_dist_all <- tt_mode_dist_all %>%
#   select(-distance) %>%
#   left_join(mdt_mode_dist_all, by = "mode_c") %>%
#   select(-distance) %>%
#   pivot_longer(cols = c("tt_share":"mdt_share"))


################################################################################
# ARCHIVE - Chart of mode share, MDT vs. TT
################################################################################
#
# mode_share_p3 <-
#   mode_all %>%
#   mutate(name = recode_factor(name,
#                               tt_share = "Travel Tracker ('08)",
#                               mdt_share = "My Daily Travel ('19)"),
#          mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
#   mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
#   ggplot(aes(x = value, y = name, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p3,
#               title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive).
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p3",
#               mode = "png",
#               overwrite = T)

################################################################################
# ARCHIVE - Chart of travel distance, MDT vs. TT
################################################################################
#
# mode_share_p4 <-
#   mode_dist_all %>%
#   mutate(name = recode_factor(name,
#                               tt_share = "Travel Tracker ('08)",
#                               mdt_share = "My Daily Travel ('19)"),
#          mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
#   mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
#   ggplot(aes(x = value, y = name, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p4,
#               title = "Share of travel distance for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive). Distances are \"as the crow files.\"
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p4",
#               mode = "png",
#               overwrite = T)

