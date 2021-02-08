library(ggplot2)
library(tidyverse)
library(cmapplot)


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
           schol %in% c(5,6,7,8)) %>%
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


tt_base_3 <-
  tt %>%                               # 140751 records
  # Keep only travelers >= 16 years old, either through age, age bucket, or
  # school enrollment
  filter(AGE >= 16 |                   # 118886
           SCHOL %in% c(5,6,7,8) |
           AGEB == 2) %>%
  # Keep only trips with nonzero distance
  filter(DIST > 0) %>%                 # 89784
  # Exclude missing modes
  filter(mode_c != "missing") %>%      # 89784
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

# Create baseline totals for percentage calculations
mdt_mode_all <-
  mdt_base_3 %>%
  mutate(total = sum(wthhfin)) %>%
  group_by(mode_c) %>%
  summarise(count = sum(wthhfin),
            mdt_share = round((count/median(total))*100, digits = 2)) %>%
  select(mode_c, mdt_share, count)

tt_mode_all <-
  tt_base_3 %>%
  mutate(total = sum(weight)) %>%
  group_by(mode_c) %>%
  summarize(count = sum(weight),
            tt_share = round((count/median(total))*100, digits = 2)) %>%
  select(mode_c, tt_share, count)

mode_all <- tt_mode_all %>%
  select(-count) %>%
  left_join(mdt_mode_all, by = "mode_c") %>%
  select(-count) %>%
  pivot_longer(cols = c("tt_share":"mdt_share"))

##########################################
# Comparison charts (NOTE : TRANSIT IS INVALID COMPARISON)
##########################################

mode_share_p1 <-
  mode_all %>%
  mutate(name = recode_factor(name,
                              tt_share = "Travel Tracker ('08)",
                              mdt_share = "My Daily Travel ('19)"),
         mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
  mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
  ggplot(aes(x = value, y = name, fill = mode_c)) +
  geom_col(position = position_stack(reverse = T)) +
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))

finalize_plot(mode_share_p1,
              title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 to 89 (inclusive).
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p1",
              mode = "png",
              overwrite = T)



mdt_mode_dist_all <-
  mdt_base_3 %>%
  mutate(total = sum(wthhfin*hdist_pg)) %>%
  group_by(mode_c) %>%
  summarise(distance = sum(wthhfin*hdist_pg),
            mdt_share = round((distance/median(total))*100, digits = 2)) %>%
  select(mode_c, mdt_share, distance)

tt_mode_dist_all <-
  tt_base_3 %>%
  mutate(total = sum(weight*DIST)) %>%
  group_by(mode_c) %>%
  summarize(distance = sum(weight*DIST),
            tt_share = round((distance/median(total))*100, digits = 2)) %>%
  select(mode_c, tt_share, distance)

mode_dist_all <- tt_mode_dist_all %>%
  select(-distance) %>%
  left_join(mdt_mode_dist_all, by = "mode_c") %>%
  select(-distance) %>%
  pivot_longer(cols = c("tt_share":"mdt_share"))


mode_share_p2 <-
  mode_dist_all %>%
  mutate(name = recode_factor(name,
                              tt_share = "Travel Tracker ('08)",
                              mdt_share = "My Daily Travel ('19)"),
         mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
  mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
  ggplot(aes(x = value, y = name, fill = mode_c)) +
  geom_col(position = position_stack(reverse = T)) +
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))

finalize_plot(mode_share_p2,
              title = "Share of travel distance for trips in the CMAP region, 2008 vs. 2019.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 to 89 (inclusive). Distances are \"as the crow files.\"
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p2",
              mode = "png",
              overwrite = T)

######################################
# MDT-only charts
######################################

mode_share_p3 <-
  mdt_mode_all %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         survey = "mdt") %>%

  ggplot(aes(y = mdt_share, x = survey, fill = mode_c)) +
  geom_col(position = position_stack(reverse = T)) +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_y_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))

finalize_plot(mode_share_p3,
              title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
              caption = "Note: Includes all trips in the CMAP region made by
              travelers from ages 16 to 89 (inclusive).
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p3",
              # mode = "png",
              overwrite = T)



mdt_mode_counties <-
  mdt_base_3 %>%
  filter(home_county %in% cmap_counties) %>%
  rbind(mdt_base_3 %>%
          filter(home_county %in% cmap_counties) %>%
          mutate(home_county = "CMAP region")
        ) %>%
  group_by(home_county) %>%
  mutate(total = sum(wtperfin)) %>%
  group_by(mode_c,home_county) %>%
  summarise(count = sum(wtperfin),
            mdt_share = round((count/median(total))*100, digits = 2)) %>%
  select(home_county,mode_c, mdt_share, count)

mode_share_p4 <-
  mdt_mode_counties %>%
  mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
                                           "transit","bike","other")),
         mdt_share = ifelse(mode_c %in% c("driver","passenger"),-1 *mdt_share,mdt_share),
         home_county = recode(home_county,
                                  "CMAP region" = "CMAP region",
                                  "31" = "Cook",
                                  "43" = "DuPage",
                                  "89" = "Kane",
                                  "93" = "Kendall",
                                  "97" = "Lake",
                                  "111" = "McHenry",
                                  "197" = "Will")
         ) %>%
  mutate(home_county = factor(home_county,
                              levels = c("Will","McHenry","Lake","Kendall",
                                         "Kane","DuPage","Cook","CMAP region"))
         ) %>%

  ggplot(aes(x = mdt_share, y = home_county, fill = mode_c)) +
  geom_col(position = position_stack(reverse = T)) +

  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))

finalize_plot(mode_share_p4,
              title = "Mode share for trips in the CMAP region by home county.",
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

