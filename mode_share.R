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

mdt_base_3 <-
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

tt_base_3 <-
  tt %>%                      # 140,751 records
  filter(AGE < 90,            # 137,844 records
         AGE >= 5 |           # 131,082 records
           SCHOL %in% c(4,5,6,7,8),
         DIST > 0,            # 98,800 records
         mode_c != "missing") # 98,800 records


mdt_mode_all <-
  mdt_base_3 %>%
  mutate(total = sum(wthhfin)) %>%
  group_by(mode_c) %>%
  summarise(count = sum(wthhfin),
            mdt_share = round((count/median(total))*100, digits = 2)) %>%
  select(mode_c, mdt_share)

tt_mode_all <-
  tt_base_3 %>%
  mutate(total = sum(weight)) %>%
  group_by(mode_c) %>%
  summarize(count = sum(weight),
            tt_share = round((count/median(total))*100, digits = 2)) %>%
  select(mode_c, tt_share)

mode_all <- tt_mode_all %>%
  left_join(mdt_mode_all, by = "mode_c") %>%
  pivot_longer(cols = c("tt_share":"mdt_share"))


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
              travelers from ages 5 to 89 (inclusive).
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              height = 5.5,
              width = 11.3,
              filename = "mode_share_p1",
              mode = "png",
              overwrite = T)
