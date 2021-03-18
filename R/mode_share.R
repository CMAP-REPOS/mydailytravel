# This file produces overall charts on mode share in the CMAP region

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


# Create base dataset for mode analyses

mdt_base_3 <-
  mdt %>%                              # 125463 records
  # Keep only travelers >= 16 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 16 |                   # 108622
           (age < 0 & aage %in% c(4,5,6,7)) |
           (age < 0 & schol %in% c(5,6,7,8))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%    # 85022
  # Exclude trips with zero distance
  filter(distance_pg > 0) %>%          # 84969
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%      # 84932
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

# # Note: The TT code is included but was not used in published analyses. We
# # decided to exclude this comparison because the overall mode shares between TT
# # and MDT are not "apples to apples" - in particular, the overall transit mode
# # share in MDT was weighted to approximate known transit ridership (from RTA
# # statistics). Since this was not done in TT, the transit mode share (and thus
# # the overall mode shares) are not directly comparable.
# tt_base_3 <-
#   tt %>%                               # 139769 records
#   # Keep only travelers >= 16 years old, either through age, age bucket, or
#   # school enrollment. Note that 99 is DK/RF for AGE.
#   filter((AGE >= 16 & AGE < 99) |                   # 117605
#            (AGE == 99 & SCHOL %in% c(5,6,7,8)) |
#            (AGE == 99 & AGEB == 2)) %>%
#   # Keep only trips with nonzero distance
#   filter(DIST > 0) %>%                 # 89463
#   # Exclude missing modes
#   filter(mode_c != "missing") %>%      # 89463
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

# Analyze percents at the county and region-wide level
mdt_mode_counties <-
  pct_calculator(
    # Combine data from residents of the seven counties...
    mdt_base_3 %>% filter(home_state == 17 & home_county %in% cmap_seven_counties) %>%
      # ...with the same data, replacing county with region-wide
      rbind(mdt_base_3 %>%
              filter(home_state == 17 & home_county %in% cmap_seven_counties) %>%
              mutate(home_county = "CMAP region")),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "home_county",
    weight = "wtperfin") %>%
  # Recode counties for publication
  mutate(home_county = recode(home_county,
                              "31" = "Cook",
                              "CMAP region" = "CMAP region",
                              "97" = "Lake",
                              "43" = "DuPage",
                              "89" = "Kane",
                              "93" = "Kendall",
                              "111" = "McHenry",
                              "197" = "Will"))

# Analyze percents by household income
mdt_mode_income <-
  pct_calculator(
    # Keep all respondents with a reported household income...
    mdt_base_3 %>% filter(hhinc > 0) %>%
      # ...and add the whole set again to enable regional totals
      rbind(mdt_base_3 %>% filter(hhinc > 0) %>% mutate(hhinc = 99)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "hhinc",
    weight = "wtperfin") %>%
  # Recode for publication
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

# Do the same again, but for race and ethnicity
mdt_mode_race <-
  pct_calculator(
    # Keep all respondents with a reported race and ethnicity...
    mdt_base_3 %>% filter(race_eth != "missing") %>%
      # ...and add the whole set again to enable regional totals
      rbind(mdt_base_3 %>% filter(race_eth != "missing") %>%
                           mutate(race_eth = "CMAP region average")),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "race_eth",
    weight = "wtperfin") %>%
  # Recode for publication
  mutate(race_eth = recode_factor(factor(race_eth),
                                  "other" = "Other",
                                  "black" = "Black",
                                  "asian" = "Asian",
                                  "hispanic" = "Hispanic",
                                  "white" = "White"))

# Do the same again, but for age

# Age bins
breaks <- c(-1,29, 49, 69, 150)
age_labels <- c("16 to 29", "30 to 49",  "50 to 69", "70 and above")

mdt_mode_age <-
  pct_calculator(
    # Keep all respondents with a reported age...
    mdt_base_3 %>% filter(age > 0) %>%
                   mutate(age_bin=cut(age,breaks=breaks,labels=age_labels)) %>%
      # ...and add the whole set again to enable regional totals
      rbind(mdt_base_3 %>% filter(age > 0) %>% mutate(age_bin = "CMAP region average")),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "age_bin",
    weight = "wtperfin") %>%
  # Reorder factors for publication
  mutate(age_bin = fct_relevel(age_bin,"CMAP region average")) %>%
  mutate(age_bin = fct_rev(factor(age_bin)))

################################################################################
# Chart of mode share by home county
################################################################################

# Create labels
mode_share_p1_labels <-
  mdt_mode_counties %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(home_county) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p1 <-
  # Get data
  mdt_mode_counties %>%
  # Add labels
  left_join(mode_share_p1_labels, by = "home_county") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)
  ) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(home_county,label))) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p1_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = home_county),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.4))

finalize_plot(mode_share_p1,
              title = "Mode share for trips in northeastern Illinois by home 
              county, highlighting non-car mode share.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p1",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by income
################################################################################

# Create labels
mode_share_p2_labels <-
  mdt_mode_income %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(hhinc) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p2 <-
  # Get data
  mdt_mode_income %>%
  # Add labels
  left_join(mode_share_p2_labels, by = "hhinc") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)
  ) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = hhinc)) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p2_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = hhinc),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.6))

finalize_plot(mode_share_p2,
              title = "Mode share for trips in northeastern Illinois by household 
              income, highlighting non-car mode share.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p2",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by race
################################################################################

# Create labels
mode_share_p3_labels <-
  mdt_mode_race %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(race_eth) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p3 <-
  # Get data
  mdt_mode_race %>%
  # Add labels
  left_join(mode_share_p3_labels, by = "race_eth") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)
  ) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(str_wrap(race_eth,18),label))) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p3_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = str_wrap(race_eth,18)),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.55))

finalize_plot(mode_share_p3,
              title = "Mode share for trips in northeastern Illinois by race and 
              ethnicity, highlighting non-car mode share.",
              caption = "Note: 'Hispanic' includes respondents who identified as 
              Hispanic of any racial category. Other categories are non-Hispanic. 
              Includes trips by residents of the region that  start and/or end 
              in the Illinois counties of Cook, DeKalb, DuPage, Grundy, Kane, 
              Kendall, Lake, McHenry, and Will. Excludes trips by travelers 
              younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p3",
              # mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by age
################################################################################

# Create labels
mode_share_p4_labels <-
  mdt_mode_age %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(age_bin) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p4 <-
  # Get data
  mdt_mode_age %>%
  # Add labels
  left_join(mode_share_p4_labels, by = "age_bin") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct)
  ) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = age_bin)) +
  geom_col(aes(fill = mode_c),position = position_stack(reverse = T)) +
  geom_label(data = mode_share_p4_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = age_bin),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(n.breaks = 6, labels = scales::label_percent(),
                     limits = c(-1,.55))

finalize_plot(mode_share_p4,
              title = "Mode share for trips in northeastern Illinois by age, 
              highlighting non-car mode share.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # # height = 5.5,
              # width = 11.3,
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
