# This script produces analyses and charts on the share of car trips taken as
# passengers by travelers in the CMAP region. It was excluded from final
# publication because of concerns about the validity of comparisons between
# Travel Tracker and My Daily Travel on the topic: there were distinctions in
# the question wordings on accompanying passengers, and an unexplained (and
# potentially problematic) variation between the two surveys in the number of
# outside-of-household travelers.


#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(ggpattern)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")

# Age bins
age_breaks <- c(-1, 9, 15, 19, 29, 49, 69, 150)
age_labels <- c("5 to 9", "10 to 15", "16 to 19", "20 to 29",
                "30 to 49", "50 to 69","70 and above")

# # Breaks for 5-year increments - note that the 20-24 and 25-29 increments are
# # the only two adult categories with an increasing passenger share.
# breaks <- c(-1, 9, 14, 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 150)
# age_labels <- c("5-9", "10-14","15-17","18-19","20-24","25-29","30-34","34-39",
#                 "40-44","44-49","50-54","54-59","60-64","65-69","70+")




driver_pax_mdt <-
  mdt %>%                        # 125463 records
  # Keep those who are 16 or older, OR
  filter(age >= 16 |             # 108612 records
           # age buckets that are >= 16
           (age < 0 & aage %in% c(4,5,6,7)) |
           # those that said they were over 18
           (age <0 & age18 == 1)) %>%
  # Keep only trips with > 0 distance
  filter(distance_pg > 0) %>%    # 84961 records
  # Keep only driver or passenger trips, but exclude motorcycles
  filter(mode_c %in% c("driver", #
                       "passenger"),
         mode != "motorcycle"    # 61997 records
         ) %>%
  # Add age bins
  mutate(age_bin = cut(age, breaks = age_breaks,
                     labels = age_labels)) %>%
  ungroup()


driver_pax_tt <-
  tt %>%                         # 139769 records
  # Keep only travelers from age 16 to 98 (this handles DK/RF since that is
  # coded as 99). Also include travelers with AGEB of 2, which signifies 16+
  filter(AGE < 99) %>%           # 137281 records
  filter(AGE >= 16 | 
           (AGE == 99 & AGEB == 2)) %>%          # 115497 records
  # Exclude the first record of the day - this is the beginning record, and does
  # not represent a trip.
  filter(PLANO != 1) %>%         # 92369 records
  # Keep only trips with > 0 distance
  filter(DIST > 0) %>%           # 87875 records
  # Keep only driver and passenger trips
  filter(mode_c %in% c("driver", # 73302 records
                       "passenger")) %>%
  # Add age bins
  mutate(age_bin = cut(AGE, breaks = age_breaks,
                       labels = age_labels)) %>%
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
  # Keep only passenger statistics
  filter(mode_c == "passenger") %>% 
  # Exclude statistics on passengers without an age (these are included only for
  # MDT with age buckets).
  filter(!is.na(age_bin)) %>% 
  # Remove <18 travelers (since their share is so much higher due to non-drivers)
  filter(!(age_bin %in% c("5 to 9","10 to 15"))) %>%
  # # Factor age bin into desired order
  mutate(age_bin = factor(age_bin,
                          levels = c("70 and above","50 to 69","30 to 49",
                                     "20 to 29","16 to 19"
                                     ))) %>%
  # Create "type" for increasing vs. decreasing
  mutate(type = case_when(
    age_bin %in% c("20 to 29") ~ "Increasing passenger share",
    TRUE ~ "Decreasing passenger share"
    )) %>%
  # Factor "type"
  mutate(type = factor(type, levels = c("Increasing passenger share",
                                        "Decreasing passenger share"))) %>%
  # Rename and factor survey
  mutate(survey = recode_factor(survey,
                                tt = "Travel Tracker ('08)",
                                mdt = "My Daily Travel ('19)")) %>%
  # Create ggplot object
  ggplot(aes(y = age_bin, x = pct, pattern = type)) + 
  geom_label(aes(label = scales::label_percent(accuracy = .1)(pct),
                 group = survey),
             position = position_dodge2(width = 0.9,reverse = T),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = survey),
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

  # Call CMAP style and palette
  theme_cmap(gridlines = "v", vline = 0,
             xlab = "Share of car trips where the traveler is a passenger") +
  cmap_fill_discrete(palette = "mobility",reverse = T) +
  
  # Adjust x axis labels
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),limits = c(0,.5)) +
  
  # Adjust legend for formatting
  guides(pattern = guide_legend(order = 2,override.aes = list(fill = "white", color = "black")),
         fill = guide_legend(order = 1,override.aes = list(pattern = "none")))

# Export plot
finalize_plot(driver_pax_p1,
              title = "While most travelers in the region are shifting away from 
              passenger trips, they have become more common for some younger 
              travelers.",
              caption = "Source: Chicago Metropolitan Agency for Planning 
              analysis of Travel Tracker and My Daily Travel surveys.",
              filename = "driver_pax_p1",
              mode = "png",
              # width = 11.3,
              height = 4.5,
              width = 8,
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
  pivot_wider(id_cols = c("survey"),
              names_from = "mode_c",
              values_from = c("mode_count")) %>%
  mutate(pax_share = passenger/(passenger + driver))

################################################################################
#
# VEHICLE OCCUPANCY
################################################################################

# It appears that average vehicle occupancy for car trips captured in MDT
# actually increased slightly vs. TT. Relatedly, the share of car trips
# completed as SOV trips declined slightly. While this initially seems
# inconsistent with the passenger figures identified above, it is a result of an
# unexpected variation in household vs. non-household travelers in the vehicle.
# In MDT, more travelers are non-household members than in TT.


occupancy_mdt <-
  driver_pax_mdt %>% # 61977 records
  # Filter out passenger trips that have a party of one (since this indicates
  # inaccurate entry)
  filter(!(mode_c == "passenger" & party == 1)) %>% # 61588 records
  # Keep distinct trips based on household, location, and start/end time
  distinct(sampno,locno_pg,start_times_pg,
           travtime_pg_calc,.keep_all = TRUE) %>% # 58180 records
  # Calculate summary statistics
  summarize(occupancy = weighted.mean(party,wtperfin),
            total = sum(wtperfin),
            sov_total = sum(wtperfin * (party == 1)),
            sov_pct = sov_total/total,
            survey = "mdt")

occupancy_tt <-
  driver_pax_tt %>% # 73302 records
  # Filter out passenger trips that have a party of one (since this indicates
  # inaccurate entry)
  filter(!(mode_c == "passenger" & TOTTR == 1)) %>% # 73302 records
  # Keep distinct trips based on household, location, and start/end time
  distinct(SAMPN,locno,start_hr,start_min,
           TRPDUR,.keep_all = TRUE) %>% # 65904 records
  # Calculate summary statistics
  summarize(occupancy = weighted.mean(TOTTR,weight),
            total = sum(weight),
            sov_total = sum(weight * (TOTTR == 1)),
            sov_pct = sov_total/total,
            survey = "tt")

occupancy <-
  rbind(occupancy_mdt,occupancy_tt)



pct_calculator(
  driver_pax_mdt %>% mutate(out_of_hh = party > hhparty),
  breakdown_by = "tpurp_c",
  second_breakdown = "out_of_hh",
  weight = "wtperfin"
) %>% View()

# ################################################################################
# #
# # ARCHIVE - PASSENGER BEHAVIOR BY INCOME
# ################################################################################
# 
# driver_pax_inc_mdt <-
#   pct_calculator(driver_pax_mdt,
#                  breakdown_by = "mode_c",
#                  second_breakdown = "income_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# driver_pax_inc_tt <-
#   pct_calculator(driver_pax_tt,
#                  breakdown_by = "mode_c",
#                  second_breakdown = "income_c",
#                  weight = "weight",
#                  survey = "tt")
# 
# 
# ################################################################################
# # Chart of passenger share by income
# ################################################################################
# 
# driver_pax_p2 <- 
#   # Combine surveys
#   rbind(driver_pax_inc_mdt %>% select(income_c,mode_c,pct,survey),
#         driver_pax_inc_tt  %>% select(income_c,mode_c,pct,survey)) %>%
#   # Keep only pasenger data and exclude those with no household income data
#   filter(mode_c == "passenger", income_c != "missing") %>%
#   # Recode for presentation
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel ('19)",
#                                 tt = "Travel Tracker ('08)"),
#          income_c = recode_factor(income_c,
#                                   "high" = "$100,000 or more",
#                                   "middle-high" = "$60,000 to $99,999",
#                                   "middle-low" = "$35,000 to $59,999",
#                                   "low" = "$34,999 or less")) %>%
#   
#   # Create ggplot object
#   ggplot(aes(y = income_c, x = pct, fill = survey)) +
#   geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
#   geom_label(aes(label = scales::label_percent(accuracy = 0.1)(pct),
#                  group = survey),
#              position = position_dodge2(0.9,reverse = T),
#              hjust = 0,
#              fill = "white",
#              label.size = 0) +
#   
#   # Add CMAP style
#   theme_cmap(gridlines = "v", vline = 0) +
#   cmap_fill_discrete(palette = "mobility") +
#   
#   # Adjust axes
#   scale_x_continuous(labels = scales::label_percent(accuracy = 1),
#                      limits = c(0,.37))
# 
# finalize_plot(driver_pax_p2,
#               title = "Share of weekday car trips in the CMAP region where the
#               traveler is a passenger and not a driver, over time and by income.",
#               caption = "Note: Excludes travelers younger than 18.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of
#               Travel Tracker and My Daily Travel surveys.",
#               filename = "driver_pax_p2",
#               # mode = "png",
#               # width = 11.3,
#               # height = 6.3,
#               overwrite = T
# 
# )
# 
# ################################################################################
# #
# # ARCHIVE - PASSENGER BEHAVIOR BY RACE
# ################################################################################
# 
# ### Same analysis, looking at race and ethnicity instead - only looking at MDT
# ### since TT only asked about race and ethnicity for primary household
# ### responder, which would not be comparable with MDT's individual-level data.
# 
# driver_pax_race_mdt <-
#   pct_calculator(driver_pax_mdt,
#                  breakdown_by = "mode_c",
#                  second_breakdown = "race_eth",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 

# ################################################################################
# # Chart of passenger share by race
# ################################################################################
# 
# driver_pax_p3 <-
#   # Get data
#   driver_pax_race_mdt %>% select(race_eth,mode_c,pct,survey) %>%
#   # Keep only passenger statistics and exclude those missing race/ethnicity data.
#   filter(mode_c == "passenger", race_eth != "missing") %>%
#   # Reformat race/ethnicity for capitalization
#   mutate(race_eth = recode_factor(race_eth,
#                                   "white" = "White",
#                                   "asian" = "Asian",
#                                   "hispanic" = "Hispanic",
#                                   "other" = "Other",
#                                   "black" = "Black")) %>% 
#   
#   # Create ggplot object
#   ggplot(aes(y = reorder(race_eth,desc(pct)), x = pct)) +
#   geom_col(aes(fill = race_eth)) +
#   geom_label(aes(label = scales::label_percent(accuracy = 0.1)(pct)),
#              label.size = 0,
#              hjust = 0) +
#   
#   # Add CMAP style
#   theme_cmap(gridlines = "v", legend.position = "none", vline = 0,
#              xlab = "Share of car trips where the traveler is a passenger") +
#   cmap_fill_race(white = "White",asian = "Asian",hispanic = "Hispanic",
#                  other = "Other",black = "Black") +
#   
#   # Adjust axes
#   scale_x_continuous(labels = scales::label_percent(accuracy = 1),
#                      limits = c(0,.22))
# 
# finalize_plot(driver_pax_p3,
#               title = "When traveling by car, white residents are the least 
#               likely to be passengers.",
#               caption = "Note: Excludes travelers younger than 16. \"Hispanic\" 
#               includes all travelers who identified as Hispanic. Other groups 
#               (e.g., \"White\") are non-Hispanic.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel survey.",
#               filename = "driver_pax_p3",
#               mode = "png",
#               # height = 6.3,
#               # width = 11.3,
#               overwrite = T
# )
# 
# ################################################################################
# #
# # ARCHIVE - PASSENGER BEHAVIOR BY RACE AND INCOME
# ################################################################################
# 
# driver_pax_inc_race_mdt <-
#   driver_pax_mdt %>%
#   # Calculate totals for race and income
#   group_by(income_c,race_eth) %>%
#   mutate(total = sum(wtperfin)) %>%
#   ungroup() %>%
#   # Calculate percentages by race, income, and mode
#   group_by(income_c,race_eth,mode_c) %>%
#   summarise(mode_count = sum(wtperfin),
#             total = median(total)) %>%
#   mutate(mode_share = (mode_count / total)) %>%
#   mutate(survey = "mdt")
# 
# ################################################################################
# # Chart of passenger share by age and income
# ################################################################################
# 
# # Chart of drivers and passengers by income and race/ethnicity
# driver_pax_p4 <-
#   # Get data
#   driver_pax_inc_race_mdt %>%
#   # Filter for appropriate data
#   filter(mode_c == "passenger", income_c != "missing", race_eth != "missing") %>%
#   # Reformat
#   mutate(income_c = recode_factor(income_c,
#                                   "high" = "$100,000 or more",
#                                   "middle-high" = "$60,000 to $99,999",
#                                   "middle-low" = "$35,000 to $59,999",
#                                   "low" = "$34,999 or less"),
#          race_eth = recode_factor(race_eth,
#                                   "white" = "White",
#                                   "asian" = "Asian",
#                                   "hispanic" = "Hispanic",
#                                   "other" = "Other",
#                                   "black" = "Black")) %>% 
#   
#   # Create ggplot object
#   ggplot(aes(y = income_c, x = mode_share, fill = race_eth)) +
#   geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
#   geom_label(aes(label = scales::label_percent(accuracy = 0.1)(mode_share),
#                  group = race_eth),
#              position = position_dodge2(0.9,reverse = T),
#              hjust = 0,
#              label.size = 0,
#              fill = "white") +
#   
#   # CMAP style
#   theme_cmap(gridlines = "v",
#              axis.text.y = element_blank()) +
#   cmap_fill_race(white = "White",asian = "Asian",hispanic = "Hispanic",
#                  other = "Other",black = "Black") +
#   
#   
#   # Facet
#   facet_wrap(~income_c,scales = "free_y") +
#   
#   # Adjust axis
#   scale_x_continuous(labels = scales::label_percent(),limits = c(0,.3))
# 
# 
# finalize_plot(driver_pax_p4,
#               title = "Share of weekday car trips in the CMAP region where the
#               traveler is a passenger and not a driver, over time, by race and income.",
#               caption = "Note: Excludes travelers younger than 18 and older than
#               89.\"Hispanic\" includes all travelers who identified as Hispanic.
#               Other groups (e.g., \"White\") are non-Hispanic.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of
#               Travel Tracker and My Daily Travel surveys.",
#               filename = "driver_pax_p4",
#               # # # mode = "png",
#               # # width = 11.3,
#               # height = 6.3,
#               overwrite = T
# )

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

