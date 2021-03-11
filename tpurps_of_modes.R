# This script produces analyses and charts on the trip purposes of trips made by
# specific modes, e.g., ridesharing.

#################################################
#                                               #
#                 Load libraries                #
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
source("helper_fns.R")
source("data_cleaning.R")


# Create base dataset for mode analyses

mdt_base_2 <-
  mdt %>%                             # 125091 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets, school enrollment, or manual location identification
  # of school trips
  filter(age >= 5 |                   #
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8)) |
           sampno %in% c(70038312,
                         70051607)) %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  #
  # Exclude trips with no travel distance
  filter(distance_pg > 0) %>%        #
  # Exclude trips with no trip purpose
  filter(tpurp_c != "missing") %>%   # 96874
  # Add flag for under 18 vs. 18 and over
  mutate(under18 = ifelse(age >= 18 | aage %in% c(5,6,7),
                          "18 and over", "Under 18")) %>%
  ungroup()


tt_base_2 <-
  tt %>%                             # 137491 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets or school enrollment. Note that 99 is DK/RF for AGE.
  filter((AGE >= 5 & AGE < 99) |                  #
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGE == 99 & AGEB == 2)) %>%
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in MDT (great circle vs. actual travel distance).
  filter(DIST > 0) %>%              #
  filter(tpurp_c != "missing") %>%  # 100619 records
  # Add flag for under 18 vs. 18 and over. None of the school enrollment or age
  # buckets are precise enough to code either way.
  mutate(under18 = ifelse(AGE >= 18 & AGE < 99, "18 and over",
                          ifelse(AGE != 99,"Under 18",NA))) %>%
  ungroup()

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# CARPOOL VS PASSENGER
################################################################################

### Calculate proportions for subcategories for driver/passenger in MDT and TT
detailed_passenger_totals_under18_mdt <-
  pct_calculator(mdt_base_2,
                 subset = "passenger",
                 subset_of = "mode_c",
                 breakdown_by = "mode",
                 second_breakdown = "under18",
                 weight = "wtperfin",
                 survey = "mdt")

passenger_totals_under18_tt <-
  pct_calculator(tt_base_2,
                 subset = "passenger",
                 subset_of = "mode_c",
                 breakdown_by = "mode",
                 second_breakdown = "under18",
                 weight = "weight",
                 survey = "tt") %>%
  mutate(mode = "Passenger (all)")

all_passenger_under18 <-
  rbind(passenger_totals_under18_tt,
        detailed_passenger_totals_under18_mdt)

################################################################################
# Chart of total passenger trips, MDT vs TT
################################################################################

tpurps_of_modes_p1 <-
  all_passenger_under18 %>%
  group_by(under18,survey) %>%
  mutate(total = sum(breakdown_total)) %>%
  ungroup() %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("Passenger (all)","personal auto (passenger)","carpool"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = breakdown_total, fill = mode)) +
  geom_col(position = position_stack(reverse = T)) +
  theme_cmap() +
  facet_wrap(~under18, ncol = 1) +
  geom_label(aes(label = scales::label_comma(accuracy = 1)(total),
                 y = total),
             fill = "white",
             vjust = 0,
             label.size = 0) +
  cmap_fill_discrete(palette = "friday",reverse = F) +
  scale_y_continuous(labels = scales::label_comma(scale = 1),
                     limits = c(0,3200000))

finalize_plot(tpurps_of_modes_p1,
              "Change in daily automobile passenger trips, 2008 vs. 2019, by age.",
              "Note: Travel Tracker did not have a 'Carpool' category, and so
              'Passenger (all)' includes both types of trips.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              filename = "tpurps_of_modes_p1",
              # height = 6.3,
              # width = 11.3,
              mode = "png",
              overwrite = T
              )

################################################################################
#
# BIKE TRIPS
################################################################################

### Calculate proportions for subcategories for biking in MDT
detailed_bike_tpurp_c_mdt <-
  pct_calculator(mdt_base_2,subset = "bike",
                 subset_of = "mode_c",
                 breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of trip purposes, bike share vs. personal bike
################################################################################

tpurps_of_modes_p2 <-
  detailed_bike_tpurp_c_mdt %>%
  filter(tpurp_c != "missing") %>%

  ggplot(aes(y = reorder(tpurp_c,desc(-pct)), x = pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v",vline = 0) +
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,.38)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = mode),
             fill = "white",
             position = position_dodge2(reverse = T, width = 0.9),
             hjust = 0,
             label.size = 0) +
  cmap_fill_discrete(palette = "friday")

finalize_plot(tpurps_of_modes_p2,
              "Trip purposes of bike trips in 2019, personal vs. bike share",
              "Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              # width = 11.3,
              # height = 6.3,
              filename = "tpurps_of_modes_p2",
              mode = "png",
              overwrite = T)

################################################################################
#
# RIDESHARE VS. SHARED RIDESHARE VS. TAXI
################################################################################

## Create totals for trips by purpose category (within universe of rideshare trips)

### Calculate proportions for TT
all_tnc_tpurp_c_tt <-
  pct_calculator(tt_base_2,
                 subset = c("rideshare","shared rideshare","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "weight",
                 survey = "tt")

### Calculate proportions for MDT
all_tnc_tpurp_c_mdt <-
  pct_calculator(mdt_base_2,
                 subset = c("rideshare","shared rideshare","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "wtperfin",
                 survey = "mdt")

### Join MDT and TT
total_tnc_tpurp_c <-
  rbind(all_tnc_tpurp_c_tt,
        all_tnc_tpurp_c_mdt) %>%
  mutate(mode = case_when(
    survey == "mdt" ~ "tnc (all)",
    TRUE ~ "taxi"))


### Calculate proportions for subcategories for rideshare in MDT
detailed_tnc_tpurp_c_mdt <-
  pct_calculator(mdt_base_2 %>%
                   mutate(tpurp_c =
                            fct_collapse(tpurp_c,
                                         "all other" = c("health","recreation/fitness",
                                                         "school","transport",
                                                         "transfer","other"))),
                 subset = c("rideshare","shared rideshare","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 weight = "wtperfin",
                 survey = "mdt")

all_tnc_tpurp_c <-
  rbind(total_tnc_tpurp_c,
        detailed_tnc_tpurp_c_mdt)

################################################################################
# Chart of total TNC/taxi trips, MDT vs. TT
################################################################################

tpurps_of_modes_p3_total_labels <-
  tibble(survey = c("My Daily Travel ('19)")) %>%
  cbind(all_tnc_tpurp_c %>%
          filter(mode == "tnc (all)") %>%
          summarize(n = sum(breakdown_total))
  )

tpurps_of_modes_p3 <-
  all_tnc_tpurp_c %>%
  filter(mode != "tnc (all)") %>%
  group_by(survey,mode) %>%
  summarize(total = sum(breakdown_total)) %>%
  mutate(survey = factor(survey,levels = c("tt","mdt")),
         mode = factor(mode, levels = c("taxi","rideshare","shared rideshare"))) %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker ('08)",
                                "mdt" = "My Daily Travel ('19)")) %>%
  ggplot(aes(x = survey, y = total)) +
  geom_col(aes(fill = mode), position = position_stack(reverse = T)) +
  theme_cmap(hline = 0) +
  scale_fill_discrete(type = c("#3f0030","#36d8ca","#006b8c")) +
  geom_label(aes(label = scales::label_comma(accuracy = 1)(total),
                 group = mode,
                 fill = mode),
             position = position_stack(reverse = T),
             hjust = 0.5,
             vjust = 1,
             label.size = 0,
             color = "white",
             show.legend = FALSE) +
  geom_label(data = tpurps_of_modes_p3_total_labels,
             aes(label = scales::label_comma(accuracy = 1)(n),
                 x = survey,
                 y = n),
             vjust = 0,
             label.size = 0,
             label.padding = unit(.5,"lines")) +
  scale_y_continuous(labels = scales::label_comma(scale = 1),limits = c(0,215000))

finalize_plot(tpurps_of_modes_p3,
              "Change in daily TNC and taxi trips, 2008 vs. 2019.",
              "Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              filename = "tpurps_of_modes_p3",
              mode = "png",
              # height = 6.3,
              # width = 11.3,
              overwrite = T)

################################################################################
# Chart of trip purposes for TNC/taxi trips, MDT vs. TT
################################################################################

tpurps_of_modes_p4 <-
  all_tnc_tpurp_c %>%
  filter(tpurp_c != "missing",
         survey == "mdt",
         mode != "tnc (all)") %>%
  mutate(mode = factor(mode, levels = c("taxi","rideshare","shared rideshare")),
         tpurp_c = factor(tpurp_c,c("all other","dining","community",
                                    "shopping/errands","work","home"))) %>%
  ggplot(aes(y = tpurp_c, x = pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = mode),
             position = position_dodge2(reverse = T,
                                        width = 0.9),
             hjust = 0,
             label.size = 0,
             fill = "white") +
  theme_cmap(gridlines = "v",
             vline = 0) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, .45)) +
  scale_fill_discrete(type = c("#3f0030","#36d8ca","#006b8c"))

finalize_plot(tpurps_of_modes_p4,
              "Trip purposes of TNC and taxi trips, 2019.",
              "Note: \"All other\" includes categories with less than 5% of the
              overall mode share for taxis and TNCs. This includes healthcare,
              school, transport, transfers, and other.
              <br><br>
              Source: CMAP analysis of MDT and TT data.",
              width = 11.3,
              height = 6.3,
              filename = "tpurps_of_modes_p4",
              mode = "png",
              overwrite = T)

################################################################################
# ARCHIVE
#
################################################################################

################################################################################
# Archive - old charts on carpool vs. passenger
################################################################################
#
# ### Calculate proportions for TT
# all_passenger_tpurp_c_tt <-
#   all_passenger_tt %>%
#   # Calculate totals
#   mutate(total = sum(weight)) %>%
#   # Calculate percentages
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(weight),
#             total = median(total)) %>%
#   mutate(tpurp_c_pct = tpurp_c_total / total,
#          survey = "tt")
#
# ### Calculate proportions for MDT
# all_passenger_tpurp_c_mdt <-
#   all_passenger_mdt %>%
#   # Calculate totals
#   mutate(total = sum(wtperfin)) %>%
#   # Calculate percentages
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(wtperfin),
#             total = median(total)) %>%
#   mutate(tpurp_c_pct = tpurp_c_total / total,
#          survey = "mdt")
#
# ### Join MDT and TT
# total_passenger_tpurp_c <-
#   rbind(all_passenger_tpurp_c_tt,
#         all_passenger_tpurp_c_mdt) %>%
#   mutate(mode = "Passenger (all)") %>%
#   select(-total)
#
#
# ### Calculate proportions for subcategories for driver/passenger in MDT
# detailed_passenger_tpurp_c_mdt <-
#   all_passenger_mdt %>%
#   # Calculate totals
#   group_by(mode) %>%
#   mutate(total = sum(wtperfin)) %>%
#   # Calculate percentages
#   group_by(tpurp_c,mode) %>%
#   summarize(tpurp_c_total = sum(wtperfin),
#             total = median(total)) %>%
#   mutate(tpurp_c_pct = tpurp_c_total / total) %>%
#   select(-total) %>%
#   mutate(survey = "mdt")
#
# all_passenger_tpurp_c <-
#   rbind(total_passenger_tpurp_c,
#         detailed_passenger_tpurp_c_mdt)
#
#
# tpurps_of_modes_p2 <-
#   all_passenger_tpurp_c %>%
#   filter(tpurp_c != "missing") %>%
#   mutate(survey = factor(survey,levels = c("tt","mdt")),
#          mode = factor(mode, levels = c("Passenger (all)","carpool","personal auto (passenger)"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 "tt" = "Travel Tracker ('08)",
#                                 "mdt" = "My Daily Travel ('19)")) %>%
#   ggplot(aes(y = reorder(tpurp_c,desc(-tpurp_c_pct)), x = tpurp_c_pct, fill = mode)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(~survey,ncol = 1) +
#   theme_cmap(gridlines = "v",legend.max.columns = 3) +
#   scale_x_continuous(labels = scales::label_percent()) +
#   cmap_fill_discrete(palette = "friday")
#
# finalize_plot(tpurps_of_modes_p2,
#               "Trip purposes of passenger trips, 2008 vs. 2019.",
#               "Note: Travel Tracker did not have a 'Carpool' category, and so
#               'Passenger (all)' includes both types of trips.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               title_width = 1.8,
#               width = 11.3,
#               height = 6.3,
#               filename = "tpurps_of_modes_p2")
#
#
# tpurps_of_modes_p1a <-
#   all_passenger_tpurp_c %>%
#   filter(!(survey == "mdt" & mode == "Passenger (all)")) %>%
#   group_by(survey,mode) %>%
#   summarize(total = sum(tpurp_c_total)) %>%
#   mutate(survey = factor(survey,levels = c("tt","mdt")),
#          mode = factor(mode, levels = c("Passenger (all)","carpool","personal auto (passenger)"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 "tt" = "Travel Tracker ('08)",
#                                 "mdt" = "My Daily Travel ('19)")) %>%
#   ggplot(aes(x = survey, y = total, fill = mode)) +
#   geom_col() +
#   theme_cmap() +
#   cmap_fill_discrete(palette = "governance",reverse = TRUE) +
#   scale_y_continuous(labels = scales::label_comma(scale = 1))
#
# finalize_plot(tpurps_of_modes_p1a,
#               "Change in daily automobile passenger trips, 2008 vs. 2019.",
#               "Note: Travel Tracker did not have a 'Carpool' category, and so
#               'Passenger (all)' includes both types of trips.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               filename = "tpurps_of_modes_p1a",
#               height = 6.3,
#               width = 11.3,
#               mode = "png",
#               overwrite = T
# )



################################################################################
# ARCHIVE - old bike analyses (unusable due to data collection issues)
################################################################################

# The data collection for bike trips in TT was phased, and high bike ridership
# parts of the region were sampled in the winter.
#
## Create totals for trips by purpose category (within universe of bike trips)

# ### Calculate proportions for TT
# all_bike_tpurp_c_tt <-
#   pct_calculator(tt_base_2,subset = "bike",
#                  subset_of = "mode_c",
#                  breakdown_by = "tpurp_c",
#                  weight = "weight",
#                  survey = "tt")
#
# ### Calculate proportions for MDT
# all_bike_tpurp_c_mdt <-
#   pct_calculator(mdt_base_2,subset = "bike",
#                  subset_of = "mode_c",
#                  breakdown_by = "tpurp_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
#
# ### Join MDT and TT
# total_bike_tpurp_c <-
#   rbind(all_bike_tpurp_c_tt,
#         all_bike_tpurp_c_mdt) %>%
#   mutate(mode = "Bike (all)")
#
# all_bike_tpurp_c <-
#   rbind(total_bike_tpurp_c,
#         detailed_bike_tpurp_c_mdt)

# # Chart of bike trips, MDT vs TT
# tpurps_of_modes_p4 <-
#   all_bike_tpurp_c %>%
#   filter(!(survey == "mdt" & mode == "Bike (all)")) %>%
#   group_by(survey,mode) %>%
#   summarize(total = sum(tpurp_c_total)) %>%
#   mutate(survey = factor(survey,levels = c("tt","mdt")),
#          mode = factor(mode, levels = c("Bike (all)","bike share","personal bike"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 "tt" = "Travel Tracker ('08)",
#                                 "mdt" = "My Daily Travel ('19)")) %>%
#   ggplot(aes(x = survey, y = total, fill = mode)) +
#   geom_col() +
#   theme_cmap() +
#   cmap_fill_discrete(palette = "governance",reverse = TRUE) +
#   scale_y_continuous(labels = scales::label_comma(scale = 1))
#
# finalize_plot(tpurps_of_modes_p4,
#               "Change in daily bike trips, 2008 vs. 2019.",
#               "Note: Travel Tracker did not have a 'bike share' category, and so
#               'Bike (all)' includes both types of trips.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               filename = "tpurps_of_modes_p4",
#               height = 6.3,
#               width = 11.3,
#               mode = "png",
#               overwrite = T)
#
#
# ### Bike mode share of all travel
#
#
# ### Calculate total number of trips
# all_purp_mdt <-
#   mdt_base_2 %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(wtperfin))
#
# all_purp_tt <-
#   tt_base_2 %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(weight))
#
#
# # Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
# bike_purp_mdt <-
#   all_bike_mdt %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_count = sum(wtperfin)) %>%
#   inner_join(all_purp_mdt, by = "tpurp_c") %>%
#   mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
#          survey = "mdt") %>% # add identifier
#   arrange(tpurp_c_pct) # sort by mode share
#
# bike_purp_tt <-
#   all_bike_tt %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_count = sum(weight)) %>%
#   inner_join(all_purp_tt, by = "tpurp_c") %>%
#   mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
#          survey = "tt") %>% # add identifier
#   arrange(tpurp_c_pct) # sort by mode share
#
# # Combine data from MDT and TT
# bike_purp <-
#   rbind(bike_purp_mdt,bike_purp_tt) %>%
#   select(-tpurp_c_total) # Remove total by purpose (now that bike share is calculated)
#
# # Graph mode share for the two surveys
# tpurps_of_modes_p5 <-
#   bike_purp %>%
#   filter(tpurp_c != "other") %>%
#   mutate(survey = recode_factor(survey,
#                                 "mdt" = "My Daily Travel (2019)",
#                                 "tt" = "Travel Tracker (2008)"
#                                 )) %>%
#   ggplot(aes(x = reorder(tpurp_c,desc(tpurp_c)), y = tpurp_c_pct, fill = survey)) +
#   geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   coord_flip() +
#   theme_cmap(gridlines = "v") +
#   cmap_fill_discrete(palette = "legislation")
#
# finalize_plot(tpurps_of_modes_p5,
#               title = "Bicycle mode share by category of trips, 2008 vs. 2019.",
#               caption = "Source: CMAP analysis of MDT and TT data. Note that
#               categories are not precise comparisons.",
#               filename = "tpurps_of_modes_p5",
#               height = 6.3,
#               width = 11.3,
#               mode = "png",
#               overwrite = T)
#
#
# # Generate output table
# bike_purp %>%
#   mutate(tpurp_c_pct = paste0(round(tpurp_c_pct * 100,1),"%")) %>%
#   pivot_wider(names_from = "survey",
#               values_from = c("tpurp_c_count","tpurp_c_pct"))
