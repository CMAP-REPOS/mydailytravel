# This script produces charts and graphs to better understand the modes used to
# travel for specific purposes (e.g., how do people get to healthcare).

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(matrixStats)
library(ggplot2)
library(tidyverse)
library(slider)
library(cmapplot)

source("helper_fns.R")

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

# Create base dataset for mode analyses

mdt_base_1 <-
  mdt %>%                             # 125103 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets, school enrollment, or manual location identification
  # of school trips
  filter(age >= 5 |                   # 125099
           age < 0 & aage %in% c(2,3,4,5,6,7) |
           age < 0 & schol %in% c(4,5,6,7,8) |
           sampno %in% c(70038312,
                         70051607)) %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97014
  # Exclude trips with no travel distance
  filter(distance_pg > 0) %>%        # 96956
  # Exclude trips with no mode
  filter(mode_c != "missing") %>%    # 96919
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  ungroup()

tt_base_1 <-
  tt %>%                             # 140751 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets or school enrollment. Note that 99 is DK/RF for AGE.
  filter((AGE >= 5 & AGE < 99)|                  # 133989
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGE == 99 & AGEB == 2)) %>%
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in MDT (great circle vs. actual travel distance).
  filter(DIST > 0) %>%              # 100880
  # Remove missing modes
  filter(mode_c != "missing") %>%   # 100880
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  ungroup()


#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# DINING
################################################################################

### Calculate proportions for subcategories for dining in MDT
detailed_dining_mode_c_mdt <-
  pct_calculator(mdt_base_1,
                 subset = "dining",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of dining sub-purposes by mode
################################################################################

modes_of_tpurps_p1 <-
  detailed_dining_mode_c_mdt %>%
  mutate(tpurp = factor(tpurp,levels = c("Drive thru / take-out dining",
                                         "Ate / dined out"))) %>%
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Other modes",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  group_by(mode_c,tpurp) %>%
  summarize(pct = sum(pct)) %>%
  ggplot(aes(x = pct, y = tpurp,
             label = ifelse(pct >.045,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  scale_x_continuous(labels = scales::label_percent()) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  # ggplot(aes(y = reorder(mode_c,desc(-pct)),
  #            x = pct)) +
  # geom_col(aes(fill = tpurp),
  #          position = position_dodge2(width = .8,reverse = TRUE)) +
  theme_cmap(gridlines = "v",vline = 0) +
  # scale_x_continuous(labels = scales::label_percent(accuracy=1),n.breaks = 6,limits = c(0,.75)) +
  # geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 # group = tpurp),
             # position = position_dodge2(width = .8,reverse = T),
             # hjust = 0,
             # label.size = 0,
             # fill = "white") +
  cmap_fill_discrete(palette = "environment")

finalize_plot(modes_of_tpurps_p1,
              "Mode share of dining trips, 2019.",
              "Note: \"By car\" includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              \"Other modes\" includes transit, biking, and all other modes.
              Unlabeled bars have less than 5 percent mode share.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. ",
              # width = 11.3,
              # height = 6.3,
              filename = "modes_of_tpurps_p1",
              mode = "png",
              overwrite = T)

################################################################################
#
# HEALTHCARE
################################################################################

### Calculate proportions for subcategories for dining in MDT
detailed_health_mode_c_mdt <-
  pct_calculator(mdt_base_1,
                 subset = "health",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of healthcare sub-purposes by mode
################################################################################

modes_of_tpurps_p2 <-
  detailed_health_mode_c_mdt %>%
  mutate(tpurp = factor(tpurp,levels = c("Visited a person staying at the hospital",
                                         "Health care visit for someone else",
                                         "Health care visit for self"))) %>%
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "transit" = "Transit",
                                "walk" = "Other modes",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  group_by(mode_c,tpurp) %>%
  summarize(pct = sum(pct)) %>%
  ggplot(aes(x = pct, y = str_wrap_factor(tpurp,15),
             label = ifelse(pct >.045,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  scale_x_continuous(labels = scales::label_percent()) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  # ggplot(aes(y = reorder(mode_c,desc(-pct)), x = pct)) +
  # geom_col(aes(fill = tpurp),
  #          position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v",legend.max.columns = 3, vline = 0) +
  # geom_label(aes(label = scales::label_percent(accuracy = 1)(pct)),
  #            position = position_dodge2(reverse = TRUE,width = 0.9),
  #            hjust = 0,
  #            label.size = 0) +
  # scale_x_continuous(labels = scales::label_percent(),n.breaks = 6,
  #                    limits = c(0,.85)) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(modes_of_tpurps_p2,
              "Mode share of health trips, 2019.",
              "Note: \"By car\" includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              \"Other modes\" includes walking, biking, and all other modes.
              Unlabeled bars have less than 5 percent mode share.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. ",
              # width = 11.3,
              # height = 6.3,
              filename = "modes_of_tpurps_p2",
              mode = "png",
              overwrite = TRUE)


################################################################################
#
# COMMUNITY
################################################################################

### Calculate proportions for subcategories for community in MDT

detailed_community_mode_c_mdt <-
  pct_calculator(mdt_base_1,
                 subset = "community",
                 subset_of = "tpurp_c",
                 breakdown_by = "mode_c",
                 second_breakdown = "tpurp",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Chart of community sub-purposes by mode
################################################################################

modes_of_tpurps_p3 <-
  detailed_community_mode_c_mdt %>%
  mutate(tpurp = factor(tpurp,levels = c("Socialized with relatives",
                                         "Socialized with friends",
                                         "Attended a religious event",
                                         "Attended a community event"
                                         )),
         category = recode_factor(tpurp,
                                  "Socialized with friends" = "Friends/Family",
                                  "Socialized with relatives" = "Friends/Family",
                                  "Attended a community event" = "Civic/Religious",
                                  "Attended a religious event" = "Civic/Religious"

         )) %>%
  filter(survey == "mdt",
         category == "Friends/Family") %>%
  mutate(mode_c = recode_factor(mode_c,
                                "driver" = "By car",
                                "passenger" = "By car",
                                "walk" = "Walking",
                                "transit" = "Other modes",
                                "other" = "Other modes",
                                "bike" = "Other modes")) %>%
  group_by(mode_c,tpurp) %>%
  summarize(pct = sum(pct)) %>%
  ggplot(aes(x = pct, y = tpurp,
             label = ifelse(pct >.045,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  scale_x_continuous(labels = scales::label_percent()) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  # ggplot(aes(y = reorder(mode_c,desc(-pct)), x = pct)) +
  # geom_col(aes(fill = tpurp),
  #          position = position_dodge2(reverse = TRUE)) +
  # facet_wrap(~category) +
  # geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
  #                group = tpurp),
  #            position = position_dodge2(reverse = T, width = .9),
  #            hjust = 0,
  #            label.size = 0) +
  theme_cmap(gridlines = "v",legend.max.columns = 3, vline = 0) +
  # scale_x_continuous(labels = scales::label_percent(),
                     # limits = c(0,.9)) +
  cmap_fill_discrete(palette = "environment")

finalize_plot(modes_of_tpurps_p3,
              "Mode share of trips to visit friends and family, 2019.",
              "Note: \"By car\" includes trips as either a driver of a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              \"Other modes\" includes transit, biking, and all other modes.
              Unlabeled bars have less than 5 percent mode share.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data. ",
              # width = 6.5,
              # height = 4,
              overwrite = T,
              mode = "png",
              filename = "modes_of_tpurps_p3")

################################################################################
# Median distances for community trips
################################################################################

mdt_base_1 %>%
  filter(tpurp_c == "community") %>%
  group_by(tpurp) %>%
  summarize(distance = weightedMedian(distance_pg,wtperfin))

################################################################################
# ARCHIVE
#
################################################################################

################################################################################
# Archive - TT vs. MDT comparison of dining sub-purposes by mode
################################################################################

# ## Create totals for trips by mode category (within universe of dining trips)
#
# ### Calculate proportions for TT
# all_dining_mode_c_tt <-
#   pct_calculator(tt_base_2,
#                  subset = "dining",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
#
# ### Calculate proportions for MDT
# all_dining_mode_c_mdt <-
#   pct_calculator(mdt_base_2,
#                  subset = "dining",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
#
# ### Join MDT and TT
# total_dining_mode_c <-
#   rbind(all_dining_mode_c_tt,
#         all_dining_mode_c_mdt) %>%
#   mutate(tpurp = "Dining outside of home (all)") %>%
#   select(-total)
#
# all_dining_mode_c <-
#   rbind(total_dining_mode_c,
#         detailed_dining_mode_c_mdt)

# modes_of_tpurps_p1a <-
#   all_dining_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Dining outside of home (all)",
#                                          "Drive thru / take-out dining",
#                                          "Ate / dined out")),
#          survey = factor(survey, levels = c("tt","mdt"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-=pct)), x = =pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(~survey,ncol = 1) +
#   theme_cmap(gridlines = "v") +
#   scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
#   cmap_fill_discrete(palette = "friday")
#
# finalize_plot(modes_of_tpurps_p1a,
#               "Mode share of dining trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               width = 11.3,
#               height = 6.3,
#               filename = "modes_of_tpurps_p1a",
#               mode = "png")

################################################################################
# Archive - TT vs. MDT comparison of healthcare sub-purposes by mode
################################################################################
#
# ## Create totals for trips by mode category (within universe of health trips)
#
# ### Calculate proportions for TT
# all_health_mode_c_tt <-
#   pct_calculator(tt_base_1,
#                  subset = "health",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
#
# ### Calculate proportions for MDT
# all_health_mode_c_mdt <-
#   pct_calculator(mdt_base_1,
#                  subset = "health",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
#
# ### Join MDT and TT
# total_health_mode_c <-
#   rbind(all_health_mode_c_tt,
#         all_health_mode_c_mdt) %>%
#   mutate(tpurp = "Healthcare (all)") %>%
#   select(-total)
#
# all_health_mode_c <-
#   rbind(total_health_mode_c,
#         detailed_health_mode_c_mdt)

# modes_of_tpurps_p2a <-
#   all_health_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Healthcare (all)",
#                                          "Health care visit for self",
#                                          "Health care visit for someone else",
#                                          "Visited a person staying at the hospital")),
#          survey = factor(survey, levels = c("tt","mdt"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel (2019)",
#                                 tt = "Travel Tracker (2008)")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-mode_c_pct)), x = mode_c_pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(~survey,ncol = 1) +
#   theme_cmap(gridlines = "v",legend.max.columns = 3) +
#   scale_x_continuous(labels = scales::label_percent(),n.breaks = 6) +
#   cmap_fill_discrete(palette = "governance")
#
# finalize_plot(modes_of_tpurps_p2a,
#               "Mode share of health trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               width = 11.3,
#               height = 6.3,
#               filename = "modes_of_tpurps_p2a",
#               mode = "png",
#               overwrite = TRUE)

################################################################################
# Archive - TT vs. MDT comparison of community sub-purposes by mode
################################################################################
#

## Create totals for trips by mode category (within universe of community trips)
#
# ### Calculate proportions for TT
# all_community_mode_c_tt <-
#   pct_calculator(tt_base_1,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "weight",
#                  survey = "tt")
#
# ### Calculate proportions for MDT
# all_community_mode_c_mdt <-
#   pct_calculator(mdt_base_1,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
#
# ### Join MDT and TT
# total_community_mode_c <-
#   rbind(all_community_mode_c_tt,
#         all_community_mode_c_mdt) %>%
#   mutate(tpurp = "Community (all)")
#
# ### Calculate proportions for subcategories for community in TT
#
# detailed_community_mode_c_tt <-
#   pct_calculator(tt_base_1,
#                  subset = "community",
#                  subset_of = "tpurp_c",
#                  breakdown_by = "mode_c",
#                  second_breakdown = "tpurp",
#                  weight = "weight",
#                  survey = "tt")
#
#
# all_community_mode_c <-
#   rbind(total_community_mode_c,
#         detailed_community_mode_c_mdt,
#         detailed_community_mode_c_tt)

# modes_of_tpurps_p3a <-
#   all_community_mode_c %>%
#   mutate(tpurp = factor(tpurp,levels = c("Community (all)",
#                                          "Visit friends/relatives",
#                                          "Socialized with friends",
#                                          "Socialized with relatives",
#                                          "Civic/religious activities",
#                                          "Attended a community event",
#                                          "Attended a religious event")),
#          survey = factor(survey, levels = c("tt","mdt")),
#          category = recode_factor(tpurp,
#                                   "Community (all)" = "Overall",
#                                   "Visit friends/relatives" = "Friends/Family",
#                                   "Socialized with friends" = "Friends/Family",
#                                   "Socialized with relatives" = "Friends/Family",
#                                   "Civic/religious activities" = "Civic/Religious",
#                                   "Attended a community event" = "Civic/Religious",
#                                   "Attended a religious event" = "Civic/Religious"
#
#          )) %>%
#   mutate(survey = recode_factor(survey,
#                                 mdt = "My Daily Travel",
#                                 tt = "Travel Tracker")) %>%
#   ggplot(aes(y = reorder(mode_c,desc(-pct)), x = pct, fill = tpurp)) +
#   geom_col(position = position_dodge2(reverse = TRUE)) +
#   facet_wrap(survey~category) +
#   theme_cmap(gridlines = "v",legend.max.columns = 3) +
#   scale_x_continuous(labels = scales::label_percent()) +
#   cmap_fill_discrete(palette = "environment")
#
# finalize_plot(modes_of_tpurps_p3a,
#               "Mode share of community trips, 2008 vs. 2019.",
#               "Source: CMAP analysis of MDT and TT data.",
#               title_width = 1.8,
#               width = 11.3,
#               height = 6.3,
#               overwrite = T,
#               filename = "modes_of_tpurps_p3a",
#               mode = "png")

################################################################################
# Examination of TNC school trips - not enough records for rigorous analysis
################################################################################
#
# ### Filter data
# all_tnc_school_mdt <-
#   mdt_base_1 %>%                         # 96,821 records
#   filter(age <= 18,                      # 15,495 records
#          schol %in% c(3,4),              # 13,879 records
#          mode %in% c("rideshare",
#                      "shared rideshare",
#                      "taxi"),            # 31 records
#          tpurp_c == "school")            # 11 records
#
# all_tnc_school_mdt %>%
#   group_by(income_c, mode) %>%
#   summarize(trips = sum(wtperfin))
