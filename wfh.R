# This script analyzes travel behavior for individuals that work from home
# and/or telecommute.
#
# NOTE: I need to think more about the nuances of individuals that telecommute
# vs individuals that work at home and also travel around (e.g., realtors)

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

source("data_cleaning.R")

# Summary statistics (includes individuals with no trips)
wfh_mdt_all <-
  mdt_all_respondents %>% # 30,683 records
  # keep only employed respondents (either those who report having 1+ jobs or
  # those who report being employed)
  filter(emply_ask == 1 | jobs > 0) %>% # 17,656 records
  # Create a flag for people who telecommute and/or work from home
  mutate(tc =
           case_when(
             tcdays >= 1 ~ 1,
             TRUE ~ 0),
         wfh = case_when(
           wplace == 2 ~ 1,
           TRUE ~ 0
         )) %>%
  ungroup()

# Create working dataset for charts (relies on individuals with trips)
wfh_mdt <-
  mdt %>% # 125,103 records
  # Add a flag for whether the respondent worked from home or in general today
  left_join(mdt %>%
              mutate(
                # Identify trips that were work-from-home
                wfh_today = case_when(
                  tpurp == "Worked at home (paid)" ~ 1,
                  TRUE ~ 0),
                # Identify other work trips (WFO = Work from outside the home)
                wfo_today = case_when(
                  tpurp %in% c("Worked at fixed work location",
                               "Worked at non-fixed work location",
                               "Work related (off-site meeting)") ~ 1,
                  TRUE ~ 0)) %>%
              group_by(sampno,perno) %>%
              # Flag any travelers with at least one tc or work trip
              summarize(wfh_today = max(0,min(1,sum(wfh_today))),
                        wfo_today = max(0,min(1,sum(wfo_today)))),
            # Join these new flags to the above data set
            by = c("sampno","perno")) %>%
  # Remove trips by individuals that do not appear in the filtered list of
  # employed individuals above. For those that are in the list, add the flags we
  # developed on telecommuting and working from home.
  inner_join(wfh_mdt_all %>%
               select(sampno,perno,tc,wfh),
             by = c("sampno","perno"))

# Summarize data at the traveler level
wfh_person_level <-
  wfh_mdt %>% # 83025 records
  group_by(sampno,perno) %>% # 17524 groups
  summarize(wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE)) %>%
  ungroup()

wfh_person_level_plus_no_trips <-
  wfh_mdt_all %>%
  left_join(wfh_person_level, by = c("sampno","perno")) %>%
  mutate(across(all_of(c("wfh_today","wfo_today","travtime_pg","distance_pg")),
                ~replace_na(.,0)))

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################



################################################################################
# Summary statistics about TC and WFH
################################################################################

# Overview of telecommuting
wfh_mdt_all %>%
  mutate(tc_or_wfh = case_when(
    tc == 1 ~ 1,
    wfh == 1 ~ 1,
    TRUE ~ 0
  )) %>%
  summarize(tc_pct = weighted.mean(x=tc,w=wtperfin),
            wfh_pct = weighted.mean(x=wfh,w=wtperfin),
            tc_or_wfh_pct = weighted.mean(x=tc_or_wfh,w=wtperfin),
            tc = sum(wtperfin*tc),
            wfh = sum(wtperfin*wfh))

# Breakdown of tc behavior by household income
wfh_mdt_all %>% # 17,656 records
  filter(income_c != "missing") %>% # 17,516 records
  group_by(income_c) %>%
  summarize(tc_pct = weighted.mean(x = tc,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(tcers = n * tc_pct) %>%
  mutate(tcers_pct = tcers / sum(.$tcers))

# Breakdown of tc behavior by home county
wfh_mdt_all %>% # 17,656 records
  filter(home_county %in% cmap_counties) %>% # 17,220 records
  group_by(home_county) %>%
  summarize(tc_pct = weighted.mean(x = tc,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(tcers = n * tc_pct) %>%
  mutate(tcers_pct = tcers / sum(.$tcers))

# Breakdown of tc behavior by race and ethnicity
wfh_mdt_all %>% # 17,656 records
  filter(race_eth != "missing") %>% # 17,599 records
  group_by(race_eth) %>%
  summarize(tc_pct = weighted.mean(x = tc,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(tcers = n * tc_pct) %>%
  mutate(tcers_pct = tcers / sum(.$tcers))

################################################################################
#
# WFH/TC ALL TRIPS
################################################################################

### Create chart with travel characteristics for those who report working from
### home on days when they do and do not work from home

wfh_with_today_status <-
  wfh_person_level_plus_no_trips %>%
  mutate(
    category = case_when(
      wfh_today == 1 & wfo_today == 1 ~ 1,
      wfh_today == 1 ~ 2,
      wfo_today == 1 ~ 3,
      TRUE ~ 4),
    wfh_andor_tc = case_when(
      wfh == 1 ~ 1,
      tc == 1 ~ 1,
      TRUE ~ 0)) %>%
  group_by(category,wfh_andor_tc) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "tc_with_today_status")

################################################################################
# Chart of trip distances for individuals depending on their WFH/TC status and
# today's travel behavior
################################################################################

wfh_p1 <-
  wfh_with_today_status %>%
  filter(name != "travtime_pg") %>%
  mutate(flag = recode_factor(wfh_andor_tc,
                              "1" = "Telecommute at least one day a week and/or work from home",
                              "0" = "Do not regularly telecommute or work from home"),
         xmax = case_when(
           name == "distance_pg" ~ 45,
           name == "pertrips" ~ 5.95
         ),
         name = recode_factor(name,
                              "distance_pg" = "Distance (mi.)",
                              "pertrips" = "Trips")) %>%
  mutate(category = recode_factor(category,
                                  "2" = "Only worked at \nhome today",
                                  "4" = "Did not work today",
                                  "1" = "Worked both from \nhome and outside \nthe home today",
                                  "3" = "Only worked outside \nthe home today")) %>%
  mutate(category = factor(category, levels = c("Only worked at \nhome today",
                                                "Did not work today",
                                                "Worked both from \nhome and outside \nthe home today",
                                                "Only worked outside \nthe home today"))) %>%
  filter(name == "Distance (mi.)") %>%
  ggplot(aes(x = value, y = category, fill = flag)) +
  geom_col(position = position_dodge2(reverse = T)) +
  # facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",legend.max.columns = 1,
             vline = 0, xlab = "Distance (mi.)") +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
                 group = flag),
             position = position_dodge2(reverse = T, width = 0.9),
             label.size = 0,
             hjust = 0,
             fill = "white") +
  geom_blank(aes(x = xmax)) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(wfh_p1,
              "Travel characteristics individuals who telecommute or work from
              home vs. those who do not, on days when they are and are not
              working from home.",
              "Note: These estimates are based on on answers given in
              both the survey and travel diary components of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2.5,
              width = 11.3,
              height = 6.3,
              filename = "wfh_p1",
              mode = "png",
              overwrite = T)


################################################################################
#
# WFH/TC WORK TRIPS
################################################################################

# Specific analysis of work trips for individuals who report telecommuting or
# working from home (survey)
wfh_worktrips_person_level <-
  wfh_mdt %>%
  # Look only at the universe of individuals who had a work trip outside of the
  # home today and did not also work from home
  filter(wfo_today == 1 & wfh_today == 0) %>%
  # Flag for individuals that work at a home workplace or have at least one
  # telecommute day per week
  mutate(wfh_andor_tc = case_when(
    wplace == 2 ~ 1,
    tcdays > 0 ~ 1,
    TRUE ~ 0)) %>%
  # Examine only trips that were part of a work trip chain
  filter(chain %in% c("Work trip","Return home (work)")) %>%
  # Remove "beginning" trips
  filter(mode != "beginning") %>%
  group_by(sampno,perno) %>%
  summarize(pertrips = n(),
            wfh_andor_tc = first(wfh_andor_tc),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            wtperfin = first(wtperfin)) %>%
  ungroup()

wfh_worktrips_general <-
  wfh_worktrips_person_level %>%
  group_by(wfh_andor_tc) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  rename(flag = wfh_andor_tc)

################################################################################
# Chart of work trips for individuals who report working from home sometimes but
# worked from the office today vs. those who do not telecommute but worked from
# the office today
################################################################################

wfh_p2 <-
  wfh_worktrips_general %>%
  filter(name != "travtime_pg") %>%
  mutate(flag = recode(flag,
                       "0" = "Do not telecommute or work from home and worked
                       from outside the home today",
                       "1" = "Work from home and/or telecommute at least one day
                       a week and worked from outside the home today"),
         name = recode_factor(name,
                              "distance_pg" = "Distance (mi.)",
                              "pertrips" = "Trips"),
         xmax = case_when(
           name == "Distance (mi.)" ~ 45,
           name == "Trips" ~ 4.9
         )) %>%
  ggplot(aes(x = value, y = stringr::str_wrap(flag,20))) +
  geom_col(aes(fill = name)) +
  facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             legend.position = "none", vline = 0) +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(value)),
             hjust = 0,
             label.size = 0) +
  geom_blank(aes(x = xmax)) +
  cmap_fill_discrete(palette = "mobility")

finalize_plot(wfh_p2,
              "Travel characteristics for trips in work trip chains of
              individuals who telecommute and/or work from home vs. those who do
              not, on days when they are only working outside the home.",
              "Note: These estimates are based on on answers given in
              both the survey and travel diary components of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              filename = "wfh_p2",
              mode = "png",
              overwrite = T,
              height = 6.3,
              width = 11.3
              )

################################################################################
# Archive - old analysis
################################################################################
#
# # Create averages comparing those who report telecommuting vs. not
# wfh_general <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(tc) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "tc_general") %>%
#   rename(flag = tc)
#
# # Create averages comparing those who worked from home today vs. those who did not
# wfh_today <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(wfh_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_today") %>%
#   rename(flag = wfh_today)
#
# # Create averages comparing those who worked from outside the home today vs. not
# wfo_today <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(wfo_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfo_today") %>%
#   rename(flag = wfo_today)
#
# # Create averages comparing those who worked from outside the home today vs.
# # not, including only individuals with at least one work-related trip
# wfh_vs_wfo <-
#   wfh_person_level_plus_no_trips %>%
#   filter(wfh_today == 1 | wfo_today == 1) %>%
#   group_by(wfh_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_vs_wfo") %>%
#   rename(flag = wfh_today)
#
# # Combine into one dataset
# wfh_chart_inputs <-
#   rbind(wfh_general,
#         wfh_today,
#         wfo_today,
#         wfh_vs_wfo)
#
# # Chart of travel characteristics and work from home behavior
# wfh_p3 <-
#   wfh_chart_inputs %>%
#   mutate(flag = recode(flag,
#                        "0" = "No",
#                        "1" = "Yes"),
#          type = recode_factor(type,
#                               "wfo_today" = "Worked outside of home (today)",
#                               "wfh_today" = "Worked from home (today)",
#                               "tc_general" = "Work from home (generally)"),
#          name = recode_factor(name,
#                               "distance_pg" = "Distance (mi.)",
#                               "pertrips" = "Trips",
#                               "travtime_pg" = "Travel time (min.)")) %>%
#   filter(type != "wfh_vs_wfo") %>%
#   ggplot(aes(x = value, y = type, fill = flag)) +
#   geom_col(position = position_dodge2()) +
#   facet_wrap(~name,scales = "free_x") +
#   theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
#   cmap_fill_discrete(palette = "legislation") +
#   scale_y_discrete(labels = c("Worked outside\nof home (today)",
#                               "Worked from\nhome (today)",
#                               "Worked from\nhome (survey)"))
#
# finalize_plot(wfh_p3,
#               "Travel characteristics and work from home behavior.",
#               "Note: \"Worked from home (survey)\" is based on answers given in
#               the survey component of MDT. The other two categories are based on
#               observed trip behavior recorded in the travel diaries.
#               <br><br>
#               Source: CMAP analysis of MDT data.",
#               title_width = 2)
#
#
#
# #### REPEAT BUT COUNTY-LEVEL
#
# # Create averages comparing those who report working from home vs. not
# wfh_general_counties <-
#   wfh_person_level_plus_no_trips %>%
#   mutate(cook = case_when(
#     home_county == "31" ~ 1,
#     TRUE ~ 0
#   )) %>%
#   # Group by both tc status and Cook county status
#   group_by(tc,cook) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "tc_general") %>%
#   rename(flag = tc)
#
# # Repeat for those who worked from home today vs. those who did not
# wfh_today_counties <-
#   wfh_person_level_plus_no_trips %>%
#   mutate(cook = case_when(
#     home_county == "31" ~ 1,
#     TRUE ~ 0
#   )) %>%
#   group_by(wfh_today,cook) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_today") %>%
#   rename(flag = wfh_today)
#
# # Combine data
# wfh_chart_counties_inputs <-
#   rbind(wfh_general_counties,
#         wfh_today_counties)
#
# # Chart of travel characteristics and work from home behavior by home location
# wfh_p4 <-
#   wfh_chart_counties_inputs %>%
#   mutate(flag = recode(flag,
#                        "0" = "Do not telecommute",
#                        "1" = "Telecommute at least one day a week"),
#          type = recode_factor(type,
#                               "wfh_today" = "Worked from home (today)",
#                               "tc_general" = "Work from home (generally)"),
#          name = recode_factor(name,
#                               "distance_pg" = "Distance (mi.)",
#                               "pertrips" = "Trips",
#                               "travtime_pg" = "Travel time (min.)"),
#          cook = recode_factor(cook,
#                               "0" = "Rest of the region",
#                               "1" = "Cook County")) %>%
#   filter(type == "Work from home (generally)") %>%
#   ggplot(aes(x = value, y = cook, fill = flag)) +
#   geom_col(position = position_dodge2()) +
#   facet_wrap(~name,scales = "free_x") +
#   theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
#   cmap_fill_discrete(palette = "legislation")
#
# finalize_plot(wfh_p4,
#               "Travel characteristics and work from home behavior by home location.",
#               "Note: These estimates are based on on answers given in
#               the survey component of MDT.
#               <br><br>
#               Source: CMAP analysis of MDT data.",
#               title_width = 2)

