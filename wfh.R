library(ggplot2)
library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

source("data_cleaning.R")

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################

# Summary statistics (includes individuals with no trips)
wfh_mdt_all <-
  mdt_all_respondents %>% # 30,683 records
  # keep only employed respondents
  filter(emply_ask == 1) %>% # 17,656 records
  # Create a flag for people who work from home
  mutate(wfh = case_when(
    tcdays >= 1 ~ 1,
    TRUE ~ 0)) %>%
  ungroup()


# Breakdown of wfh behavior by household income
wfh_mdt_all %>% # 17,656 records
  filter(income_c != "missing") %>% # 17,516 records
  group_by(income_c) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(wfhers = n * wfh_pct) %>%
  mutate(wfhers_pct = wfhers / sum(.$wfhers))

# Breakdown of wfh behavior by home county
wfh_mdt_all %>% # 17,656 records
  filter(home_county %in% cmap_counties) %>% # 17,220 records
  group_by(home_county) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(wfhers = n * wfh_pct) %>%
  mutate(wfhers_pct = wfhers / sum(.$wfhers))

# Breakdown of wfh behavior by race and ethnicity
wfh_mdt_all %>% # 17,656 records
  filter(race_eth != "missing") %>% # 17,599 records
  group_by(race_eth) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            n = sum(wtperfin)) %>%
  mutate(wfhers = n * wfh_pct) %>%
  mutate(wfhers_pct = wfhers / sum(.$wfhers))


# Create working dataset for charts (relies on individuals with trips)
wfh_mdt <-
  mdt %>% # 125,103 records
  # keep only employed respondents
  filter(emply_ask == 1) %>% # 83,035 records
  # Create a flag for people who work from home
  mutate(wfh = case_when(
    tcdays >= 1 ~ 1,
    TRUE ~ 0)) %>%
  ungroup() %>%
  # Add a flag for whether the respondent worked from home or in general today
  left_join(.,
            mdt %>%
              mutate(
                # Identify trips that were work-from-home
                wfh_today = case_when(
                  tpurp == "Worked at home (paid)" ~ 1,
                  TRUE ~ 0),
                # Identify other work trips (WFO = Work from Office)
                wfo_today = case_when(
                  tpurp %in% c("Worked at fixed work location",
                               "Worked at non-fixed work location",
                               "Work related (off-site meeting)") ~ 1,
                  TRUE ~ 0)) %>%
              group_by(sampno,perno) %>%
              # Flag any travelers with at least one WFH or work trip
              summarize(wfh_today = max(0,min(1,sum(wfh_today))),
                        wfo_today = max(0,min(1,sum(wfo_today)))),
            # Join these new flags to the above data set
            by = c("sampno","perno"))

# Summarize data at the traveler level
wfh_person_level <-
  wfh_mdt %>% # 83,035 records
  group_by(sampno,perno) %>% # 17,528 groups
  summarize(pertrips = first(pertrips),
            wfh = first(wfh),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            wtperfin = first(wtperfin),
            home_county = first(home_county)) %>%
  ungroup()

# Create averages comparing those who report telecommuting vs. not
wfh_general <-
  wfh_person_level %>%
  group_by(wfh) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_general") %>%
  rename(flag = wfh)

# Create averages comparing those who worked from home today vs. those who did not
wfh_today <-
  wfh_person_level %>%
  group_by(wfh_today) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_today") %>%
  rename(flag = wfh_today)

# Create averages comparing those who worked from outside the home today vs. not
wfo_today <-
  wfh_person_level %>%
  group_by(wfo_today) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfo_today") %>%
  rename(flag = wfo_today)

# Create averages comparing those who worked from outside the home today vs.
# not, including only individuals with at least one work-related trip
wfh_vs_wfo <-
  wfh_person_level %>%
  filter(wfh_today == 1 | wfo_today == 1) %>%
  group_by(wfh_today) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_vs_wfo") %>%
  rename(flag = wfh_today)

# Combine into one dataset
wfh_chart_inputs <-
  rbind(wfh_general,
        wfh_today,
        wfo_today,
        wfh_vs_wfo)

# Chart of travel characteristics and work from home behavior
wfh_p1 <-
  wfh_chart_inputs %>%
  mutate(flag = recode(flag,
                       "0" = "No",
                       "1" = "Yes"),
         type = recode_factor(type,
                       "wfo_today" = "Worked outside of home (today)",
                       "wfh_today" = "Worked from home (today)",
                       "wfh_general" = "Work from home (generally)"),
         name = recode_factor(name,
                       "distance_pg" = "Distance (mi.)",
                       "pertrips" = "Trips",
                       "travtime_pg" = "Travel time (min.)")) %>%
  filter(type != "wfh_vs_wfo") %>%
  ggplot(aes(x = value, y = type, fill = flag)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
  cmap_fill_discrete(palette = "legislation") +
  scale_y_discrete(labels = c("Worked outside\nof home (today)",
                              "Worked from\nhome (today)",
                              "Worked from\nhome (survey)"))

finalize_plot(wfh_p1,
              "Travel characteristics and work from home behavior.",
              "Note: \"Worked from home (survey)\" is based on answers given in
              the survey component of MDT. The other two categories are based on
              observed trip behavior recorded in the travel diaries.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2)



#### REPEAT BUT COUNTY-LEVEL

# Create averages comparing those who report working from home vs. not
wfh_general_counties <-
  wfh_person_level %>%
  mutate(cook = case_when(
    home_county == "31" ~ 1,
    TRUE ~ 0
  )) %>%
  # Group by both WFH status and Cook county status
  group_by(wfh,cook) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_general") %>%
  rename(flag = wfh)

# Repeat for those who worked from home today vs. those who did not
wfh_today_counties <-
  wfh_person_level %>%
  mutate(cook = case_when(
    home_county == "31" ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(wfh_today,cook) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_today") %>%
  rename(flag = wfh_today)

# Combine data
wfh_chart_counties_inputs <-
  rbind(wfh_general_counties,
    wfh_today_counties)

# Chart of travel characteristics and work from home behavior by home location
wfh_p2 <-
  wfh_chart_counties_inputs %>%
  mutate(flag = recode(flag,
                       "0" = "Do not telecommute",
                       "1" = "Telecommute at least one day a week"),
         type = recode_factor(type,
                              "wfh_today" = "Worked from home (today)",
                              "wfh_general" = "Work from home (generally)"),
         name = recode_factor(name,
                              "distance_pg" = "Distance (mi.)",
                              "pertrips" = "Trips",
                              "travtime_pg" = "Travel time (min.)"),
         cook = recode_factor(cook,
                              "0" = "Rest of the region",
                              "1" = "Cook County")) %>%
  filter(type == "Work from home (generally)") %>%
  ggplot(aes(x = value, y = cook, fill = flag)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(wfh_p2,
              "Travel characteristics and work from home behavior by home location.",
              "Note: These estimates are based on on answers given in
              the survey component of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2)

# Specific analysis of work trips for individuals who report working from home (survey)
wfh_worktrips_person_county_level <-
  wfh_mdt %>%
  filter(wfo_today == 1) %>%
  filter(tpurp %in% c("Worked at fixed work location",
                      "Worked at non-fixed work location",
                      "Work related (off-site meeting)")) %>%
  group_by(sampno,perno) %>%
  summarize(pertrips = n(),
            wfh = first(wfh),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            wtperfin = first(wtperfin),
            home_county = first(home_county)) %>%
  ungroup()

wfh_worktrips_general <-
  wfh_worktrips_person_county_level %>%
  mutate(cook = case_when(
    home_county == "31" ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(wfh,cook) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  rename(flag = wfh)


# Chart of work trips for individuals who report working from home sometimes but
# worked from the office today vs. those who do not telecommute but worked from
# the office today
wfh_p3 <-
  wfh_worktrips_general %>%
  mutate(flag = recode(flag,
                       "0" = "Do not telecommute and worked from outside the home today",
                       "1" = "Telecommute at least one day a week and worked from outside the home today"),
         name = recode_factor(name,
                              "distance_pg" = "Distance (mi.)",
                              "pertrips" = "Trips",
                              "travtime_pg" = "Travel time (min.)"),
         cook = recode_factor(cook,
                              "0" = "Rest of the region",
                              "1" = "Cook County")) %>%
  ggplot(aes(x = value, y = cook, fill = flag)) +
  geom_col(position = position_dodge2(reverse = T)) +
  facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),legend.max.columns = 1) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(wfh_p3,
              "Travel characteristics for work trips of individuals who
              telecommute vs. those who do not, on days when they are <i>not</i>
              telecommuting.",
              "Note: These estimates are based on on answers given in
              both the survey and travel diary components of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2.5)

### Create chart with travel characteristics for those who report working from
### home on days when they do and do not work from home

wfh_with_today_status <-
  wfh_person_level %>%
  group_by(wfh_today,wfh) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
            n = n()) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_with_today_status")


wfh_p4 <-
  wfh_with_today_status %>%
  filter(name != "travtime_pg") %>%
  mutate(flag = recode(wfh,
                       "0" = "Do not regularly telecommute",
                       "1" = "Telecommute at least one day a week"),
         name = recode_factor(name,
                              "distance_pg" = "Distance (mi.)",
                              "pertrips" = "Trips"),
         wfh_today = recode_factor(wfh_today,
                              "0" = "Did not work from home today",
                              "1" = "Worked from home today")) %>%
  ggplot(aes(x = value, y = flag, fill = wfh_today)) +
  geom_col(position = position_dodge2(reverse = T)) +
  facet_wrap(~name,scales = "free_x") +
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),legend.max.columns = 1) +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(wfh_p4,
              "Travel characteristics individuals who telecommute vs. those who
              do not, on days when they are and are not telecommuting.",
              "Note: These estimates are based on on answers given in
              both the survey and travel diary components of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2.5,
              width = 11.3,
              height = 6.3,
              filename = "wfh_p4",
              mode = "png",
              overwrite = T)
