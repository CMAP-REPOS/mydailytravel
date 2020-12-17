
library(ggplot2)
library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Load My Daily Travel
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, deptime, travtime, tpurp, distance, hdist)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, hisp, race, pertrips, wtperfin, tcoff, tcdays, emply_ask)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region, county_fips, home)

# home location flag
home <- region %>%
  filter(home == 1) %>%
  select(sampno,
         home_county = county_fips) %>%
  distinct()

# merge datasets
mdt <- trips %>%
  inner_join(ppl, by = c("sampno", "perno")) %>%
  inner_join(hh, by = "sampno") %>%
  inner_join(region, by = c("sampno", "locno")) %>%
  left_join(home, by = c("sampno")) %>%
  filter(out_region==0 & distance<100)

# add combined duration and distance for placeGroup trips
placeGroupStats <- mdt %>%
  filter(hdist >= 0,
         distance >= 0,
         travtime >= 0) %>%
  group_by(sampno,perno,placeGroup) %>%
  summarize(hdist_pg = sum(hdist, na.rm = TRUE),
            distance_pg = sum(distance, na.rm = TRUE),
            travtime_pg = sum(travtime, na.rm = TRUE))

# take care of collapsed trips with placeGroup
mdt <- mdt %>%
  # distinct takes the first row for duplicates, so order by distance to get right mode
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>%
  # add combined distance and time values calculated above
  left_join(.,placeGroupStats, by = c("sampno","perno","placeGroup"))


# Load Travel Tracker
# Downloaded from CMAP data portal; exported from Microsoft Access database to csv.
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2008 survey")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(SAMPN, SURVEY, ASSN, DAY, HHVEH, INCOM)

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN, PERNO, SURVEY, AGE, HISP, RACE, WGTP)

# trips
#   day beginning/place #1 already null in mode var
tt_place <- read_csv("place_public.csv") %>%
  select(MPO, SAMPN, PERNO, DAYNO, PLANO, locno, TPURP, MODE, DIST, TRPDUR)

# home location
tt_home <- read_csv("loc_public.csv") %>%
  select(LOCNO, FIPS) %>%
  mutate(home = case_when(
    substr(LOCNO,1,1) == "9" ~ 1,
    TRUE ~ 0)) %>%
  filter(home == 1) %>%
  mutate(home_county = as.integer(substr(FIPS,3,5))) %>%
  select(-FIPS,home) %>%
  inner_join(.,
             tt_place %>% select(SAMPN,PERNO,PLANO,locno),
             by = c("LOCNO" = "locno"))

# Check - is each sample associated with exactly one home
test1 <- tt_home %>% distinct(SAMPN,LOCNO,home_county,.keep_all = TRUE)

number <- test1 %>%
  group_by(SAMPN) %>%
  summarize(n = n())

test1 %>%
  left_join(.,
            number,
            by = "SAMPN") %>%
  filter(n > 1)

test2 <- tt_home %>% distinct(SAMPN,.keep_all = TRUE)
# Answer: Yes - except for 14 records with no sample number

tt_home <- tt_home %>%
  select(SAMPN,home_county) %>%
  distinct()


# Combine datasets
#   Remove trips ending outside the region
tt <- tt_place %>%
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>%
  inner_join(tt_hh, by = c("SAMPN", "SURVEY")) %>%
  left_join(tt_home, by = "SAMPN")

# Flag weekend trips and adjust weights accordingly
tt <- tt %>%
  mutate(weekend = if_else(SURVEY==2 & DAY==5 & DAYNO==2, 1,
                           if_else(SURVEY==2 & DAY==7 & DAYNO==1, 1, 0)),
         weekdays2 = if_else(SURVEY==2 & DAY==1 |
                               SURVEY==2 & DAY==2 |
                               SURVEY==2 & DAY==3 |
                               SURVEY==2 & DAY==4, 1, 0),
         # If respondent recorded two weekdays, divide weight in half
         weight = if_else(weekdays2==1, WGTP/2, WGTP))


#   Remove trips ending outside the region, over 100 miles, and/or on weekends
tt <- tt %>%
  filter(MPO==1 & DIST<100 & weekend==0)


# recode mode factors and group into buckets
mdt <- mdt %>%
  mutate(mode = factor(mode),
         mode = recode_factor(mode,
                              !!!recode_mode_detailed_mdt))%>%
  mutate(mode_c = fct_collapse(mode,
                               !!!recode_mode_buckets_mdt))

tt <- tt %>%
  mutate(MODE = factor(MODE),
         MODE = recode_factor(MODE,
                              !!!recode_mode_detailed_tt)) %>%
  mutate(mode_c = fct_collapse(MODE,
                               !!!recode_mode_buckets_tt))



# Recode trip purposes and group into buckets for comparison
mdt <- mdt %>%
  mutate(tpurp = factor(tpurp)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_mdt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_mdt))

tt <- tt %>%
  mutate(tpurp = factor(TPURP)) %>%
  mutate(tpurp = recode_factor(tpurp,
                               !!!recode_tpurp_detailed_tt)) %>%
  mutate(tpurp_c = fct_collapse(tpurp,
                                !!!recode_tpurp_buckets_tt))


# Recode incomes and group into buckets for comparison
mdt <- mdt %>%
  mutate(income = factor(hhinc),
         income = recode_factor(income,!!!recode_income_detailed_mdt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_mdt))

tt <- tt %>%
  mutate(income = factor(INCOM),
         income = recode_factor(income,!!!recode_income_detailed_tt)) %>%
  mutate(income_c = fct_collapse(income,!!!recode_income_buckets_tt))


# Recode into race and ethnicity groups
mdt <- mdt %>%
  mutate(race_eth = recode(race,
                           "1" = "white",
                           "2" = "black",
                           "3" = "asian",
                           "4" = "other",
                           "5" = "other",
                           "6" = "other",
                           "97" = "other",
                           "-8" = "missing",
                           "-7" = "missing")) %>%
  mutate(race_eth = case_when(
    hisp == 1 ~ "hispanic",
    TRUE ~ race_eth))


setwd("~/GitHub/mydailytravel")


#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################

# Create working dataset
wfh_mdt <- mdt %>%
  # keep only employed respondents
  filter(emply_ask == 1) %>%
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


# Breakdown of wfh behavior by household income
wfh_mdt %>%
  filter(income_c != "missing") %>%
  group_by(income_c) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            wfh_today_pct = weighted.mean(x = wfh_today, w = wtperfin))

# Breakdown of wfh behavior by home county
wfh_mdt %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(home_county) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            wfh_today_pct = weighted.mean(x = wfh_today, w = wtperfin))

# Breakdown of wfh behavior by race and ethnicity
wfh_mdt %>%
  filter(race_eth != "missing") %>%
  group_by(race_eth) %>%
  summarize(wfh_pct = weighted.mean(x = wfh,w = wtperfin),
            wfh_today_pct = weighted.mean(x = wfh_today, w = wtperfin))


# Summarize data at the traveler level
wfh_person_level <-
  wfh_mdt %>%
  group_by(sampno,perno) %>%
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
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_general") %>%
  rename(flag = wfh)

# Create averages comparing those who worked from home today vs. those who did not
wfh_today <-
  wfh_person_level %>%
  group_by(wfh_today) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
  pivot_longer(cols = distance_pg:travtime_pg) %>%
  mutate(type = "wfh_today") %>%
  rename(flag = wfh_today)

# Create averages comparing those who worked from outside the home today vs. not
wfo_today <-
  wfh_person_level %>%
  group_by(wfo_today) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            pertrips = weighted.mean(pertrips, w = wtperfin),
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
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
            travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
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
wfh_chart1 <-
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

finalize_plot(wfh_chart1,
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
wfh_chart2 <-
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

finalize_plot(wfh_chart2,
              "Travel characteristics and work from home behavior by home location.",
              "Note: These estimates are based on on answers given in
              the survey component of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2)

# Specific analysis of work trips for individuals who report working from home (survey)
wfh_worktrips_person_level <-
  wfh_mdt %>%
  filter(wfo_today == 1) %>%
  filter(tpurp %in% c("Worked at fixed work location",
                      "Worked at non-fixed work location",
                      "Work related (off-site meeting)")) %>%
  group_by(sampno,perno) %>%
  summarize(pertrips = first(pertrips),
            wfh = first(wfh),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            wtperfin = first(wtperfin),
            home_county = first(home_county)) %>%
  ungroup()

wfh_worktrips_general <-
  wfh_worktrips_person_level %>%
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
wfh_chart3 <-
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

finalize_plot(wfh_chart3,
              "Travel characteristics for individuals who telecommute vs. those who do not, on days when they are <i>not</i> telecommuting.",
              "Note: These estimates are based on on answers given in
              both the survey and travel diary components of MDT.
              <br><br>
              Source: CMAP analysis of MDT data.",
              title_width = 2.5)
