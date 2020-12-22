library(tidyverse)
library(cmapplot)
library(forcats)




#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Load My Daily Travel
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, distance, arrtime, deptime, travtime, tpurp, hdist)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, hisp, race, wtperfin)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region)

# home location flag
home_wip <- region %>%
  filter(home == 1) %>% # identify home locations
  select(sampno,
         home_county = county_fips) %>%
  distinct() # keep distinct home locations based on sample

# Some households are coded as having home locations in multiple counties. Identify them.
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace multi-county samples with a county_fips of 999
home <- home_wip %>%
  mutate(home_county = case_when(
    sampno %in% two_homes ~ 999,
    TRUE ~ home_county)) %>%
  distinct()

# merge datasets
mdt <- trips %>%
  inner_join(ppl, by = c("sampno", "perno")) %>%
  inner_join(hh, by = "sampno") %>%
  inner_join(region, by = c("sampno", "locno")) %>%
  inner_join(home, by = c("sampno")) %>%
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
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>%
# distinct takes the first row for duplicates, so order by distance to get right mode
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
  select(MPO, SAMPN, PERNO, DAYNO, PLANO, locno, TPURP, MODE, DIST)

# Combine datasets
#   Remove trips ending outside the region
tt <- tt_place %>%
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>%
  inner_join(tt_hh, by = c("SAMPN", "SURVEY"))

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


setwd("~/GitHub/mydailytravel")

#################################################
#                                               #
#            Trip purpose of bike trips         #
#                                               #
#################################################


## Create totals for trips by purpose category

### Filter data
all_mdt <- mdt %>%
  filter(age < 90, age >= 5,
         mode_c != "missing",
         distance_pg > 0)

all_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5,
         mode_c != "missing",
         DIST > 0)

### Calculate total number of trips
all_purp_mdt <- all_mdt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(wtperfin)) %>%
  filter(tpurp_c != "missing")

all_purp_tt <- all_tt %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_total = sum(weight))


# Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
bike_purp_mdt <- all_mdt %>%
  filter(mode_c == "bike") %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_count = sum(wtperfin)) %>%
  inner_join(., all_purp_mdt, by = "tpurp_c") %>%
  mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
         survey = "mdt") %>% # add identifier
  arrange(tpurp_c_pct) # sort by mode share

bike_purp_tt <- all_tt %>%
  filter(mode_c == "bike") %>%
  group_by(tpurp_c) %>%
  summarize(tpurp_c_count = sum(weight)) %>%
  inner_join(., all_purp_tt, by = "tpurp_c") %>%
  mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
         survey = "tt") %>% # add identifier
  arrange(tpurp_c_pct) # sort by mode share

# Combine data from MDT and TT
bike_purp <-
  rbind(bike_purp_mdt,bike_purp_tt) %>%
  select(-tpurp_c_total) # Remove total by purpose (now that bike share is calculated)

# Graph mode share for the two surveys
bike_mode_share <-
  bike_purp %>%
  filter(tpurp_c != "other") %>%
  mutate(survey = recode_factor(survey,
                                "tt" = "Travel Tracker (2008)",
                                "mdt" = "My Daily Travel (2019)")) %>%
  ggplot(aes(x = reorder(tpurp_c,desc(tpurp_c)), y = tpurp_c_pct, fill = survey)) +
  geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "legislation")

finalize_plot(bike_mode_share,
              title = "Bicycle mode share by category of trips, 2008 vs. 2019.",
              caption = "Source: CMAP analysis of MDT and TT data. Note that categories are not precise comparisons.")


# Generate output table
bike_purp %>%
  mutate(tpurp_c_pct = paste0(round(tpurp_c_pct * 100,1),"%")) %>%
  pivot_wider(names_from = "survey",
              values_from = c("tpurp_c_count","tpurp_c_pct"))
