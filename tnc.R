
library(ggplot2)
library(tidyverse)
library(cmapplot)



#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

# Load My Daily Travel
setwd("C:/Users/Daniel/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, distance, arrtime, deptime, travtime, tpurp)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, hisp, race, pertrips, wtperfin)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region, county_fips, home)

# merge datasets
mdt <- trips %>%
  inner_join(ppl, by = c("sampno", "perno")) %>%
  inner_join(hh, by = "sampno") %>%
  inner_join(region, by = c("sampno", "locno")) %>%
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


#########

active_travel <- mdt %>%
  group_by(sampno,perno) %>%
  summarize(takes_transit = sum(mode_c == "transit"),
            takes_bike = sum(mode_c == "bike"),
            takes_walk = sum(mode_c == "walk"),
            takes_tnc = sum((mode == "rideshare") | mode == "shared rideshare")) %>%
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))


tnc <- read_csv("person.csv") %>%
  select(sampno,
         perno,
         age,
         sex,
         hisp,
         race,
         smrtphn,
         wkstat,
         tnc_use,
         tnc_typ,
         tnc_cost,
         tnc_purp,
         disab,
         pertrips,
         wtperfin) %>%
  left_join(.,region %>% filter(home == 1) %>% select(sampno,county_fips) %>% distinct(),by = "sampno") %>%
  left_join(.,hh %>% select(sampno,hhinc,hhveh), by = "sampno") %>%
  inner_join(.,active_travel, by = c("sampno","perno")) %>%
  mutate(n = 1,
         county = as.character(county_fips),
         white = case_when(
           race == 1 ~ 1,
           TRUE ~ 0
         ),
         high_income = case_when(
           hhinc >= 8 ~ 1,
           TRUE ~ 0
         )) %>%
  select(-county_fips)

tnc <- tnc %>%
  tidyr::pivot_wider(
    .,
    names_from = county,
    id_cols = c(sampno:takes_active,white:high_income),
    values_from = n,
    names_prefix = 'county_',
    values_fill = list(n = 0))



foo <- tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-1))) %>%
  filter(!(hhinc %in% c(-9,-8,-7))) %>%
  filter(!(hhveh %in% c(-9,-8,-7)),
         pertrips != -1,
         !(race %in% c(-8,-7))) %>%
  #filter(county_fips == 31) %>%
  #group_by(county_fips) %>%
  lm(tnc_use ~
       takes_transit +
       takes_bike +
       takes_walk +
       county_31 +
       #county_43 + county_89 + county_93 +
       #county_97 + county_111 + county_197 +
       high_income +
       white +
       hhveh +
       pertrips,
      .,
      weights = wtperfin)

summary(foo)

## There appears to be a positive correlation with TNC usage and transit use


tnc_data %>%
  filter(!(tnc_use %in% c(-9,-8,-1))) %>%
  group_by(age_bucket) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

tnc_data %>%
  filter(tnc_cost >0) %>%
  group_by(age_bucket) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

# Breakdown by race
tnc_data %>%
  filter(!(race %in% c(-8,-7,-1))) %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(county_fips != -9) %>%
  mutate(race_ethn = case_when(
    race == 1 & hisp != 1 ~ "White",
    hisp == 1 ~ "Hispa",
    race == 2 & hisp != 1 ~ "Black",
    race == 3 & hisp != 1 ~ "Asian",
    race == 4 & hisp != 1 ~ "Amer. Indian/AK Native",
    race == 5 & hisp != 1 ~ "HI/Pac. Islander",
    race == 6 & hisp != 1 ~ "Multiracial",
    TRUE ~ "Other"
  )) %>%
  group_by(race,county_fips) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()

# Breakdown by home location
tnc_data %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(county_fips != -9) %>%
  group_by(county_fips) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()

tnc_data %>%
  filter(tnc_cost>0) %>%
  filter(county_fips != -9) %>%
  group_by(county_fips) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  View()
