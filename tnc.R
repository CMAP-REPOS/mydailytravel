
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
#                  Analysis                     #
#                                               #
#################################################

active_travel <- mdt %>%
  group_by(sampno,perno) %>%
  summarize(takes_transit = sum(mode_c == "transit",na.rm = TRUE),
            takes_bike = sum(mode_c == "bike",na.rm = TRUE),
            takes_walk = sum(mode_c == "walk",na.rm = TRUE),
            takes_tnc = sum((mode == "rideshare") | (mode == "shared rideshare"),na.rm = TRUE)) %>%
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))


tnc <- read_csv("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data/person.csv") %>%
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
         wtperfin)

tnc <- tnc %>%
  inner_join(.,
             mdt %>%
               select(sampno,perno,income_c,hhveh,
                      race_eth,home_county) %>%
               distinct(),
             by = c("sampno","perno")) %>%
  inner_join(.,active_travel, by = c("sampno","perno")) %>%
  mutate(n = 1,
         county = as.character(home_county),
         white = case_when(
           race_eth == "white" ~ 1,
           TRUE ~ 0
         ),
         high_income = case_when(
           income_c %in% c("high","middle-high") ~ 1,
           TRUE ~ 0
         ))

# Age bins
breaks <- c(-1, 10, 25, 30, 40, 50, 60, 70, 80, 90)
age_labels <- c("5 to 9", "10 to 17", "18 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

tnc <- tnc %>%
  tidyr::pivot_wider(
    .,
    names_from = county,
    id_cols = c(sampno:takes_active,white:high_income),
    values_from = n,
    names_prefix = 'county_',
    values_fill = list(n = 0)) %>%
  mutate(age_bin = cut(age, breaks = breaks,
                       labels = age_labels))


tnc_use_lm <- tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(income_c != "missing") %>%
  filter(!(hhveh %in% c(-9,-8,-7)),
         pertrips != -1,
         race_eth != "missing") %>%
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

summary(tnc_use_lm)




## There appears to be a positive correlation with TNC usage and transit use



### Age-based analysis



## Look at usage by age
tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1)),
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Cost by age
tnc %>%
  filter(tnc_cost >0,
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n())

# Usage by race and ethnicity
tnc %>%
  filter(race_eth != "missing",
         !(tnc_use %in% c(-9,-8,-7,-1))) %>%
  group_by(race_eth) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n())

# Usage by home county
tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_use = weighted.mean(tnc_use,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_use)

# Cost by home county
tnc %>%
  filter(tnc_cost>0) %>%
  filter(home_county %in% cmap_counties) %>%
  group_by(home_county) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,wtperfin, na.rm = TRUE),
            n = n()) %>%
  arrange(-tnc_cost)
