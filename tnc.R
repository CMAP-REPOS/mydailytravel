
library(ggplot2)
library(lubridate)
library(tidyverse)
library(slider)
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
  select(sampno, perno, age, hisp, race, wtperfin)

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
  filter(out_region==0 & distance<=100)

mdt <- mdt %>%
  arrange(desc(distance)) %>%
  distinct(sampno, perno, placeGroup, .keep_all = TRUE)
# distinct takes the first row for duplicates, so order by distance to get right mode


# recode mode factors
mdt <- mdt %>%
  mutate(mode = factor(mode),
         mode = recode(mode,
                       "101" = "walk",
                       "102" = "personal bike",
                       "103" = "bike share",
                       "104" = "bike share",
                       "201" = "motorcyle",
                       "202" = "personal auto (driver)",
                       "203" = "personal auto (passenger)",
                       "301" = "carpool",
                       "401" = "school bus",
                       "500" = "rail and bus",
                       "501" = "bus",
                       "502" = "paratransit",
                       "503" = "paratransit",
                       "504" = "paratransit",
                       "505" = "train",
                       "506" = "local transit",
                       "509" = "transit",
                       "601" = "private shuttle",
                       "701" = "taxi",
                       "702" = "private limo",
                       "703" = "private car",
                       "704" = "rideshare",
                       "705" = "shared rideshare",
                       "801" = "airplane",
                       "997" = "other",
                       "-9"  = "missing",
                       "-1" = "beginning"))



# condense into mode categories
mdt <- mdt %>%
  mutate(mode_c = fct_collapse(mode,
                               walk = "walk",
                               bike = c("personal bike", "bike share"),
                               transit = c("rail and bus", "bus", "train", "local transit", "transit"),
                               driver = c("motorcyle", "personal auto (driver)"),
                               passenger = c("personal auto (passenger)", "carpool"),
                               other = c("school bus", "paratransit", "private shuttle",
                                         "taxi", "private limo", "private car", "rideshare",
                                         "shared rideshare", "airplane", "other"),
                               missing = "missing",
                               beginning = "beginning"))

#########

active_travel <- mdt %>%
  group_by(sampno,perno) %>%
  summarize(takes_transit = sum(mode_c == "transit"),
            takes_bike = sum(mode_c == "bike"),
            takes_walk = sum(mode_c == "walk"))


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
         wtperfin) %>%
  left_join(.,region %>% filter(home == 1) %>% select(sampno,county_fips),by = "sampno") %>%
  inner_join(.,active_travel, by = c("sampno","perno"))



foo <- tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-1))) %>%
  group_by(county_fips) %>%
  do(model = lm(tnc_use ~ takes_transit + takes_walk + takes_bike,.))

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
