
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
  filter(out_region==0 & distance<=100)

# take care of collapsed trips with placeGroup
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

mdt <- mdt %>%
  mutate(tpurp = factor(tpurp)) %>%
  mutate(tpurp = recode(tpurp,
                        "1"	= "Typical home activities",
                        "2"	= "Worked at home (paid)",
                        "3" = "Worked at fixed work location",
                        "4"	= "Worked at non-fixed work location",
                        "5"	= "Work related (off-site meeting)",
                        "6"	= "Attended school or daycare / studied",
                        "7"	= "Volunteered",
                        "8" = "Shopped (non-routine like for appliances, cars, home furnishings)",
                        "9"	= "Shopped (routine like grocery, clothing)",
                        "10" = "Drive-thru errands (ATM, dry cleaning, pharmacy, etc.)",
                        "11" = "Serviced a vehicle (purchased gas, regular maintenance)",
                        "12" = "Health care visit for self",
                        "13" = "Health care visit for someone else",
                        "14" = "Visited a person staying at the hospital",
                        "15" = "Non-shopping errands (banking, post office, government, etc.)",
                        "16" = "Drive thru / take-out dining",
                        "17" = "Ate / dined out",
                        "18" = "Socialized with friends",
                        "19" = "Socialized with relatives",
                        "20" = "Attended a community event",
                        "21" = "Attended a religious event",
                        "22" = "Exercised outdoors",
                        "23" = "Went to the gym",
                        "24" = "Other recreation",
                        "25" = "Attended a major special event",
                        "26" = "Drop off / Pick up passenger(s) / child(ren)",
                        "27" = "Accompanied someone else",
                        "28" = "Changed travel mode / transferred",
                        "97" = "Something else",
                        "-7" = "Missing",
                        "-8" = "Missing",
                        "-9" = "Missing"
  ))

# condense into trip purpose categories
mdt <- mdt %>%
  mutate(tpurp.c = fct_collapse(tpurp,

                                home = "Typical home activities",

                                work = c("Worked at home (paid)",
                                         "Worked at fixed work location",
                                         "Worked at non-fixed work location",
                                         "Work related (off-site meeting)"),

                                school = "Attended school or daycare / studied",

                                "shopping/errands" =
                                  c("Shopped (non-routine like for appliances, cars, home furnishings)",
                                    "Shopped (routine like grocery, clothing)",
                                    "Drive-thru errands (ATM, dry cleaning, pharmacy, etc.)",
                                    "Non-shopping errands (banking, post office, government, etc.)",
                                    "Serviced a vehicle (purchased gas, regular maintenance)"),

                                health = c("Health care visit for self",
                                           "Health care visit for someone else",
                                           "Visited a person staying at the hospital"),

                                dining = c("Drive thru / take-out dining",
                                           "Ate / dined out"),

                                community = c("Socialized with friends",
                                              "Socialized with relatives",
                                              "Attended a community event",
                                              "Attended a religious event"),

                                "recreation/fitness" = c("Other recreation",
                                                         "Attended a major special event",
                                                         "Exercised outdoors",
                                                         "Went to the gym"),

                                transport = c("Drop off / Pick up passenger(s) / child(ren)",
                                              "Accompanied someone else"),

                                transfer = "Changed travel mode / transferred",

                                other = c("Something else",
                                          "Volunteered"),

                                missing = "Missing")
  )


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
