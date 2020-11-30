library(tidyverse)
library(cmapplot)
library(forcats)




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
  select(sampno, locno, out_region)

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


# Load Travel Tracker
# Downloaded from CMAP data portal; exported from Microsoft Access database to csv.
setwd("C:/Users/Daniel/OneDrive - Chicago Metropolitan Agency for Planning/travel_tracker")

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
  filter(MPO==1 & DIST<=100 & weekend==0)


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

tt <- tt %>%
  mutate(MODE = factor(MODE),
         MODE = recode(MODE,
                       "1"  = "walk",
                       "2"  = "bike",
                       "3"  = "personal auto (driver)",
                       "4"  = "personal auto (passenger)",
                       "5"  = "CTA bus",
                       "6"  = "CTA train",
                       "7"  = "Pace",
                       "8"  = "Metra",
                       "9"  = "private shuttle",
                       "10" = "paratransit",
                       "11" = "school bus",
                       "12" = "taxi",
                       "14" = "local transit",
                       "15" = "transit (many)",
                       "97" = "other",
                       "98" = "missing",
                       "99" = "missing"))

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

tt <- tt %>%
  mutate(mode_c = fct_collapse(MODE,
                               walk = "walk",
                               bike = "bike",
                               transit = c("CTA bus", "CTA train", "Pace", "Metra",
                                           "local transit", "transit (many)"),
                               driver = "personal auto (driver)",
                               passenger = "personal auto (passenger)",
                               other = c("private shuttle", "paratransit", "school bus",
                                         "taxi", "other"),
                               missing = "missing"))



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


tt <- tt %>%
  mutate(tpurp = factor(TPURP)) %>%
  mutate(tpurp = recode(tpurp,
                        "1"	= "Working at home (for pay)",
                        "2"	= "All other home activities",
                        "3" = "Work/Job",
                        "4"	= "All other activities at work",
                        "5"	= "Attending class",
                        "6"	= "All other activities at school",
                        "7"	= "Change type of transportation/transfer",
                        "8" = "Dropped off passenger from car",
                        "9"	= "Picked up passenger",
                        "10" = "Other - transportation",
                        "11" = "Work/Business related",
                        "12" = "Service private vehicle",
                        "13" = "Routine shopping",
                        "14" = "Shopping for major purpose",
                        "15" = "Household errands",
                        "16" = "Personal business",
                        "17" = "Eat meal outside of home",
                        "18" = "Health care",
                        "19" = "Civic/religious activities",
                        "20" = "Recreation/entertainment",
                        "21" = "Visit friends/relatives",
                        "24" = "Loop trip",
                        "97" = "Other")) %>%
  mutate(tpurp.c = fct_collapse(tpurp,
                                home = "All other home activities",

                                work = c("Working at home (for pay)",
                                         "Work/Job",
                                         "All other activities at work",
                                         "Work/Business related"),

                                school = c("Attending class",
                                           "All other activities at school"),

                                "shopping/errands" =
                                  c("Routine shopping",
                                    "Shopping for major purpose",
                                    "Personal business",
                                    "Service private vehicle",
                                    "Household errands"),

                                health = "Health care",

                                dining = "Eat meal outside of home",

                                community = c("Civic/religious activities",
                                              "Visit friends/relatives"),

                                "recreation/fitness" = "Recreation/entertainment",

                                transport = c("Dropped off passenger from car",
                                              "Picked up passenger",
                                              "Other - transportation"),

                                transfer = "Change type of transportation/transfer",

                                other = c("Other",
                                          "Loop trip")))


#################################################
#                                               #
#            Analysis of driver vs. passenger   #
#                                               #
#################################################

# Age bins
breaks <- c(-1, 10, 17, 25, 30, 40, 50, 60, 70, 80, 90)
age_labels <- c("5 to 9", "10 to 17", "18 to 24", "25 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 to 69", "70 to 79", "80 to 89")

mdt <- mdt %>%
  mutate(age_bin = cut(age, breaks = breaks,
                       labels = age_labels))

tt <- tt %>%
  mutate(age_bin = cut(AGE, breaks = breaks,
                       labels = age_labels))

d_and_p_total_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(age_bin) %>%
  summarise(total = sum(wthhfin))

d_vs_p_age_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_mdt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(mode_c = fct_relevel(mode_c,
                              levels = c("driver", "passenger"))) %>%
  mutate(survey = "2018 - My Daily Travel")

d_and_p_total_tt <- tt %>%
  filter(AGE < 90 & AGE>=5 & PLANO!=1 & mode_c %in% c("driver","passenger")) %>%
  group_by(age_bin) %>%
  summarise(total = sum(weight))

d_vs_p_age_tt <- tt %>%
  filter(AGE < 90 & AGE>=5 & PLANO!=1 & mode_c %in% c("driver","passenger")) %>%
  group_by(age_bin, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(d_and_p_total_tt, by = "age_bin") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(mode_c = fct_relevel(mode_c,
                              levels = c("driver", "passenger"))) %>%
  mutate(survey = "2008 - Travel Tracker")

chart1 <- rbind(d_vs_p_age_mdt %>% select(age_bin,mode_c,mode_share,survey),
      d_vs_p_age_tt  %>% select(age_bin,mode_c,mode_share,survey)) %>%
  filter(mode_c == "passenger" & age_bin != "5 to 9" & age_bin != "10 to 17") %>%
  ggplot(aes(y = age_bin, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(chart1,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 18 and older than 90.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
              )

rbind(d_vs_p_age_mdt, d_vs_p_age_tt) %>%
  group_by(mode_c,survey) %>%
  summarize(mode_count = sum(mode_count)) %>%
  pivot_wider(id_cols = c("survey"),names_from = "mode_c",values_from = c("mode_count")) %>%
  mutate(pax_share = passenger/driver)



#################################################
#                                               #
#            Trip purpose of bike trips         #
#                                               #
#################################################


## Create totals for trips by purpose category

### Filter data
all_mdt <- mdt %>%
  filter(age < 90 & age >= 5 & mode_c != "missing")

all_tt <- tt %>%
  filter(AGE < 90 & AGE >= 5 & mode_c != "missing")

### Calculate total number of trips
all_purp_mdt <- all_mdt %>%
  group_by(tpurp.c) %>%
  summarize(tpurp.c_total = sum(wtperfin)) %>%
  filter(tpurp.c != "missing")

all_purp_tt <- all_tt %>%
  group_by(tpurp.c) %>%
  summarize(tpurp.c_total = sum(weight))


# Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
bikes_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c == "bike")

bikes_tt <- tt %>%
  filter(AGE < 90 & AGE>=5 & mode_c == "bike")

bike_purp_mdt <- bikes_mdt %>%
  group_by(tpurp.c) %>%
  summarize(tpurp.c_count = sum(wtperfin)) %>%
  inner_join(., all_purp_mdt, by = "tpurp.c") %>%
  mutate(tpurp.c_pct = tpurp.c_count / tpurp.c_total, # Calculate share of bike trips out of all
         survey = "mdt") %>% # add identifier
  arrange(tpurp.c_pct) # sort by mode share

bike_purp_tt <- bikes_tt %>%
  group_by(tpurp.c) %>%
  summarize(tpurp.c_count = sum(weight)) %>%
  inner_join(., all_purp_tt, by = "tpurp.c") %>%
  mutate(tpurp.c_pct = tpurp.c_count / tpurp.c_total, # Calculate share of bike trips out of all
         survey = "tt") %>% # add identifier
  arrange(tpurp.c_pct) # sort by mode share

# Combine data from MDT and TT
bike_purp <-
  rbind(bike_purp_mdt,bike_purp_tt) %>%
  select(-tpurp.c_total) # Remove total by purpose (now that bike share is calculated)

# Graph mode share for the two surveys
bike_mode_share <-
  bike_purp %>%
  filter(tpurp.c != "other") %>%
  ggplot(aes(x = reorder(tpurp.c,desc(tpurp.c)), y = tpurp.c_pct, fill = survey)) +
  geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
  scale_y_continuous(labels = scales::label_percent()) +
  coord_flip() +
  theme_cmap(gridlines = "v")

finalize_plot(bike_mode_share,
              title = "Bicycle mode share by category of trips, 2008 vs. 2018.",
              caption = "Source: CMAP analysis of MDT and TT data. Note that categories are not precise comparisons.")


# Generate output table
bike_purp %>%
  mutate(tpurp.c_pct = paste0(round(tpurp.c_pct * 100,1),"%")) %>%
  pivot_wider(names_from = "survey",
              values_from = c("tpurp.c_count","tpurp.c_pct"))
