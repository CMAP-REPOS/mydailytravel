
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
  select(sampno, locno, perno, placeno, placeGroup, mode, distance, arrtime, deptime, travtime)

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

