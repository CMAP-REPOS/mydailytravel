
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
  filter(MPO==1 & DIST<=100 & weekend==0)


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
    hisp %in% c(-8,-7,97) ~ "missing",
    TRUE ~ race_eth))


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


### Same analysis, looking at income instead

d_and_p_total_inc_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(income_c) %>%
  summarise(total = sum(wthhfin))

d_vs_p_inc_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_inc_mdt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2018 - My Daily Travel")

d_and_p_total_inc_tt <- tt %>%
  filter(AGE < 90 & AGE>=5 & PLANO!=1 & mode_c %in% c("driver","passenger")) %>%
  group_by(income_c) %>%
  summarise(total = sum(weight))

d_vs_p_inc_tt <- tt %>%
  filter(AGE < 90 & AGE>=5 & PLANO!=1 & mode_c %in% c("driver","passenger")) %>%
  group_by(income_c, mode_c) %>%
  summarise(mode_count = sum(weight)) %>%
  left_join(d_and_p_total_inc_tt, by = "income_c") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2008 - Travel Tracker")

chart_passengers_income <- rbind(d_vs_p_inc_mdt %>%
                                   select(income_c,mode_c,mode_share,survey),
                                 d_vs_p_inc_tt  %>%
                                   select(income_c,mode_c,mode_share,survey)) %>%
  filter(mode_c == "passenger", income_c != "missing") %>%
  ggplot(aes(y = income_c, x = mode_share, fill = survey)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(chart_passengers_income,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver, over time.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 90.<br><br>
              Source: CMAP analysis of Travel Tracker and My Daily Travel surveys.",
)


### Same analysis, looking at race and ethnicity instead - only looking at MDT
### since TT only asked about race and ethnicity for primary household
### responder.

d_and_p_total_race_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(race_eth) %>%
  summarise(total = sum(wthhfin))

d_vs_p_race_mdt <- mdt %>%
  filter(age < 90 & age>=5 & mode_c %in% c("driver","passenger")) %>%
  group_by(race_eth, mode_c) %>%
  summarise(mode_count = sum(wthhfin)) %>%
  left_join(d_and_p_total_race_mdt, by = "race_eth") %>%
  mutate(mode_share = (mode_count / total)) %>%
  mutate(survey = "2018 - My Daily Travel")


chart_passengers_race <- d_vs_p_race_mdt %>%
  select(race_eth,mode_c,mode_share,survey) %>%
  filter(mode_c == "passenger", race_eth != "missing") %>%
  ggplot(aes(y = reorder(race_eth,desc(mode_share)), x = mode_share)) +
  geom_bar(stat = "identity", position = position_dodge2(reverse = TRUE)) +
  theme_cmap(gridlines = "v") +
  cmap_fill_discrete(palette = "mobility") +
  scale_x_continuous(labels = scales::label_percent())


finalize_plot(chart_passengers_race,
              title = "Share of weekday car trips in the CMAP region where the
              traveler is a passenger and not a driver.",
              caption = "Note: Excludes trips out of the CMAP region, as well as
              travelers younger than 5 and older than 90.<br><br>
              Source: CMAP analysis of My Daily Travel surveys.",
)
