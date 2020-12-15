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
  select(sampno, locno, perno, placeno, placeGroup, mode, distance, hdist,
         arrtime, deptime, travtime, tpurp)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, aage, schol, hisp, race, pertrips, wtperfin)

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
  inner_join(region %>% select(-county_fips,-home), by = c("sampno", "locno"))

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
setwd("C:/Users/Daniel/OneDrive - Chicago Metropolitan Agency for Planning/travel_tracker")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(SAMPN, SURVEY, ASSN, DAY, HHVEH, INCOM)

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN, PERNO, MPO, SURVEY, AGE, AGEB, HISP, RACE, WGTP, PTRIPS1,PTRIPS2)

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

tt <- tt %>%
  # Keep only trips in the CMAP mpo region (exluding NIRPC) and on weekdays
  filter(MPO==1 & weekend==0) %>%
  # Select the correct number of trips per day (based on day number)
  mutate(pertrips = ifelse(DAYNO == 1,PTRIPS1,PTRIPS2))


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


#################################################
#                                               #
#            Average resident behavior          #
#                                               #
#################################################


### MY DAILY TRAVEL - closely aligns with Sarah's work, seem to be right

# filter out trips we don't want to evaluate
avgtravel_mdt <- mdt %>%
  filter(
    # Keep only trips in the region
    out_region == 0,
    # Keep only trips under 100 miles
    distance_pg < 100,
    # Keep only trips with travelers at least 5 years old.
    # - note the "sampno" selections have no information but are school-aged
    # based on place
    age >= 5 | aage %in% c(2,3,4,5,6,7) |
      schol %in% c(4,5,6,7,8) | sampno %in% c(70038312, 70051607),
    # Keep only people who traveled
    pertrips > 0) %>%
  mutate(hdist_pg_weight = hdist_pg * wtperfin)

# home location flag
home_mdt <- region %>%
  filter(home == 1) %>%
  select(sampno,county_fips) %>%
  distinct()

# Calculate total number of daily travelers who take at least one trip
daily_travelers_mdt <-
  avgtravel_mdt %>%
  left_join(.,
               home_mdt,
               by = "sampno") %>%
  select(sampno,perno,wtperfin,county_fips) %>%
  distinct() %>%
  group_by(county_fips) %>%
  summarize(total_travelers = sum(wtperfin))

daily_travelers_mdt <-
  daily_travelers_mdt %>%
  rbind(.,
        tibble(county_fips = 999,
               total_travelers = sum(daily_travelers_mdt$total_travelers)))

# Calculate summary statistics by county
avgtravel_mdt %>%
  left_join(.,
            home_mdt,
            by = "sampno") %>%
  rbind(.,
        avgtravel_mdt %>% mutate(county_fips = 999)) %>%
  group_by(county_fips) %>%
  summarize(
    total_distance = sum(hdist_pg_weight),
    total_trips = sum(wtperfin),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(.,
            daily_travelers_mdt,
            by = "county_fips") %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  filter(county_fips %in% c(cmap_counties,999)) %>%
  View()


# Total distance
sum(avgtravel_mdt$hdist_pg_weight)

# Mileage per traveler
sum(avgtravel_mdt$hdist_pg_weight) / daily_travelers_mdt

# Average trip length
sum(avgtravel_mdt$hdist_pg_weight) / sum(avgtravel_mdt$wtperfin)

# Trips per person
sum(avgtravel_mdt$wtperfin) / daily_travelers_mdt




##### TRAVEL TRACKER - don't trust these numbers right now

avgtravel_tt <-
  tt %>%
  filter(
    # Keep only trips by travelers at least 5 years old
    AGE >= 5,
    # Keep only trips less than 100 miles
    DIST > 0 & DIST < 100,
    # Include only travelers who made at least one trip
    pertrips > 0
  ) %>%
  mutate(dist_weight = DIST * weight)


# calculate total number of daily travelers who take at least one trip
daily_travelers_tt_test <-
  tt_ppl %>%
  mutate(

    age5 = case_when(
      (AGE >= 5 & AGE < 99) ~ 1,
      AGEB == 2 ~ 1,
      TRUE ~ 0),

    notravel = case_when(
      SURVEY == 1 & PTRIPS1 == 0 ~ 1,
      SURVEY == 2 & PTRIPS1 == 0 & PTRIPS2 == 0 ~ 1,
      TRUE ~ 0

      )) %>%
  filter(age5 == 1,
         notravel == 0,
         MPO == 1)

daily_travelers_tt_test %>%
  summarize(sum = sum(WGTP))


# Calculate total number of daily travelers who take at least one trip
daily_travelers_tt <-
  avgtravel_tt %>%
  select(SAMPN,PERNO,DAYNO,weight,home_county) %>%
  distinct() %>% View()
  group_by(home_county) %>%
  summarize(total_travelers = sum(weight))

daily_travelers_tt <-
  daily_travelers_tt %>%
  rbind(.,
        tibble(home_county = 999,
               total_travelers = sum(daily_travelers_tt$total_travelers)))

# Calculate summary statistics by county
avgtravel_tt %>%
  rbind(.,
        avgtravel_tt %>% mutate(home_county = 999)) %>%
  group_by(home_county) %>%
  summarize(
    total_distance = sum(dist_weight),
    total_trips = sum(weight),
    avg_trip_length = total_distance / total_trips,
  ) %>%
  left_join(.,
            daily_travelers_tt,
            by = "home_county") %>%
  mutate(distance_per_capita = total_distance / total_travelers,
         trips_per_capita = total_trips / total_travelers) %>%
  filter(home_county %in% c(cmap_counties,999)) %>%
  View()


# Total distance
sum(avgtravel_tt$dist_weight)

# Mileage per traveler
sum(avgtravel_tt$dist_weight) / daily_travelers_tt

# Average trip length
sum(avgtravel_tt$dist_weight) / sum(avgtravel_tt$weight)

# Trips per person
sum(avgtravel_tt$weight) / daily_travelers_tt



tt %>%
  filter(AGE >= 5,
         DIST > 0) %>%
  summarize(total_distance = sum(DIST * weight, na.rm = TRUE),
            avg_distance = weighted.mean(DIST,weight, na.rm = TRUE))

avgtravel_tt %>%
  select(SAMPN,PERNO,DAYNO,pertrips,weight) %>%
  distinct() %>%
  summarize(total_trips = weighted.mean(pertrips,weight))
