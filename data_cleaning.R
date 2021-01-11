
# Load recoding information
source("recoding.R")

# Load My Daily Travel
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2018 survey/Data")

# trips
trips <- read_csv("place.csv") %>%
  select(sampno, locno, perno, placeno, placeGroup, mode, arrtime, deptime, travtime, tpurp, distance, hdist)

# person info
ppl <- read_csv("person.csv") %>%
  select(sampno, perno, age, aage, schol, hisp, race, pertrips, wtperfin, tcoff, tcdays, emply_ask)

# household info
hh <- read_csv("household.csv") %>%
  select(sampno, hhinc, hhveh, wthhfin)

# location file w/region flag
region <- read_csv("location.csv") %>%
  select(sampno, locno, out_region, county_fips, home)

# Trip chains
chains <- read_csv("chains.csv")

# home location flag
home_wip <- region %>%
  filter(home == 1) %>% # identify home locations
  select(sampno,
         home_county = county_fips) %>%
  distinct() # keep distinct home locations based on sample

# There are 68 households coded as having home locations in multiple counties. Identify them.
two_homes <- (home_wip %>%
                group_by(sampno) %>%
                summarize(n = n()) %>%
                filter(n > 1))$sampno

# Replace multi-county records with a county_fips of 999
home <- home_wip %>%
  mutate(home_county = case_when(
    sampno %in% two_homes ~ 999,
    TRUE ~ home_county)) %>%
  distinct()

# Remove WIP tables
rm(home_wip, two_homes)

# merge datasets and filter for destination and distance
mdt <- trips %>% # 128,229 records
  inner_join(ppl, by = c("sampno", "perno")) %>% # 128,229 records
  inner_join(hh, by = "sampno") %>% # 128,229 records
  inner_join(region, by = c("sampno", "locno")) %>% # 128,229 records
  inner_join(home, by = c("sampno")) %>% # 128,229 records
  inner_join(chains, by = c("sampno", "perno", "placeno")) %>% # 128,229 records
  # Remove trips that leave the CMAP region
  filter(out_region==0) %>% # 126,075 records
  # Remove trips >= 100 miles
  filter(distance<100) # 125,955 records

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
  distinct(sampno, perno, placeGroup, .keep_all = TRUE) %>% # 125,103 records
  # add combined distance and time values calculated above (note some trips are missing one or more records)
  left_join(.,placeGroupStats, by = c("sampno","perno","placeGroup")) # 125,103 records

# Remove placegroup stats
rm(placeGroupStats)

# Load Travel Tracker
# Downloaded from CMAP data portal; exported from Microsoft Access database to csv.
setwd("C:/Users/dcomeaux/OneDrive - Chicago Metropolitan Agency for Planning/My Daily Travel 2020/2008 survey")

# Household
tt_hh <- read_csv("hh_public.csv") %>%
  select(SAMPN, SURVEY, ASSN, DAY, HHVEH, INCOM)

# people
tt_ppl <- read_csv("per_public.csv") %>%
  select(SAMPN, PERNO, MPO_per = MPO, SURVEY, PTRIPS1, PTRIPS2, AGE, AGEB, HISP, RACE, WGTP)

# trips
tt_place <- read_csv("place_public.csv") %>%
  select(MPO, SAMPN, PERNO, DAYNO, PLANO, locno, TPURP, MODE, DIST, TRPDUR)

# home location
tt_home <- read_csv("loc_public.csv") %>%
  select(LOCNO, FIPS) %>%
  mutate(home = case_when(
    substr(LOCNO,1,1) == "9" ~ 1,
    TRUE ~ 0)) %>%
  filter(home == 1) %>%
  # Extract FIPS for county from larger FIPS code
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

test1 %>%
  left_join(.,
            number,
            by = "SAMPN") %>%
  filter(is.na(n))
# Answer: Yes - except for 14 records with no sample number

# remove tests
rm(test1,test2,number)

# Create flag for home county location
tt_home <- tt_home %>% # 105,554 records
  select(SAMPN,home_county) %>%
  distinct() # 14,376 records


# Combine datasets
tt <- tt_place %>% # 218,945 records
  inner_join(tt_ppl, by = c("SAMPN", "PERNO")) %>% # 218,945 records
  inner_join(tt_hh, by = c("SAMPN", "SURVEY")) %>% # 218,945 records
  left_join(tt_home, by = "SAMPN") # 218,945 records (58 records lack a home county; they are kept for analyses that do not rely on home location)

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
tt <- tt %>% # 218,945 records
  filter(MPO==1) %>% # 159,856 records
  filter(DIST<100) %>% # 159,856 records
  filter(weekend==0) # 140,751 records

# Select the correct number of trips per day (based on day number)
tt <- tt %>%
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

# Recode trip chains in MDT data
mdt <- mdt %>%
  mutate(chain_bucket = case_when(
    home_to_work == 1 ~ "Work trip",
    work_to_work == 1 ~ "Work trip",
    work_to_home == 1 ~ "Return home (work)",
    home_to_shop == 1 ~ "Shopping trip",
    shop_to_shop == 1 ~ "Shopping trip",
    shop_to_home == 1 ~ "Return home (shopping)",
    TRUE ~ "Other trip"
    )) %>%
  mutate(chain_bucket = factor(chain_bucket,
                               levels = c("Work trip",
                                          "Return home (work)",
                                          "Shopping trip",
                                          "Return home (shopping)",
                                          "Other trip")))

# remove recoding helpers
rm(recode_income_buckets_mdt,recode_income_buckets_tt,
   recode_mode_buckets_mdt,recode_mode_buckets_tt,
   recode_tpurp_buckets_mdt,recode_tpurp_buckets_tt,
   recode_income_detailed_mdt,recode_income_detailed_tt,
   recode_mode_detailed_mdt,recode_mode_detailed_tt,
   recode_tpurp_detailed_mdt,recode_tpurp_detailed_tt)

setwd("~/GitHub/mydailytravel")
