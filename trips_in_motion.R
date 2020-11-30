

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
  select(sampno, locno, out_region)

chains <- read_csv("chains.csv")

# merge datasets
mdt <- trips %>%
  inner_join(ppl, by = c("sampno", "perno")) %>%
  inner_join(hh, by = "sampno") %>%
  inner_join(region, by = c("sampno", "locno")) %>%
  inner_join(chains, by = c("sampno", "perno", "placeno")) %>%
  filter(out_region==0 & distance<=100)

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
                               beginning = "beginning")) %>%
  mutate(trip_bucket = case_when(
    home_to_work == 1 ~ "Work trip",
    work_to_work == 1 ~ "Work trip",
    work_to_home == 1 ~ "Return home (work)",
    home_to_shop == 1 ~ "Shopping trip",
    shop_to_shop == 1 ~ "Shopping trip",
    shop_to_home == 1 ~ "Return home (shopping)",
    TRUE ~ "Other trip"
  )) %>%
  mutate(trip_bucket = factor(trip_bucket,
                              levels = c("Work trip",
                                         "Return home (work)",
                                         "Shopping trip",
                                         "Return home (shopping)",
                                         "Other trip")))

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



# Convert to datetime object and add day of week
tim_mdt <- mdt %>%
  mutate(arrtime = ymd_hms(arrtime),
  ) %>%
  mutate(day_of_week = wday(arrtime))

# Create helper values
threshold <- as.numeric(ymd_hms("2020-01-01 03:00:00", tz = "America/Chicago"))
day_value <- 60*60*24


# Process data
tim_mdt_wip <-
  tim_mdt %>%
  # Remove weekends
  filter(day_of_week != 1 & day_of_week != 7,
         # Remove trips > 15 hours
         travtime < 15 * 60 & travtime > 0) %>%
  # Make every trip on the same day (for analysis and graphing)
  mutate(trip_end = force_tz(ymd_hms(paste0("2020-01-01 ",
                                            substr(arrtime,12,19))),
                             tzone = "America/Chicago")) %>%
  # Make trips that end before 3am into trips on the next day (given survey timing)
  mutate(trip_end = case_when(
    trip_end <= threshold ~ trip_end + day_value,
    TRUE ~ trip_end)
  ) %>%
  # Calculate trip start time as end time minus travel time
  mutate(trip_start = trip_end - 60*travtime) %>%
  # Create trip interval using the Lubridate package
  mutate(trip_interval = interval(trip_start,trip_end,tz = "America/Chicago")) %>%
  # Create combined sample and person number
  mutate(samp_per_no = paste0(sampno,perno)) %>%
  # Create combined mode and purpose
  mutate(mode_tpurp = paste(mode,tpurp,sep = "_")) %>%
  # Create combined mode and purpose category
  mutate(mode_tpurp.c = paste(mode,tpurp.c,sep = "_"))



# Extract possible modes
possible_modes <- tibble(mode_c = unique(tim_mdt_wip$mode_c))
possible_tpurp <- tibble(tpurp = unique(tim_mdt_wip$tpurp))
possible_mode_tpurp <- tibble(mode_tpurp = unique(tim_mdt_wip$mode_tpurp))
possible_mode_tpurp.c <- tibble(mode_tpurp.c = unique(tim_mdt_wip$mode_tpurp.c))
possible_buckets <- tibble(trip_bucket = unique(tim_mdt_wip$trip_bucket))

# Calculate trips in motion by mode
trip_times <-
  # Establish sequence of times over the day (in one minute increments)
  tibble(time_band = seq.POSIXt(
    from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
    to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago"),
    by = "5 min"))

trip_times_mode_mdt <- trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_modes, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct purpose)
  mutate(mode_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_c)]),
    time_band,
    mode_c
  )) %>%
  group_by(mode_c) %>%
  # Calculate rolling average
  mutate(rolling_mode_count = slide_dbl(mode_count, mean, .before = 12, .after = 12)) %>%
  ungroup()



#### Charts


# Graph output of trips in motion by mode
chart2 <- trip_times_mode_mdt %>%
  filter(mode_c != "missing") %>%
  ggplot(aes(x = time_band,y = rolling_mode_count)) +
  geom_area(aes(fill = mode_c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "4 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(chart2,
              "Trips in motion in the CMAP region by mode on weekdays.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)


### Mode and purpose


trip_times_mode_and_purp.c_mdt <- trip_times %>%
  # Add all possible modes to each time
  full_join(.,possible_mode_tpurp.c, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct mode)
  mutate(trip_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$mode_tpurp.c)]),
    time_band,
    mode_tpurp.c
  )) %>%
  separate(col = mode_tpurp.c, into = c("mode","tpurp.c"), sep = "_") %>%
  group_by(mode,tpurp.c) %>%
  # Calculate rolling average
  mutate(rolling_count = slide_dbl(trip_count, mean, .before = 12, .after = 12)) %>%
  ungroup()


# Graph output of trips in motion by purpose for bike trips
chart3 <- trip_times_mode_and_purp.c_mdt %>%
  filter(mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp.c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "4 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(chart3,
              "Bike trips in motion by travel purpose.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)

# Graph output of trips in motion by purpose for bike trips
chart4 <- trip_times_mode_and_purp.c_mdt %>%
  filter(mode == "rideshare" | mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp.c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "6 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_grid(~mode) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(chart4,
              "TNC and bike share trips in motion by travel purpose.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)


# Graph output of trips in motion by purpose for bike trips
chart5 <- trip_times_mode_and_purp.c_mdt %>%
  filter(mode == "personal bike" | mode == "bike share") %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp.c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "6 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  facet_grid(~mode) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(chart5,
              "Bicycling trips in motion by travel purpose.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)


# Graph output of trips in motion by purpose for bike trips
chart6 <- trip_times_mode_and_purp.c_mdt %>%
  filter(mode %in% c("rail and bus", "bus", "train", "local transit", "transit")) %>%
  group_by(time_band,tpurp.c) %>%
  summarize(rolling_count = sum(rolling_count)) %>%
  ggplot(aes(x = time_band,y = rolling_count)) +
  geom_area(aes(fill = tpurp.c)) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "6 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 5) +
  cmap_fill_discrete(palette = "mobility") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"))

finalize_plot(chart6,
              "Transit trips in motion by travel purpose.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)


#### Group by trip chain buckets

trip_times_bucket_mdt <- trip_times %>%
  full_join(.,possible_buckets, by = character()) %>%
  # Calculate the number of trips that meet the criteria (in the interval and correct bucket)
  mutate(bucket_count = mapply(function(x,y) sum(tim_mdt_wip$wtperfin[which(
    x %within% tim_mdt_wip$trip_interval &
      y == tim_mdt_wip$trip_bucket)]),
    time_band,
    trip_bucket
  )) %>%
  group_by(trip_bucket) %>%
  # Calculate rolling average
  mutate(rolling_bucket_count = slide_dbl(bucket_count, mean, .before = 12, .after = 12)) %>%
  ungroup()


chart7 <- trip_times_bucket_mdt %>%
  ggplot(aes(x = time_band,y = rolling_bucket_count)) +
  geom_area(aes(fill = reorder(trip_bucket,desc(trip_bucket)))) +
  scale_x_datetime(labels = scales::date_format("%H:%M", tz = "America/Chicago"),date_breaks = "4 hours") +
  scale_y_continuous(label = scales::comma,breaks = waiver(), n.breaks = 6) +
  cmap_fill_discrete(palette = "friday") +
  theme_cmap(gridlines = "hv",
             panel.grid.major.x = element_line(color = "light gray"),
             legend.max.columns = 3)

finalize_plot(chart7,
              "Trips in motion in the CMAP region by trip chain type on weekdays.",
              "Source: CMAP analysis of My Daily Travel survey.",
              width = 10)

