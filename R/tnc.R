# This script analyzes TNC usage in MDT, using a combination of travel diary and
# survey entries

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(ggpattern)
library(lubridate)
library(RSocrata)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")

# Calculate the number of trips taken by travelers on active modes like transit,
# biking, and walking, as well as TNC trips
active_travel <-
  mdt %>% # 125463 records
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97374
  # Exclude trips with no travel distance.
  filter(distance_pg > 0) %>%        # 97316
  # Group data by the traveler level
  group_by(sampno,perno) %>%
  # Create flags for transit, biking, walking, and tnc trips
  summarize(takes_transit = sum(mode_c == "transit",na.rm = TRUE),
            takes_bike = sum(mode_c == "bike",na.rm = TRUE),
            takes_walk = sum(mode_c == "walk",na.rm = TRUE),
            takes_tnc = sum((mode == "rideshare") | 
                              (mode == "shared rideshare"),na.rm = TRUE)) %>%
  # Create a flag that is 1 if any of transit, biking, or walking is nonzero,
  # and zero otherwise
  mutate(takes_active = min(sum(takes_transit,takes_bike,takes_walk),1))

# Age bins
age_breaks_tnc <- c(-1, 9, 18, 29, 39, 49, 59, 150)
age_labels_tnc <- c("5 to 9", "10 to 18", "19 to 29", "30 to 39", "40 to 49",
                "50 to 59", "60 and above")

# Create working dataset based on variables pulled above and person/hh
# characteristics from other tables
tnc <-
  mdt_all_respondents %>% # 30683
  # Keep only travelers older than 18 (they are the ones that answered TNC survey
  # questions), either through age or age buckets
  filter(age > 18 |
           (age < 0 & aage %in% c(5,6,7)) |
           (age < 0 & age18 == 1)) %>% # 22957
  # Select relevant variables
  select(sampno,perno,weight,home_county,home_county_chi,
         race_eth,age,income_c,hhveh,pertrips,smrtphn,
         tnc_use,tnc_cost,tnc_purp) %>% 
  # Add information on active travel from above
  left_join(active_travel, by = c("sampno","perno")) %>%
  # Add 0s for people with no trips (who otherwise are NAs)
  mutate(across(starts_with("takes_"),~replace(.,is.na(.),0))) %>%
  # Create dummy variables for analysis
  mutate(n = 1,
         county = as.character(home_county),
         white = case_when(
           race_eth == "white" ~ 1,
           TRUE ~ 0
         ),
         black = case_when(
           race_eth == "black" ~ 1,
           TRUE ~ 0
         ),
         asian = case_when(
           race_eth == "asian" ~ 1,
           TRUE ~ 0
         ),
         hispa = case_when(
           race_eth == "latino" ~ 1,
           TRUE ~ 0
         ),
         other = case_when(
           race_eth == "other" ~ 1,
           TRUE ~ 0
         ),
         high_income = case_when(
           income_c %in% c("high","middle-high") ~ 1,
           TRUE ~ 0
         )) %>% 
  mutate(age_bin = cut(age, breaks = age_breaks_tnc,
                       labels = age_labels_tnc))

# Reshape data to add flags for each county as individual columns
tnc_wide <- tnc %>%
  pivot_wider(
    names_from = county,
    id_cols = c(sampno:takes_active,white:age_bin),
    values_from = n,
    names_prefix = 'county_',
    values_fill = list(n = 0))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
# Regression
################################################################################

# Run linear regression
tnc_use_lm <-
  # Get data
  tnc_wide %>%
  # Filter out respondents who don't have an answer for...
    # TNC use
    filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
    # Household incomes
    filter(income_c != "missing") %>%
    # Household vehicles
    filter(!(hhveh %in% c(-9,-8,-7))) %>% 
    # Trips
    filter(pertrips != -1) %>% 
    # Race and ethnicity
    filter(race_eth != "missing") %>% 
    # Smartphone
    filter(!(smrtphn %in% c(-9,-8,-7,-1))) %>%
  # Recode smartphone to be a dummy with 1 as "yes" and 0 as "no" (previously
  # "no" was coded as 2)
  mutate(smrtphn = recode(smrtphn,
                          "1" = "1",
                          "2" = "0")) %>% 
  
  # Execute linear regression
  lm(tnc_use ~
       takes_transit + # How many times did they take transit on their travel day?
       takes_bike + # How many times did they bike on their travel day?
       takes_walk + # How many times did they walk on their travel day?
       county_31 + # Is their home in Cook County?
       high_income + # Is their household income above $60,000/year?
       hhveh + # How many household vehicles do they have?
       smrtphn + # Do they have a smartphone?
       pertrips + # How many trips did they take on their travel day?
       # Are they: 
       white + # White (non-Latino)?
       black + # Black?
       hispa + # Latino (of any race)?
       asian, # Asian?
      .,
      # We use the weights to better represent the regional trends (vs. raw
      # responses)
      weights = weight
     )

summary(tnc_use_lm)

## There appears to be a positive and relatively strong correlation with TNC
## usage and transit use, as well as residence in Cook County and smartphone
## ownership. However, the overall R squared is quite low (.05).


################################################################################
# 
# Statistics about usage, cost, and purpose by age, race/ethnicity, and home 
################################################################################

################################################################################
# PLOT OF RACE | PURPOSE
################################################################################

# Age bins
age_breaks_tnc_large <- c(-1, 9, 18, 39, 59, 150)
age_labels_tnc_large <- c("5 to 9", "10 to 18", "19 to 39", "40 to 59",
                      "60 and above")

tnc_for_purposes <-
  tnc %>% # 22957
  mutate(age_bin = cut(age, breaks = age_breaks_tnc_large,
                       labels = age_labels_tnc_large)) %>%
  filter(tnc_purp > 0) %>% # 7175
  mutate(tnc_purp = recode(factor(tnc_purp,levels = c(1,2,3,5,4)),
                           "1" = "Commute (whole or part)",
                           "2" = "Commute (whole or part)",
                           "3" = "Daytime (work)",
                           "4" = "Daytime (non-work)",
                           "5" = "Late-night (non-work)"))

tnc_purpose_overall <-
  pct_calculator(tnc_for_purposes,
                 breakdown_by = "tnc_purp",
                 weight = "weight")

tnc_purpose_race <-
  pct_calculator(tnc_for_purposes %>% filter(race_eth != "missing"),
                 breakdown_by = "tnc_purp",
                 second_breakdown = "race_eth",
                 weight = "weight") %>% 
  # Add baseline totals
  rbind(tnc_purpose_overall %>% mutate(race_eth = "CMAP region")) %>% 
  # Adjust factors
  mutate(race_eth = factor(race_eth,
                           levels = c("black","asian","other","latino","white","CMAP region")))


tnc_p1_labels <-
  tnc_purpose_race %>% 
  filter(tnc_purp %in% c("Late-night (non-work)","Daytime (non-work)")) %>% 
  group_by(race_eth) %>% 
  summarize(label = sum(pct))

tnc_p1 <-
  # Get data
  tnc_purpose_race %>% 
  # Add flag for pattern
  mutate(type = case_when(race_eth == "CMAP region" ~ "1",
                          TRUE ~ "0" )) %>% 
  # Adjust values for shift around 0 axis
  mutate(pct = ifelse(!(tnc_purp %in% c("Late-night (non-work)","Daytime (non-work)")),
                      -1 * pct, pct)) %>% 
  # Add labels
  left_join(tnc_p1_labels, by = "race_eth") %>% 
  # Capitalize
  mutate(race_eth = recode_factor(race_eth,
                                  "other" = "Other",
                                  "latino" = "Latino",
                                  "black" = "Black",
                                  "asian" = "Asian",
                                  "CMAP region" = "CMAP region",
                                  "white" = "White")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(race_eth,label), fill = tnc_purp)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 30,
                              pattern_density = 0.25,
                              pattern_spacing = 0.0125,
                              pattern_key_scale_factor = 0.6,
                              position = position_stack(reverse = T),
                              width = 0.8) +
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = race_eth),
             label.size = 0,
             hjust = -.02,
             fill = "white") +
  
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-.5,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-.5,.75,by = .25))),
                     limits = c(-.5,.85)) +
  
  # Add CMAP themes
  theme_cmap(gridlines = "v",
             xlab = "Typical reason for using a TNC by race and ethnicity") +
  scale_fill_discrete(type = c("#8c0000","#efa7a7","#00093f","#006b8c")) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 3,override.aes = list(pattern = "none")))

tnc_p1_samplesize <-
  tnc_purpose_race %>% 
  ungroup() %>% 
  select(race_eth,n = total_n) %>% 
  distinct()

finalize_plot(tnc_p1,
              title = "White travelers were much more likely to report using 
              Transportation Network Companies (TNCs) for non-work trips.",
              caption = 
              paste0(
              "Note:
              'Latino' includes respondents who identified as Latino or Hispanic, 
              regardless of racial category. Other categories are non-Latino.
              Excludes travelers age 18 and younger, who were not asked about 
              TNC usage.
              <br><br>
              Sample size:
              <br>- White (",
                       tnc_p1_samplesize %>% filter(race_eth == "white") %>% select(n),
                       ");
              <br>- Asian (",
                       tnc_p1_samplesize %>% filter(race_eth == "asian") %>% select(n),
                       ");
              <br>- Latino (",
                       tnc_p1_samplesize %>% filter(race_eth == "latino") %>% select(n),
                       ");
              <br>- Other (",
                       tnc_p1_samplesize %>% filter(race_eth == "other") %>% select(n),
                       ");
              <br>- Black (",
                       tnc_p1_samplesize %>% filter(race_eth == "black") %>% select(n),
                       ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning 
              Analysis of My Daily Travel data."),
              filename = "tnc_p1",
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Usage and cost
################################################################################


# # Helper for month conversions (from weeks)
# converter <- 
#   (365/7) / # Number of weeks in a year
#   12 # Divided by the number of months in a year

# Placeholder for converter (if not going from weeks to something else)
converter <- 1

overall_usage <-
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  summarize(tnc_use = converter*weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n())

overall_cost <-
  tnc_wide %>%
  filter(tnc_cost > 0) %>% 
  summarize(tnc_cost = weighted.mean(tnc_cost,weight, na.rm = TRUE),
            n = n())

################################################################################
# PLOT OF HOME JURISDICTION USAGE
################################################################################

# Usage by home county
home_usage <- 
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  # Limit to residents of the seven counties for presentation. Grundy and DeKalb
  # are included in regional averages.
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county_chi) %>%
  summarize(tnc_use = converter*weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n()) %>% 
  rbind(overall_usage %>% mutate(home_county_chi = "CMAP region"))

# Generate output chart for age
tnc_p2 <-
  # Get data
  home_usage %>% 
  
  # Create ggplot object
  ggplot(aes(x = tnc_use, y = reorder(home_county_chi,tnc_use), fill = home_county_chi)) +
  geom_col() +
  geom_label(aes(label = scales::label_number(accuracy = 0.01)(tnc_use)),
             hjust = -0.02,
             label.size = 0,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,legend.position = "none",
             xlab = "TNC usage per week by home jurisdiction") +
  cmap_fill_highlight(home_usage$home_county_chi,"CMAP region") +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,.85))

finalize_plot(tnc_p2,
              "Usage of Transportation Network Companies (TNCs) was greatest by 
              residents of Chicago and suburban Cook County.",
              paste0("Note: These figures are based on survey responses and not 
              trip diaries. The CMAP region average includes usage by residents 
              of the seven county region (Cook, DuPage, 
              Kane, Kendall, Lake, McHenry, and Will), as well as residents of 
              Grundy and DeKalb. Excludes travelers age 18 and younger, who were 
              not asked about TNC usage. 
              <br><br>
              Sample size:
              <br>- Chicago (",
                     home_usage %>% filter(home_county_chi == "Chicago") %>% select(n),
                     ");
              <br>- Suburban Cook (",
                     home_usage %>% filter(home_county_chi == "Suburban Cook") %>% select(n),
                     ");
              <br>- DuPage (",
                     home_usage %>% filter(home_county_chi == "DuPage") %>% select(n),
                     ");
              <br>- Lake (",
                     home_usage %>% filter(home_county_chi == "Lake") %>% select(n),
                     ");
              <br>- Kane (",
                     home_usage %>% filter(home_county_chi == "Kane") %>% select(n),
                     ");
              <br>- Will (",
                     home_usage %>% filter(home_county_chi == "Will") %>% select(n),
                     "); 
              <br>- McHenry (",
                     home_usage %>% filter(home_county_chi == "McHenry") %>% select(n),
                     ");
              <br>- Kendall (",
                     home_usage %>% filter(home_county_chi == "Kendall") %>% select(n),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "tnc_p2",
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# PLOT OF AGE CHARACTERISTICS | USAGE AND COST
################################################################################

## Look at usage by age
age_usage <-
  tnc_wide %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1)),
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_use = converter*weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n())

# Cost by age
age_cost <- 
  tnc_wide %>%
  filter(tnc_cost > 0,
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_cost = weighted.mean(tnc_cost,weight, na.rm = TRUE),
            n = n())

# Combine age data for cost and usage
age_cost_and_usage <-
  left_join(age_usage %>% select(-n),
            age_cost %>% select(-n),
            by = "age_bin") %>% 
  rbind(
    left_join(overall_cost %>% select(-n) %>% mutate(age_bin = "CMAP region"),
              overall_usage %>% select(-n) %>%  mutate(age_bin = "CMAP region"),
              by = "age_bin")) %>% 
  pivot_longer(cols = c("tnc_use","tnc_cost")) %>% 
  # Adjust factor ordering for CMAP region
  mutate(age_bin = relevel(age_bin,"CMAP region"))

# Generate output chart for age
tnc_p3 <-
  # Get data
  age_cost_and_usage %>% 
  # Reformat
  mutate(name = recode(factor(name,levels = c("tnc_use","tnc_cost")),
                       "tnc_cost" = "Average cost per trip",
                       "tnc_use" = "Average weekly trips")) %>% 
  # Create values to allow for axis adjustments on the faceted graph
  mutate(blank = case_when(
    name == "Average cost per trip" ~ 20.5,
    TRUE ~ 1.05
  )) %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = factor(age_bin,levels = rev(levels(age_bin))), fill = age_bin)) +
  geom_col() +
  geom_label(aes(label = ifelse(name == "Average weekly trips",
                                scales::label_number(accuracy = 0.01)(value),
                                scales::label_dollar(accuracy = 1)(value))),
             hjust = -.02,
             label.size = 0,
             fill = "white") +
  geom_blank(aes(x = blank)) +
  
  # Add faceting
  facet_wrap(~name, scales = "free_x") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",hline = 0,legend.position = "none",
             panel.spacing.x = unit(30,"bigpts"),
             xlab = "TNC usage characteristics by age",
             strip.text = element_text(family = "Whitney Semibold",
                                       hjust = 0.5,vjust = 1)) +
  cmap_fill_highlight(age_cost_and_usage$age_bin,"CMAP region")

tnc_p3_samplesize <-
  age_usage %>% select(age_bin,usage = n) %>% 
  left_join(age_cost %>% select(age_bin,cost = n), by = "age_bin") %>% 
  ungroup()

finalize_plot(tnc_p3,
              "Usage of Transportation Network Companies (TNCs) decreased by age, 
              while the average cost per trip increased with age.",
              paste0("Note: These figures are based on survey responses and are 
              not derived from trip diaries. Excludes travelers age 18 and younger, 
              as they were not asked about TNC usage.
              <br><br>
              Sample size (Usage/Cost): 
              <br>- 19-29 (",
              paste(tnc_p3_samplesize %>% filter(age_bin == "19 to 29") %>% select(usage),
                    tnc_p3_samplesize %>% filter(age_bin == "19 to 29") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 30-39 (",
              paste(tnc_p3_samplesize %>% filter(age_bin == "30 to 39") %>% select(usage),
                    tnc_p3_samplesize %>% filter(age_bin == "30 to 39") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 40-49 (",
              paste(tnc_p3_samplesize %>% filter(age_bin == "40 to 49") %>% select(usage),
                    tnc_p3_samplesize %>% filter(age_bin == "40 to 49") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 50-59 (",
              paste(tnc_p3_samplesize %>% filter(age_bin == "50 to 59") %>% select(usage),
                    tnc_p3_samplesize %>% filter(age_bin == "50 to 59") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 60+ (",
              paste(tnc_p3_samplesize %>% filter(age_bin == "60 and above") %>% select(usage),
                    tnc_p3_samplesize %>% filter(age_bin == "60 and above") %>% select(cost),
                    sep = "/"),
                    ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "tnc_p3",
              # width = 8,
              # height = 4.5,
              # sidebar_width = 2.3,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chicago TNC trips
################################################################################

source("R/mdt_dates.R")

# Load trip totals sourced from the following url:
# https://data.cityofchicago.org/d/m6dm-c72p/visualization .The City of
# Chicago's visualization tool allows total trips to be grouped by day. I
# filtered data to the desired time period and copied and pasted the totals into
# the CSV included in `source`. Note that these totals are from midnight to
# midnight, and not from 3am to 3am, which could skew ridership down slightly
# (since it does not include early morning trips on Saturdays before 3am, and
# instead includes early morning trips on Mondays before 3am).
tnc_daily_totals <- read.csv("source/TNP_daily_totals.csv") %>% 
  mutate(date = lubridate::dmy(date,tz = "America/Chicago"))

tnc_daily_totals %>% 
  # Identify the day of the week of the trip using the `wday` function from the
  # lubridate package.
  mutate(wday = wday(date)) %>%
  # Keep out trips that are either Saturday (7) or Sunday (1)
  filter(!(wday %in% c(1,7))) %>%
  # Remove holidays (individually - note that %within% does not currently work
  # on a list of time intervals)
  filter(!(date %within% int_shift(mlk,duration(hours = -3)) |
             date %within% int_shift(pres,duration(hours = -3)) |
             date %within% int_shift(columbus,duration(hours = -3)) |
             date %within% int_shift(vets,duration(hours = -3)) |
             date %within% int_shift(xgiving,duration(hours = -3)) |
             date %within% int_shift(xmas,duration(hours = -3)) |
             date %within% int_shift(springb,duration(hours = -3)))) %>% # 1285608 records
  summarize(average = mean(rides))

# The average of daily TNC rides in Chicago from November to May during the MDT
# survey period was ~300,000 trips for non-holiday weekdays (and excluding the
# weeks of holidays documented in `mdt_dates.R`.)


# # Load TNC Trips during the My Daily Travel period (September 4, 2018 to May 9,
# # 2019 - the dates begin and end at 3am Central Time). The City of Chicago only
# # began releasing TNP data as of November 1, 2019, so the beginning date is
# # coded as November 1st. NOTE: This will take a long time to download, even
# # using these restricted dates. It will be archived because of long data
# # processing times.
# 
# mdt_socrata_dates <-
#   tibble(date =
#            c(seq.Date(from = as.Date("2018-11-01"),
#                         to = as.Date("2019-05-09"),
#                         by = "1 day"))) %>%
#   # Identify the day of the week of the trip using the `wday` function from the
#   # lubridate package.
#   mutate(wday = wday(date)) %>%
#   # Keep out trips that are either Saturday (7) or Sunday (1)
#   filter(!(wday %in% c(1,7))) %>%
#   # Remove holidays (individually - note that %within% does not currently work
#   # on a list of time intervals)
#   filter(!(date %within% int_shift(mlk,duration(hours = -3)) |
#              date %within% int_shift(pres,duration(hours = -3)) |
#              date %within% int_shift(columbus,duration(hours = -3)) |
#              date %within% int_shift(vets,duration(hours = -3)) |
#              date %within% int_shift(xgiving,duration(hours = -3)) |
#              date %within% int_shift(xmas,duration(hours = -3)) |
#              date %within% int_shift(springb,duration(hours = -3)))) %>%
#   mutate(next_day = date + 1) %>%
#   mutate(socrata = paste0("'",date,"T03:00:00' and '",next_day,"T02:59:59'"))
# 
# actual_tnc_trips_1_to_40 <-
#   map_dfr(mdt_socrata_dates$socrata[1:40],
#           ~read.socrata(
#             paste0("https://data.cityofchicago.org/resource/m6dm-c72p.json?$select= trip_start_timestamp, pickup_community_area,dropoff_community_area&$where= trip_start_timestamp between ",
#                    .)))
# 
# actual_tnc_trips_41_to_80 <-
#   map_dfr(mdt_socrata_dates$socrata[41:80],
#           ~read.socrata(
#             paste0("https://data.cityofchicago.org/resource/m6dm-c72p.json?$select= trip_start_timestamp, pickup_community_area,dropoff_community_area&$where= trip_start_timestamp between ",
#                    .)))
# 
# actual_tnc_trips_81_to_116 <-
#   map_dfr(mdt_socrata_dates$socrata[81:116],
#           ~read.socrata(
#             paste0("https://data.cityofchicago.org/resource/m6dm-c72p.json?$select= trip_start_timestamp, pickup_community_area,dropoff_community_area&$where= trip_start_timestamp between ",
#                    .)))
# 
# write_csv(actual_tnc_trips_1_to_40,"outputs/mdt_tnc_1.csv")
# write_csv(actual_tnc_trips_41_to_80,"outputs/mdt_tnc_2.csv")
# write_csv(actual_tnc_trips_81_to_116,"outputs/mdt_tnc_3.csv")
