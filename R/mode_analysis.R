# This script produces analyses and charts on the trip purposes of trips made by
# specific modes, e.g., ridesharing. It is referenced in Policy Brief #4.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


# Create base dataset for mode analyses

mode_analysis_base_mdt <-
  mdt %>%                             # 125463 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets (those from age 5 and above), school enrollment
  # (starting from 9th grade, since the prior bucket includes kindergarten and
  # that could include 4-year-olds), or manual location identification of school
  # trips.
  filter(age >= 5 |                   # 125459 records
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8)) |
           sampno %in% c(70038312,
                         70051607)) %>%
  # Exclude beginning trips
  filter(mode_c != "beginning") %>%  # 97374 records
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in TT (great circle vs. actual travel distance).
  # We chose to do this since the published graphics do not involve any
  # comparison between TT and MDT. However, if we instead filter out those trips
  # that have a nonzero haversine distance from MDT, the results are similar.
  filter(distance_pg > 0) %>%        # 97316 records
  # Exclude trips with no trip purpose
  filter(tpurp_c != "missing") %>%   # 97245 records
  # Add flag for under 18 vs. 18 and over (combining all age variables)
  mutate(under18 = ifelse(age >= 18 | 
                            (age < 0 & aage %in% c(5,6,7)) |
                            (age < 0 & age18 == 1),
                          "18 and over", "Under 18")) %>%
  ungroup()

mode_analysis_base_tt <-
  tt %>%                             # 139769 records
  # Keep only records for travelers >= 5 or who we can identify as being >= 5
  # based on age buckets (greater than 16) or school enrollment (from 9th grade
  # up, for similar reasons as in MDT - kindergarten could include 4-year-olds).
  # Note that 99 is DK/RF for AGE.
  filter((AGE >= 5 & AGE < 99) |                  # 132680 records
           (AGE == 99 & SCHOL %in% c(4,5,6,7,8)) |
           (AGE == 99 & AGEB == 2)) %>%
  # Exclude the first record of the day - this is the beginning record, and does
  # not represent a trip.
  filter(PLANO != 1) %>%            # 105568 records
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in MDT (great circle vs. actual travel distance).
  filter(DIST > 0) %>%              # 100573 records
  # Exclude trips with a missing trip purpose
  filter(tpurp_c != "missing") %>%  # 100573 records
  # Add flag for under 18 vs. 18 and over. None of the school enrollment or age
  # buckets are precise enough to code either way, so those without a defined
  # age are coded as NA.
  mutate(under18 = ifelse(AGE >= 18 & AGE < 99, "18 and over",
                          ifelse(AGE != 99,"Under 18",NA))) %>%
  ungroup()

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# RIDESHARE VS. SHARED RIDESHARE VS. TAXI
################################################################################

# Note: TT did not include any questions about ride-sharing or TNCs, since the
# survey predates the widespread adoption of those services. We thus use taxis
# as the closest proxy for comparison.

# Extract 2008 total for taxis as a baseline
all_taxi_tt <- 
  mode_analysis_base_tt %>% 
  filter(mode == "taxi") %>% 
  summarize(n = n(),
            n_wt = sum(weight))

### Calculate ridership for MDT
all_tnc_taxi_mdt <-
  mode_analysis_base_mdt %>% 
  mutate(mode = recode_factor(mode,
                              "rideshare" = "TNC",
                              "shared rideshare" = "TNC")) %>% 
  filter(mode %in% c("TNC","taxi")) %>% 
  group_by(mode) %>% 
  summarize(n = n(),
            n_wt = sum(weight))

################################################################################
# Chart of total TNC/taxi trips, MDT vs. TT
################################################################################

# Note that the total figures appear to be lower than expected based on actuals
# from the City of Chicago's TNP data, although are roughly consistent with the
# proportions of those data. Since the TNP data does not cover the region and
# only begins in 2013 for taxis, we still want to use MDT data to understand
# regional trends.Thus, we use these figures to calculate the percentage
# changes, rather than absolute figures.

# Create chart. 
figure4_1 <-
  # Get data
  all_tnc_taxi_mdt %>%
  # Summarize as a share of 2008 taxi ridership
  mutate(total = n_wt/all_taxi_tt$n_wt) %>%
  
  # Add factor levels and format for chart ordering
  mutate(mode = recode_factor(factor(mode),
                              "taxi" = "Taxi",
                              "TNC" = "TNC")) %>%
  
  # Create ggplot object
  ggplot(aes(x = mode, y = total)) +
  
  geom_hline(yintercept = c(2,3),size = 0.25) +
  geom_col(aes(fill = mode),width = 0.85) +
  # Add labels above the bars
  geom_label(aes(label = scales::label_percent(accuracy = 1)(total)),
             hjust = 0.5,
             vjust = 0,
             label.size = 0,
             fill = "white",
             label.r = grid::unit(0,"lines"),
             show.legend = FALSE) +
  
  # Adjust axis
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0,3.2)) +
  
  # Add hline for 100%
  geom_hline(yintercept = 1, size = 1,linetype = "dashed") +
  annotate(
    geom = "text",
    x = 1,y = 1.05,
    label = "2008 taxi ridership",
    vjust = 0
  ) +
  
  # Add CMAP style
  theme_cmap(hline = 0,show.legend = FALSE,
             gridlines = "none",
             ylab = "2019 ridership as a share of 2008 taxi ridership") +
  # Manually include CMAP colors
  scale_fill_discrete(type = c("#D8BA37","#38B2D8"))

figure4_1_samplesize <-
  all_taxi_tt %>% 
  mutate(survey = "tt",
         mode = "taxi") %>% 
  rbind(all_tnc_taxi_mdt %>% mutate(survey = "mdt")) %>% 
  select(mode,survey,n)

# Export finalized graphic
finalize_plot(figure4_1,
              "Taxi ridership fell significantly between 2008 and 2019, but
              transportation network company (TNC) trips more than made up the
              difference.",
              paste0(
                "Note: Includes \"rideshare\" and \"shared rideshare\" trips by residents 
                age 5 and older of the CMAP seven-county region, Grundy, and DeKalb. 
                The reported regional totals for both taxi and TNC trips in My
              Daily Travel are less than 
              those captured in the City of Chicago's data on TNC and taxi trips. 
              This may be due to the exclusion of non-resident 
              trips and/or other survey design factors.
              <br><br>
              Sample size:
              <br>- Taxi, 2008 (",
                format(figure4_1_samplesize %>% 
                         filter(mode == "taxi" & survey == "tt") %>% 
                         select(n) %>% as.numeric(),
                       big.mark = ","),
                ");
              <br>- Taxi, 2019 (",
                format(figure4_1_samplesize %>% 
                         filter(mode == "taxi" & survey == "mdt") %>% 
                         select(n) %>%  as.numeric(),
                       big.mark = ","),
                ");
              <br>- TNC, 2019 (",
                format(figure4_1_samplesize %>% filter(mode == "TNC") %>% 
                         select(n) %>%  as.numeric(),
                       big.mark = ","),
                ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data."),
              filename = "figure4_1",
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Backup - breakdown of trips by origin and destination
################################################################################

pct_calculator(
  mode_analysis_base_mdt %>% 
    filter(mode %in% c("rideshare","shared rideshare")) %>% 
    mutate(chicago_tnc = case_when(
      # Trips that are totally outside Chicago (note there are some trips that
      # left the region that are captured as NAs)
      (county_chi_name != "Chicago" | is.na(county_chi_name)) & 
        (county_chi_name_lag != "Chicago" | is.na(county_chi_name_lag)) ~ 0,
      # Trips that start or end (but not both) in Chicago
      xor(county_chi_name == "Chicago",county_chi_name_lag == "Chicago") ~ 1,
      # Trips that are entirely within Chicago
      county_chi_name == "Chicago" & county_chi_name_lag == "Chicago" ~ 2)),
  subset = c("rideshare","shared rideshare"),
  subset_of = "mode",
  breakdown_by = "chicago_tnc",
  # second_breakdown = "mode",
  weight = "weight")

# Suburb-to-suburb trips represent 12.0 percent of all TNC trips, with a higher
# share of rideshare (14.0%) than shared rideshare (8.1%). Chicago to suburb
# trips represent 9.0% of all trips.


################################################################################
#
# BIKE TRIPS
################################################################################

### Note: Bike trip totals are not easily comparable between MDT and TT. In TT,
### trip diaries were conducted in a geographically phased approach, with the
### north side of Chicago conducted entirely in the winter months. Since this
### part of the region has the highest rates of bike ridership, this sequencing
### likely skewed the overall amount of bike trips captured in the survey, as
### well as potentially skewed the purposes of those trips (e.g., by decreasing
### the share of recreational bike trips). In contrast, there was no such
### geographic phasing in the data collection for MDT.

### Calculate proportions for subcategories for biking in MDT
detailed_bike_tpurp_c_mdt <-
  pct_calculator(mode_analysis_base_mdt,
                 subset = "bike",
                 subset_of = "mode_c",
                 breakdown_by = "chain_c",
                 # # Alternative - breakdown by trip purpose (reveals connection
                 # # to other modes)
                 # breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 # # Breakdown to enable geographic considerations
                 # third_breakdown = "geog",
                 weight = "weight",
                 survey = "mdt")

detailed_bike_tpurp_c_mdt
