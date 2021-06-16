# This script produces analyses and charts on the trip purposes of trips made by
# specific modes, e.g., ridesharing.

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

## Create totals for trips by purpose category (within universe of rideshare
## trips)

### Calculate proportions for TT
all_tnc_tpurp_c_tt <-
  pct_calculator(mode_analysis_base_tt,
                 subset = "taxi",
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "weight",
                 survey = "tt")

### Calculate proportions for MDT
all_tnc_tpurp_c_mdt <-
  pct_calculator(mode_analysis_base_mdt %>% mutate(mode = recode_factor(mode,
                                                          "rideshare" = "TNC",
                                                          "shared rideshare" = "TNC")),
                 subset = c("TNC","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "weight",
                 survey = "mdt")

### Join MDT and TT
total_tnc_tpurp_c <-
  rbind(all_tnc_tpurp_c_tt,
        all_tnc_tpurp_c_mdt) %>%
  # Add mode flag to differentiate TT (taxi) from MDT (taxi + TNCs)
  mutate(mode = case_when(
    survey == "mdt" ~ "tnc (all)",
    TRUE ~ "taxi"))


### Calculate proportions for subcategories for rideshare in MDT (using the
### "second_breakdown" argument of pct_calculator)
detailed_tnc_tpurp_c_mdt <-
  pct_calculator(mode_analysis_base_mdt %>%
                   mutate(mode = recode_factor(mode,
                                               "rideshare" = "TNC",
                                               "shared rideshare" = "TNC")) %>% 
                   # Collapse low-percentage modes into "all other" for chart
                   mutate(tpurp_c =
                            fct_collapse(tpurp_c,
                                         "all other" = c("health","recreation/fitness",
                                                         "school","transport",
                                                         "transfer","other"))),
                 subset = c("TNC","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 weight = "weight",
                 survey = "mdt")

all_tnc_tpurp_c <-
  rbind(total_tnc_tpurp_c,
        detailed_tnc_tpurp_c_mdt)

################################################################################
# Chart of total TNC/taxi trips, MDT vs. TT
################################################################################

# Note that the total figures appear to be lower than expected based on actuals
# from the City of Chicago's TNP data, although are roughly consistent with the
# proportions of those data. Since the TNP data does not cover the region and
# only begins in 2013 for taxis, we still want to use MDT data to understand
# regional trends.Thus, we use these figures to calculate the percentage
# changes, rather than absolute figures.

# Extract 2008 total for taxis as a baseline
taxi_total_tt <- unique(all_tnc_tpurp_c_tt$total)

# Create chart. 
mode_analysis_p1 <-
  # Get data
  all_tnc_tpurp_c %>%
  # Exclude the 2008 taxi levels and the 2019 totals
  filter(survey == "mdt",
         mode != "tnc (all)") %>%
  # Group into mode and survey categories to enable summaries (instead of by
  # trip purpose category)
  group_by(survey,mode) %>%
  summarize(total = sum(breakdown_total)/taxi_total_tt) %>%
  
  # Add factor levels and format for chart ordering
  mutate(mode = recode_factor(factor(mode),
                              "taxi" = "Taxi",
                              "TNC" = "TNC")) %>%
  
  # Create ggplot object
  ggplot(aes(x = mode, y = total)) +
  geom_col(aes(fill = mode)) +
  # Add labels above the bars
  geom_label(aes(label = scales::label_percent(accuracy = 1)(total)),
             hjust = 0.5,
             vjust = 0,
             label.size = 0,
             fill = "white",
             show.legend = FALSE) +
  
  # Adjust axis
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0,3.2)) +
  
  # Add hline for 100%
  geom_hline(yintercept = 1,color = "dark gray", size = 2) +
  annotate(
    geom = "text",
    x = 1,y = 1.05,
    label = "2008 taxi ridership",
    vjust = 0
  ) +
  
  # Add CMAP style
  theme_cmap(hline = 0,show.legend = FALSE,
             xlab = "Taxi and TNC ridership in 2019 as a share of 2008 taxi ridership") +
  # Manually include CMAP colors
  scale_fill_discrete(type = c("#3f0030","#36d8ca"))

mode_analysis_p1_samplesize <-
  all_tnc_tpurp_c %>% 
  select(mode,survey,n = total_n) %>% 
  ungroup() %>% 
  distinct()

# Export finalized graphic
finalize_plot(mode_analysis_p1,
              "While taxi ridership fell significantly between 2008 and 2019, 
              Transportation Network Company (TNC) trips more than made up the
              difference.",
              paste0("Note: Includes trips by residents age 5 and older of the CMAP seven 
              county region (Cook, DuPage, Kane, Kendall, Lake, McHenry, and 
              Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              'TNC' includes trips reported as either 'rideshare'or 'shared 
              rideshare.' 
              <br><br> 
              The reported regional totals for both taxi and TNC trips in My
              Daily Travel are less than 
              those captured in the City of Chicago's data on TNC and taxi trips, 
              even though the My Daily Travel survey covers a larger universe 
              of trips (including trips in the region that do not start or end 
              in Chicago). This may be due to the exclusion of non-resident 
              trips and/or other survey design factors.
              <br><br>
              Sample size:
              <br>- Taxi, 2008 (",
                     mode_analysis_p1_samplesize %>% filter(mode == "taxi" & survey == "tt") %>% select(n),
                     ");
              <br>- Taxi, 2019 (",
                     mode_analysis_p1_samplesize %>% filter(mode == "taxi" & survey == "mdt") %>% select(n),
                     ");
              <br>- TNC, 2019 (",
                     mode_analysis_p1_samplesize %>% filter(mode == "TNC") %>% select(n),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data."),
              filename = "mode_analysis_p1",
              sidebar_width = 3.25,
              mode = c("png","pdf"),
              # height = 6.3,
              # width = 11.3,
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
