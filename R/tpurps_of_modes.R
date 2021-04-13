# This script produces analyses and charts on the trip purposes of trips made by
# specific modes, e.g., ridesharing.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(ggplot2)
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

mdt_base_2 <-
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
  # Exclude trips with no travel distance
  filter(distance_pg > 0) %>%        # 97316 records
  # Exclude trips with no trip purpose
  filter(tpurp_c != "missing") %>%   # 97245 records
  # Add flag for under 18 vs. 18 and over (combining all age variables)
  mutate(under18 = ifelse(age >= 18 | 
                            (age < 0 & aage %in% c(5,6,7)) |
                            (age < 0 & age18 == 1),
                          "18 and over", "Under 18")) %>%
  ungroup()

tt_base_2 <-
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
  pct_calculator(mdt_base_2,subset = "bike",
                 subset_of = "mode_c",
                 breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 weight = "wtperfin",
                 survey = "mdt")

################################################################################
# Backup - Chart of trip purposes, bike share vs. personal bike
################################################################################

tpurps_of_modes_p1 <-
  # Get data
  detailed_bike_tpurp_c_mdt %>%

  # Create ggplot object
  ggplot(aes(y = reorder(tpurp_c,desc(-pct)), x = pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = mode),
             fill = "white",
             position = position_dodge2(reverse = T, width = 0.9),
             hjust = 0,
             label.size = 0,
             label.padding = unit(1,"bigpts")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(), limits = c(0,.38)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0) +
  cmap_fill_discrete(palette = "friday")

# Export finalized graphic
finalize_plot(tpurps_of_modes_p1,
              "Trip purposes of bike trips in 2019, personal vs. bike share",
              "Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              # width = 11.3,
              # height = 6.3,
              filename = "tpurps_of_modes_p1",
              # mode = "png",
              overwrite = T)

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
  pct_calculator(tt_base_2,
                 subset = "taxi",
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "weight",
                 survey = "tt")

### Calculate proportions for MDT
all_tnc_tpurp_c_mdt <-
  pct_calculator(mdt_base_2,
                 subset = c("rideshare","shared rideshare","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 weight = "wtperfin",
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
  pct_calculator(mdt_base_2 %>%
                   # Collapse low-percentage modes into "all other" for chart
                   mutate(tpurp_c =
                            fct_collapse(tpurp_c,
                                         "all other" = c("health","recreation/fitness",
                                                         "school","transport",
                                                         "transfer","other"))),
                 subset = c("rideshare","shared rideshare","taxi"),
                 subset_of = "mode",
                 breakdown_by = "tpurp_c",
                 second_breakdown = "mode",
                 weight = "wtperfin",
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
taxi_2008_total <- unique(all_tnc_tpurp_c_tt$total)

# Create chart. 
tpurps_of_modes_p2 <-
  # Get data
  all_tnc_tpurp_c %>%
  # Exclude the 2008 taxi levels and the 2019 totals
  filter(survey == "mdt",
         mode != "tnc (all)") %>%
  # Group into mode and survey categories to enable summaries (instead of by
  # trip purpose category)
  group_by(survey,mode) %>%
  summarize(total = sum(breakdown_total)/taxi_2008_total) %>%
  
  # Add factor levels and format for chart ordering
  mutate(mode = recode_factor(factor(mode, levels = c("taxi",
                                                      "rideshare",
                                                      "shared rideshare")),
                              "taxi" = "Taxi",
                              "rideshare" = "TNC (regular)",
                              "shared rideshare" = "TNC (shared)")) %>%
  
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
                     limits = c(0,2)) +
  
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
  scale_fill_discrete(type = c("#3f0030","#36d8ca","#006b8c"))

# Export finalized graphic
finalize_plot(tpurps_of_modes_p2,
              "While taxi ridership has fallen significantly since 2008, Transportation Network Company (TNC) trips have more than made up the difference.",
              "Note: Includes only trips by regional residents and excludes 
              weekend trips. The reported regional totals for both taxi and TNC 
              trips in My Daily Travel are similar to but slightly less than 
              those captured in the City of Chicago's data on TNC and taxi 
              trips, which may be due to the exclusion of non-resident and 
              weekend trips and/or other survey design factors.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel and Travel Tracker data.",
              filename = "tpurps_of_modes_p2",
              mode = "png",
              # height = 6.3,
              # width = 11.3,
              overwrite = T)

################################################################################
# Backup - Chart of trip purposes for TNC/taxi trips, MDT
################################################################################

tpurps_of_modes_p3 <-
  # Get data
  all_tnc_tpurp_c %>%
  # Keep only subcategories and MDT responses
  filter(survey == "mdt",
         mode != "tnc (all)") %>%
  # Add factor levels and format for chart ordering
  mutate(mode = recode_factor(factor(mode, levels = c("taxi",
                                                      "rideshare",
                                                      "shared rideshare")),
                              "taxi" = "Taxi",
                              "rideshare" = "TNC (regular)",
                              "shared rideshare" = "TNC (shared)"),
         tpurp_c = recode_factor(factor(tpurp_c,
                                        c("all other","dining","community",
                                          "shopping/errands","work","home")),
                                 "all other" = "All other",
                                 "dining" = "Dining",
                                 "community" = "Community",
                                 "shopping/errands" = "Shopping/errands",
                                 "work" = "Work",
                                 "home" = "Home")
         ) %>%
  
  # Create ggplot object
  ggplot(aes(y = tpurp_c, x = pct, fill = mode)) +
  geom_col(position = position_dodge2(reverse = TRUE)) +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(pct),
                 group = mode),
             position = position_dodge2(reverse = T,
                                        width = 0.9),
             hjust = 0,
             label.size = 0,
             label.padding = unit(3,"bigpts"),
             fill = "white") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0, .42)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0) +
  scale_fill_discrete(type = c("#3f0030","#36d8ca","#006b8c"))

# Export finalized graphic
finalize_plot(tpurps_of_modes_p3,
              "Trip purposes of TNC and taxi trips, 2019.",
              "Note: \"All other\" includes categories with less than 5% of the
              overall mode share for taxis and TNCs. This includes healthcare,
              school, transport, transfers, and other.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              # width = 11.3,
              # height = 6.3,
              filename = "tpurps_of_modes_p4",
              # mode = "png",
              overwrite = T)

################################################################################
# Backup - breakdown of trips by origin and destination
################################################################################

pct_calculator(
  mdt_base_2 %>% 
    mutate(chicago_tnc = case_when(
      county_chi_name == "Chicago" | county_chi_name_lag == "Chicago" ~ 1,
      TRUE ~ 0)),
  subset = c("rideshare","shared rideshare"),
  subset_of = "mode",
  breakdown_by = "chicago_tnc",
  # second_breakdown = "mode",
  weight = "wtperfin")
  

################################################################################
# ARCHIVE
#
################################################################################
# 
# ################################################################################
# #
# # CARPOOL VS PASSENGER
# ################################################################################
# 
# ### Calculate proportions for subcategories for driver/passenger in MDT and TT
# detailed_passenger_totals_under18_mdt <-
#   pct_calculator(mdt_base_2,
#                  subset = "passenger",
#                  subset_of = "mode_c",
#                  breakdown_by = "mode",
#                  second_breakdown = "under18",
#                  weight = "wtperfin",
#                  survey = "mdt")
# 
# passenger_totals_under18_tt <-
#   pct_calculator(tt_base_2,
#                  subset = "passenger",
#                  subset_of = "mode_c",
#                  breakdown_by = "mode",
#                  second_breakdown = "under18",
#                  weight = "weight",
#                  survey = "tt") %>%
#   mutate(mode = "Passenger (all)")
# 
# all_passenger_under18 <-
#   rbind(passenger_totals_under18_tt,
#         detailed_passenger_totals_under18_mdt)
# 
# ################################################################################
# # Chart of total passenger trips, MDT vs TT
# ################################################################################
# 
# tpurps_of_modes_p1 <-
#   # Get data
#   all_passenger_under18 %>%
#   # Filter out NAs
#   filter(!is.na(under18)) %>% 
#   # Group by age flag and survey flag
#   group_by(under18,survey) %>%
#   # Calculate totals by the chosen groups
#   mutate(total = sum(breakdown_total)) %>%
#   ungroup() %>%
#   # Add factor levels for chart ordering
#   mutate(mode = factor(mode, levels = c("Passenger (all)",
#                                         "personal auto (passenger)",
#                                         "carpool")),
#          survey = recode_factor(factor(survey),
#                                 "tt" = "Travel Tracker ('08)",
#                                 "mdt" = "My Daily Travel ('19)")) %>%
#   
#   # Create ggplot object
#   ggplot(aes(x = survey, y = breakdown_total, fill = mode)) +
#   geom_col(position = position_stack(reverse = T)) +
#   geom_label(aes(label = scales::label_comma(accuracy = 1)(total),
#                  y = total),
#              fill = "white",
#              vjust = 0,
#              label.size = 0) +
#   
#   # Facet formatting
#   facet_wrap(~under18, ncol = 1) +
#   
#   # Add CMAP style
#   theme_cmap() +
#   cmap_fill_discrete(palette = "friday",reverse = F) +
#   
#   # Adjust axis
#   scale_y_continuous(labels = scales::label_comma(scale = 1),
#                      limits = c(0,3200000))
# 
# finalize_plot(tpurps_of_modes_p1,
#               "Change in daily automobile passenger trips, 2008 vs. 2019, by age.",
#               "Note: Travel Tracker did not have a 'Carpool' category, and so
#               'Passenger (all)' includes both types of trips.
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               filename = "tpurps_of_modes_p1",
#               # height = 6.3,
#               # width = 11.3,
#               # mode = "png",
#               overwrite = T
# )
# 
# 
# ################################################################################
# # ARCHIVE - old bike analyses (unusable due to data collection issues)
# ################################################################################
# 
# The data collection for bike trips in TT was phased, and high bike ridership
# parts of the region were sampled in the winter.
#
# # Create totals for trips by purpose category (within universe of bike trips)
# 
# ### Calculate proportions for TT
# all_bike_tpurp_c_tt <-
#   pct_calculator(tt_base_2,subset = "bike",
#                  subset_of = "mode_c",
#                  breakdown_by = "tpurp_c",
#                  weight = "weight",
#                  survey = "tt")
#
# ### Calculate proportions for MDT
# all_bike_tpurp_c_mdt <-
#   pct_calculator(mdt_base_2,subset = "bike",
#                  subset_of = "mode_c",
#                  breakdown_by = "tpurp_c",
#                  weight = "wtperfin",
#                  survey = "mdt")
#
# ### Join MDT and TT
# total_bike_tpurp_c <-
#   rbind(all_bike_tpurp_c_tt,
#         all_bike_tpurp_c_mdt) %>%
#   mutate(mode = "Bike (all)")
#
# all_bike_tpurp_c <-
#   rbind(total_bike_tpurp_c,
#         detailed_bike_tpurp_c_mdt)
# 
# # Chart of bike trips, MDT vs TT
# tpurps_of_modes_p4 <-
#   all_bike_tpurp_c %>%
#   filter(!(survey == "mdt" & mode == "Bike (all)")) %>%
#   group_by(survey,mode) %>%
#   summarize(total = sum(tpurp_c_total)) %>%
#   mutate(survey = factor(survey,levels = c("tt","mdt")),
#          mode = factor(mode, levels = c("Bike (all)","bike share","personal bike"))) %>%
#   mutate(survey = recode_factor(survey,
#                                 "tt" = "Travel Tracker ('08)",
#                                 "mdt" = "My Daily Travel ('19)")) %>%
#   ggplot(aes(x = survey, y = total, fill = mode)) +
#   geom_col() +
#   theme_cmap() +
#   cmap_fill_discrete(palette = "governance",reverse = TRUE) +
#   scale_y_continuous(labels = scales::label_comma(scale = 1))
#
# finalize_plot(tpurps_of_modes_p4,
#               "Change in daily bike trips, 2008 vs. 2019.",
#               "Note: Travel Tracker did not have a 'bike share' category, and so
#               'Bike (all)' includes both types of trips.
#               <br><br>
#               Source: CMAP analysis of MDT and TT data.",
#               filename = "tpurps_of_modes_p4",
#               height = 6.3,
#               width = 11.3,
#               mode = "png",
#               overwrite = T)
#
#
# ### Bike mode share of all travel
#
#
# ### Calculate total number of trips
# all_purp_mdt <-
#   mdt_base_2 %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(wtperfin))
#
# all_purp_tt <-
#   tt_base_2 %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_total = sum(weight))
#
#
# # Create totals for trips by purpose category, specifically for bikes (reuse logic from above)
# bike_purp_mdt <-
#   all_bike_mdt %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_count = sum(wtperfin)) %>%
#   inner_join(all_purp_mdt, by = "tpurp_c") %>%
#   mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
#          survey = "mdt") %>% # add identifier
#   arrange(tpurp_c_pct) # sort by mode share
#
# bike_purp_tt <-
#   all_bike_tt %>%
#   group_by(tpurp_c) %>%
#   summarize(tpurp_c_count = sum(weight)) %>%
#   inner_join(all_purp_tt, by = "tpurp_c") %>%
#   mutate(tpurp_c_pct = tpurp_c_count / tpurp_c_total, # Calculate share of bike trips out of all
#          survey = "tt") %>% # add identifier
#   arrange(tpurp_c_pct) # sort by mode share
#
# # Combine data from MDT and TT
# bike_purp <-
#   rbind(bike_purp_mdt,bike_purp_tt) %>%
#   select(-tpurp_c_total) # Remove total by purpose (now that bike share is calculated)
#
# # Graph mode share for the two surveys
# tpurps_of_modes_p5 <-
#   bike_purp %>%
#   filter(tpurp_c != "other") %>%
#   mutate(survey = recode_factor(survey,
#                                 "mdt" = "My Daily Travel (2019)",
#                                 "tt" = "Travel Tracker (2008)"
#                                 )) %>%
#   ggplot(aes(x = reorder(tpurp_c,desc(tpurp_c)), y = tpurp_c_pct, fill = survey)) +
#   geom_bar(stat = "identity",position = position_dodge2(width = .65, reverse = TRUE), width = .7) +
#   scale_y_continuous(labels = scales::label_percent()) +
#   coord_flip() +
#   theme_cmap(gridlines = "v") +
#   cmap_fill_discrete(palette = "legislation")
#
# finalize_plot(tpurps_of_modes_p5,
#               title = "Bicycle mode share by category of trips, 2008 vs. 2019.",
#               caption = "Source: CMAP analysis of MDT and TT data. Note that
#               categories are not precise comparisons.",
#               filename = "tpurps_of_modes_p5",
#               height = 6.3,
#               width = 11.3,
#               mode = "png",
#               overwrite = T)
#
#
# # Generate output table
# bike_purp %>%
#   mutate(tpurp_c_pct = paste0(round(tpurp_c_pct * 100,1),"%")) %>%
#   pivot_wider(names_from = "survey",
#               values_from = c("tpurp_c_count","tpurp_c_pct"))
