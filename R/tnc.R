# This script analyzes TNC usage in MDT, using a combination of travel diary and
# survey entries. It is referenced in Policy Brief #4.

#################################################
#                                               #
#                 Load libraries                #
#                                               #
#################################################

library(tidyverse)
library(cmapplot)
library(ggpattern)
library(lubridate)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/mdt_dates.R")
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
  group_by(sampno,perno) %>% # 24790
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
  mutate(high_income = case_when(
           income_c %in% c("high","middle-high") ~ 1,
           TRUE ~ 0),
         cook = case_when(
           home_county == 31 ~ 1,
           TRUE ~ 0)
         ) %>% 
  mutate(age_bin = cut(age, breaks = age_breaks_tnc,
                       labels = age_labels_tnc),
         race_eth = factor(race_eth,
                           levels = c("other","white","asian",
                                      "black","latino","missing")))

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
  tnc %>%
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
       cook + # Is their home in Cook County?
       high_income + # Is their household income above $60,000/year?
       hhveh + # How many household vehicles do they have?
       smrtphn + # Do they have a smartphone?
       pertrips + # How many trips did they take on their travel day?
       race_eth,
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
  mutate(tnc_purp = recode(factor(tnc_purp,levels = c(2,3,1,4,5)),
                           "3" = "Daytime (work)",
                           "1" = "Commute (whole or part)",
                           "2" = "Commute (whole or part)",
                           "4" = "Daytime (non-work)",
                           "5" = "Late night (non-work)"))

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


figure4_2_labels <-
  tnc_purpose_race %>% 
  filter(tnc_purp %in% c("Late night (non-work)","Daytime (non-work)")) %>% 
  group_by(race_eth) %>% 
  summarize(label = sum(pct))

figure4_2 <-
  # Get data
  tnc_purpose_race %>% 
  # Add flag for pattern
  mutate(type = case_when(race_eth == "CMAP region" ~ "1",
                          TRUE ~ "0" )) %>% 
  # Adjust values for shift around 0 axis
  mutate(pct = ifelse(!(tnc_purp %in% c("Late night (non-work)","Daytime (non-work)")),
                      -1 * pct, pct)) %>% 
  # Add labels
  left_join(figure4_2_labels, by = "race_eth") %>% 
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
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_spacing = 0.02,
                              position = position_stack(reverse = T),
                              width = 0.85) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-.5,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-.5,.75,by = .25))),
                     limits = c(-.5,.75),
                     expand = expansion(mult = c(0.05,0))
                     ) +
  
  # Add CMAP themes
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Work travel                              Non-work travel\nTypical reason for using a TNC") +
  scale_fill_manual(values = c("#43B649","#D8BA37","#2C2B7F","#38B2D8"),
                    labels = c("Daytime (work)","Commute (whole or part)",
                               "Daytime (non-work)","Late night (non-work)")) +
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 3,
                             override.aes = list(fill = c("#D8BA37","#43B649",
                                                          "#2C2B7F","#38B2D8"),
                                                 pattern = "none")))

figure4_2_samplesize <-
  tnc_purpose_race %>% 
  ungroup() %>% 
  select(race_eth,n = total_n) %>% 
  distinct()

finalize_plot(figure4_2,
              title = "Non-white travelers were much more likely to report using 
              transportation network companies (TNCs) for work-related trips.",
              caption = 
              paste0(
              "Note:
              \"Latino\" includes respondents who identified as Latino or Hispanic, 
              regardless of racial category. Other categories are non-Latino.
              Excludes travelers age 18 and younger, who were not asked about 
              TNC usage.
              <br><br>
              Sample size:
              <br>- White (",
              format(figure4_2_samplesize %>% 
                       filter(race_eth == "white") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Asian (",
              format(figure4_2_samplesize %>% 
                       filter(race_eth == "asian") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Latino (",
              format(figure4_2_samplesize %>% 
                       filter(race_eth == "latino") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Other (",
              format(figure4_2_samplesize %>% 
                       filter(race_eth == "other") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ");
              <br>- Black (",
              format(figure4_2_samplesize %>% 
                       filter(race_eth == "black") %>% 
                       select(n) %>% as.numeric(),
                     big.mark = ","),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning 
              Analysis of My Daily Travel data."),
              filename = "figure4_2",
              height = 4.5,
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Backup - Purpose and home location
################################################################################

tnc_purpose_homeloc <-
  pct_calculator(tnc_for_purposes %>% filter(!(home_county_chi %in% c("Grundy","DeKalb"))),
                 breakdown_by = "tnc_purp",
                 second_breakdown = "home_county_chi",
                 weight = "weight") %>% 
  # Add baseline totals
  rbind(tnc_purpose_overall %>% mutate(home_county_chi = "CMAP region")) %>% 
  select(tnc_purp,home_county_chi,total_n,pct) %>% 
  pivot_wider(names_from = tnc_purp,values_from = pct) %>% 
  rename(Jurisdiction = home_county_chi,
         "Sample size" = total_n) %>% 
  mutate(work_total = .$'Commute (whole or part)' + .$'Daytime (work)',
         nonwork_total = .$'Late night (non-work)' + .$'Daytime (non-work)') %>% 
  arrange(desc(work_total)) %>% 
  mutate(across("Commute (whole or part)":nonwork_total,
                ~scales::percent(.,accuracy = 0.1))) %>% 
  rename("Total: Work" = work_total,
         "Total: Non-work" = nonwork_total)

write.csv(tnc_purpose_homeloc,"tnc_t1.csv")

################################################################################
# Usage and cost
################################################################################

overall_usage <-
  tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  summarize(tnc_use = weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n())

overall_cost <-
  tnc %>%
  filter(tnc_cost > 0) %>% 
  summarize(tnc_cost = weighted.mean(tnc_cost,weight, na.rm = TRUE),
            n = n())

################################################################################
# PLOT OF HOME JURISDICTION USAGE
################################################################################

# Usage by home county
home_usage <- 
  tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1))) %>%
  # Limit to residents of the seven counties for presentation. Grundy and DeKalb
  # are included in regional averages.
  filter(home_county %in% cmap_seven_counties) %>%
  group_by(home_county_chi) %>%
  summarize(tnc_use = weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n()) %>% 
  rbind(overall_usage %>% mutate(home_county_chi = "CMAP region"))

# Generate output chart for age
figure4_3 <-
  # Get data
  home_usage %>% 
  
  # Create ggplot object
  ggplot(aes(x = tnc_use, y = reorder(home_county_chi,tnc_use), fill = home_county_chi)) +
  geom_col(width = 0.85) +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(tnc_use)),
             hjust = -0.02,
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,legend.position = "none",
             xlab = "TNC use per week by home jurisdiction") +
  cmap_fill_highlight(home_usage$home_county_chi,
                      "CMAP region",
                      color_value = "#38B2D8") +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,.87),
                     expand = expansion(mult = c(0.05,0)))

figure4_3_samplesize <-
  home_usage %>% 
  select(home_county_chi,
         n) %>% 
  mutate(n = format(n,big.mark = ",")) %>% 
  mutate(n = gsub(" ","",n))

finalize_plot(figure4_3,
              "Use of transportation network companies (TNCs) was greatest by 
              residents of Chicago and suburban Cook County.",
              paste0(
                "Note: These figures are based on survey responses and not 
              trip diaries. 
              Excludes travelers age 18 and younger, who were 
              not asked about TNC use. 
              <br><br>
              Sample size:
              <br>- Chicago (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Chicago") %>% 
                  select(n),
                ");
              <br>- Suburban Cook (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Suburban Cook") %>% 
                  select(n),
                ");
              <br>- DuPage (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "DuPage") %>% 
                  select(n),
                ");
              <br>- Lake (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Lake") %>% 
                  select(n),
                ");
              <br>- Kane (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Kane") %>% 
                  select(n),
                ");
              <br>- Will (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Will") %>% 
                  select(n),
                "); 
              <br>- McHenry (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "McHenry") %>% 
                  select(n),
                ");
              <br>- Kendall (",
                figure4_3_samplesize %>% 
                  filter(home_county_chi == "Kendall") %>% 
                  select(n),
                ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "figure4_3",
              height = 4.55,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# PLOT OF AGE CHARACTERISTICS | USAGE AND COST
################################################################################

## Look at usage by age
age_usage <-
  tnc %>%
  filter(!(tnc_use %in% c(-9,-8,-7,-1)),
         !(is.na(age_bin))) %>%
  group_by(age_bin) %>%
  summarize(tnc_use = weighted.mean(tnc_use,weight, na.rm = TRUE),
            n = n())

# Cost by age
age_cost <- 
  tnc %>%
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
figure4_4 <-
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
  ggplot(aes(x = value, y = factor(age_bin,levels = rev(levels(age_bin))), 
             fill = age_bin)) +
  geom_col(width = 0.85) +
  geom_label(aes(label = ifelse(name == "Average weekly trips",
                                scales::label_number(accuracy = 0.1)(value),
                                scales::label_dollar(accuracy = 1)(value))),
             hjust = -.02,
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             fill = "white") +
  geom_blank(aes(x = blank)) +
  
  # Add faceting
  facet_wrap(~name, scales = "free_x") +
  
  scale_x_continuous(expand = expansion(mult = c(0.05,0))) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",vline = 0,legend.position = "none",
             panel.spacing.x = unit(30,"bigpts"),
             xlab = "TNC use characteristics by age",
             strip.text = element_text(family = "Whitney Semibold",
                                       hjust = 0.5,vjust = 1)) +
  cmap_fill_highlight(age_cost_and_usage$age_bin,
                      "CMAP region",
                      color_value = "#38B2D8")

figure4_4_samplesize <-
  age_usage %>% select(age_bin,usage = n) %>% 
  left_join(age_cost %>% select(age_bin,cost = n), by = "age_bin") %>% 
  ungroup() %>% 
  mutate(across(c(usage,cost),~gsub(" ","",format(.,big.mark = ","))))

finalize_plot(figure4_4,
              "Use of transportation network companies (TNCs) decreased by age, 
              while the average cost per trip increased with age.",
              paste0("Note: These figures are based on survey responses and not 
              trip diaries. Excludes travelers age 18 and younger, as they were 
              not asked about TNC use.
              <br><br>
              Sample size (Use/Cost): 
              <br>- 19-29 (",
              paste(figure4_4_samplesize %>% filter(age_bin == "19 to 29") %>% select(usage),
                    figure4_4_samplesize %>% filter(age_bin == "19 to 29") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 30-39 (",
              paste(figure4_4_samplesize %>% filter(age_bin == "30 to 39") %>% select(usage),
                    figure4_4_samplesize %>% filter(age_bin == "30 to 39") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 40-49 (",
              paste(figure4_4_samplesize %>% filter(age_bin == "40 to 49") %>% select(usage),
                    figure4_4_samplesize %>% filter(age_bin == "40 to 49") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 50-59 (",
              paste(figure4_4_samplesize %>% filter(age_bin == "50 to 59") %>% select(usage),
                    figure4_4_samplesize %>% filter(age_bin == "50 to 59") %>% select(cost),
                    sep = "/"),
                    "); 
              <br>- 60+ (",
              paste(figure4_4_samplesize %>% filter(age_bin == "60 and above") %>% select(usage),
                    figure4_4_samplesize %>% filter(age_bin == "60 and above") %>% select(cost),
                    sep = "/"),
                    ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "figure4_4",
              height = 4.25,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chicago TNC trips
################################################################################

# Load trip totals sourced from the following url:
# https://data.cityofchicago.org/d/m6dm-c72p/visualization .The City of
# Chicago's visualization tool allows total trips to be grouped by day. Data
# were filtered to the desired time period and copied and pasted the totals into
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
# weeks of holidays documented in 'mdt_dates.R'.)

