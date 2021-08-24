# This script analyzes travel behavior for individuals that work from home
# and/or telecommute. It is referenced in Policy Brief #3.

#################################################
#                                               #
#                 Libraries                     #
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


# Identify respondents who report that they don't travel for work and have a
# diary entry for working from home but did not report home as their workplace
extra_wfhers <- 
  mdt %>% # 125463
  filter(wmode == 18 | wtrav == 0, 
         wplace != 2, 
         tpurp == "Worked at home (paid)") %>% # There are 24 of these
  # Make a combined sample and person ID for recoding below.
  mutate(identifier = paste(sampno,perno,sep = "_")) %>%
  select(identifier)

# Age bins
age_breaks_wfh <- c(-1,15,29, 49, 69, 150)
age_labels_wfh <- c("5 to 15","16 to 29", "30 to 49",  "50 to 69", "70 and above")

# Create base dataset (includes individuals with no trips)
wfh_base_mdt <-
  mdt_all_respondents %>% # 30683 records
  # Keep only employed respondents (either those who report having 1+ jobs or
  # those who report being employed)
  filter(emply_ask == 1 | jobs > 0) %>% # 17656 records
  # Create a flag for people who telecommute and/or work from home
  mutate(
    # Do they telecommute? 1 is yes, 0 is no.
    tc =
      case_when(
        tcdays >= 1 ~ 1,
        TRUE ~ 0),
    
    # Do they report working from home? 1 is yes, 0 is no.
    wfh = 
      case_when(
        wplace == 2 ~ 1,
        paste(sampno,perno,sep = "_") %in% extra_wfhers$identifier ~ 1,
        TRUE ~ 0),
    
    # How often do they telecommute? 0 is none, 1 is 1-3 days/wk., 2 is 4+ days/wk.
    tc_frequency =
      case_when(
        tcdays < 1 ~ 0,
        tcdays >= 1 & tcdays <= 3 ~ 1,
        tcdays >= 4 ~ 2,
        TRUE ~ 0),
    
    # Combined flag for TC and WFH. If TC is coded, use that. If not, and they
    # work from home, make an additional category (3) for WFHers.
    tc_freq_or_wfh =
      case_when(
        tc_frequency > 0 ~ tc_frequency,
        wfh == 1 ~ 3,
        TRUE ~ 0
      ),
    
    # Do they TC or WFH? 1 is yes, 0 is no.
    tc_or_wfh =
      case_when(
        tc == 1 | wfh == 1 ~ 1,
        TRUE ~ 0)
  ) %>%
  # Create a consolidated flag for wfh and tc behavior
  mutate(combined_tc_wfh =
           case_when(
             tc_frequency == 1 ~ 1, # Flag all 1-3 day/week telecommuters
             tc_frequency == 2 ~ 2, # Then flag all 4+ day/week telecommuters
             wfh == 1 ~ 2,          # Add any WFHers that are not reporting 
                                    #   themselves in either of the first two categories
             tc_frequency == 0 ~ 0  # Finally add any remaining to the "none" category
           )) %>%
  ungroup() %>% 
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks_wfh,labels=age_labels_wfh))

# Create working dataset for charts (relies on individuals with trips)
wfh_trips_mdt <-
  mdt %>% # 125463 records
  # Add a flag for whether the respondent worked from home or in general today
  left_join(mdt %>%
              mutate(
                # Identify trips that were work from home
                wfh_today = case_when(
                  tpurp == "Worked at home (paid)" ~ 1,
                  TRUE ~ 0),
                # Identify other work trips (WFO = Work from outside the home)
                wfo_today = case_when(
                  tpurp %in% c("Worked at fixed work location",
                               "Worked at non-fixed work location",
                               "Work related (off-site meeting)") ~ 1,
                  TRUE ~ 0)) %>%
              group_by(sampno,perno) %>%
              # Flag any travelers with at least one wfh record or work trip
              summarize(wfh_today = max(0,min(1,sum(wfh_today))),
                        wfo_today = max(0,min(1,sum(wfo_today)))),
            # Join these new flags to the above data set
            by = c("sampno","perno")) %>%
  # Remove trips by individuals that do not appear in the filtered list of
  # employed individuals above. For those that are in the list, add the flags we
  # developed on telecommuting and working from home.
  inner_join(wfh_base_mdt %>% # 83291 records
               select(sampno,perno,tc,wfh,tc_frequency,tc_freq_or_wfh,
                      combined_tc_wfh,age_bin),
             by = c("sampno","perno")) %>% 
  # Recode school bus trips as other trips (there are a small but nonzero number)
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels)) %>%
  ungroup()

# Summarize data at the traveler level
wfh_person_level <-
  wfh_trips_mdt %>% # 83291 records
  group_by(sampno,perno) %>% # 17524 groups
  summarize(wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE)) %>%
  ungroup()


## Travel Tracker - only for comparison of overall trends due to differing
## survey questions - individuals who reported their workplace as home were not
## asked whether they telecommuted.


# Summary statistics (includes individuals with no trips)
wfh_tt_all <-
  tt_all_respondents %>% # 23808 records
  # Keep only employed respondents (either those who report having 1+ jobs or
  # those who report being employed full- or part-time. The individuals who
  # report having jobs while not being employed are likely volunteers.)
  filter(EMPLY == 1 | EMPLY == 2 | (JOBS > 0 & JOBS < 97)) %>% # 12966 records
  # Create a flag for people who telecommute and/or work from home
  mutate(
    # Did they telecommute? 1 is yes, 0 is no.
    tc =
      case_when(
        WHOME %in% c(1,2) ~ 1,
        TRUE ~ 0),
    
    # Was their home their workplace? 1 is yes, 0 is no.
    wfh = 
      case_when(
        WLOC == 1 ~ 1,
        TRUE ~ 0),
    
    # Combined flag for TC and WFH.
    tc_or_wfh =
      case_when(
        tc == 1 | wfh == 1 ~ 1,
        TRUE ~ 0)
  ) %>%
  ungroup() %>% 
  # And add age bins
  mutate(age_bin=cut(AGE,breaks=age_breaks_wfh,labels=age_labels_wfh))

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################

################################################################################
# Summary statistics about TC and WFH
################################################################################

# Overview of telecommuting
wfh_base_mdt %>%
  summarize(tc_pct = weighted.mean(x=tc,w=weight),
            wfh_pct = weighted.mean(x=wfh,w=weight),
            tc_or_wfh_pct = weighted.mean(x=tc_or_wfh,w=weight),
            tc = sum(weight*tc),
            wfh = sum(weight*wfh),
            wfh_not_tc = tc_or_wfh_pct - tc_pct,
            tc_or_wfh = format(sum(weight*tc_or_wfh),scientific = F))

wfh_tt_all %>%
  summarize(tc_pct = weighted.mean(x=tc,w=WGTP),
            wfh_pct = weighted.mean(x=wfh,w=WGTP),
            tc_or_wfh_pct = weighted.mean(x=tc_or_wfh,w=WGTP),
            tc = sum(WGTP*tc),
            wfh = sum(WGTP*wfh),
            tc_or_wfh = format(sum(WGTP*tc_or_wfh),scientific = F))

# Backup for prose - Summarize TC without working from home
wfh_base_mdt %>% 
  filter(wfh == 0) %>% 
  summarize(tc = weighted.mean(tc,weight)) / 
  
  wfh_tt_all %>% 
  filter(wfh == 0) %>% 
  summarize(tc = weighted.mean(tc,WGTP))
# Result: Total number of TCers has increased by 1.48X (excluding those who WFH)

# Overall baseline statistics for plot
tc_overall <-
  wfh_base_mdt %>% 
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  mutate(type = "Overall",
         subtype = "Overall")

# Breakdown of tc behavior by household income
tc_income <-
  wfh_base_mdt %>% # 17,656 records
  filter(income_c != "missing") %>% # 17,516 records
  group_by(income_c) %>%
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  # Recode for ease of understanding
  mutate(type = "Household income",
         subtype = recode_factor(income_c,
                                 "low" = "Less than $35K",
                                 "middle-low" = "$35K to $59K",
                                 "middle-high" = "$60K to $99K",
                                 "high" = "$100K or more")) %>%
  select(-income_c)

# Breakdown of tc behavior by sex
tc_sex <-
  wfh_base_mdt %>% # 17656 records
  filter(sex > 0) %>% # 17655 records
  group_by(sex) %>%
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  # Recode for ease of understanding
  mutate(type = "Sex",
         subtype = recode_factor(sex,
                                 "1" = "Male",
                                 "2" = "Female")) %>%
  select(-sex)

# Breakdown of tc behavior by home jurisdiction
tc_home <-
  wfh_base_mdt %>% # 17656 records
  # Remove individuals in DeKalb, Grundy, or those with 2+ homes (for presentation)
  filter(!(home_county_chi %in% c("DeKalb","Grundy",
                                  "Homes in multiple jurisdictions (Chicago/Cook)")
  )) %>% # 17344 records
  group_by(home_county_chi) %>%
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  mutate(type = "Home jurisdiction") %>% 
  rename(subtype = home_county_chi)

### Backup - export for Q&A
wfh_t1 <-
  tc_home %>% 
  rbind(tc_overall) %>% 
  arrange(desc(pct)) %>% 
  mutate(pct = scales::percent(pct,accuracy = 0.1)) %>% 
  select(Jurisdiction = subtype,
         'Telecommute pct.' = pct,
         'Sample size' = n) 

write.csv(wfh_t1,"wfh_t1.csv")

# Breakdown of tc behavior by race and ethnicity
tc_race_eth <-
  wfh_base_mdt %>% # 17656 records
  filter(race_eth != "missing") %>% # 17516
  group_by(race_eth) %>%
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  # Recode for ease of understanding
  mutate(type = "Race and ethnicity",
         subtype = recode_factor(race_eth,
                                 "white" = "White",
                                 "latino" = "Latino",
                                 "black" = "Black",
                                 "asian" = "Asian",
                                 "other" = "Other")) %>%
  select(-race_eth)

# Breakdown of tc behavior by race and ethnicity
tc_age <-
  wfh_base_mdt %>% # 17,656 records
  # Remove individuals without a response
  filter(!is.na(age_bin)) %>% # 17626
  # Remove 5-15 year olds 
  filter(age_bin != "5 to 15") %>% # 17462
  group_by(age_bin) %>%
  summarize(pct = weighted.mean(tc,weight),
            uw_pct = mean(tc),
            n = n(),
            w_n = sum(weight),
            tcwfhers = w_n * pct) %>% 
  mutate(type = "Age") %>% 
  rename(subtype = age_bin)

# Combine different travel statistic calculations
tc_summaries <-
  rbind(tc_overall,
        tc_sex,
        tc_age,
        tc_income,
        tc_race_eth,
        tc_home) %>% 
  # Keep relevant variables
  select(type,subtype,pct,uw_pct,n,tcwfhers) %>% 
  # Add levels
  mutate(type = factor(type,
                       levels = c("Race and ethnicity","Household income",
                                  "Age","Sex","Overall","Home jurisdiction"))) %>% 
  mutate(subtype = factor(subtype,
                          levels = c("Overall",
                                     "White","Asian","Black","Latino","Other",
                                     "Less than $35K","$35K to $59K","$60K to $99K","$100K or more",
                                     "16 to 29","30 to 49","50 to 69","70 and above",
                                     "Male","Female",
                                     "Chicago","Suburban Cook","DuPage","Kane","Kendall",
                                "McHenry","Lake","Will"
                  ))) %>% 
  # Pivot longer
  pivot_longer(cols = c(pct,n,tcwfhers,uw_pct))

# Extract values for regional averages, which will be graphed as value lines
tc_summaries_vlines <-
  tc_summaries %>%
  filter(type == "Overall" & name == "pct") %>% 
  select(-subtype,-type) %>% 
  left_join(tibble(type = c("Sex","Race and ethnicity","Household income",
                            "Age")),by = character())


# Plot of trips and distances by demographic characteristics
wfh_p1 <-
  # Get data
  tc_summaries %>%
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>% 
  # Exclude overall
  filter(!(type %in% c("Overall","Home jurisdiction"))) %>%
  # Keep only percents
  filter(name == "pct") %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype, fill = type)) +
  # Add columns
  geom_col(width = .8) +
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = tc_summaries_vlines,
             mapping = aes(xintercept = value),
             linetype = "dashed",
             size = .33
  ) +
  
  # Add regional label
  geom_label(data = tc_summaries_vlines %>% mutate(subtype = "Overall"),
             mapping = aes(x = value * 1.02,
                           label = paste0("Regional average (",
                                          round(100*value),
                                          "%)"),
                           y = 15.5),
           vjust = 0.5,
           hjust = 0,
           fill = "light gray",
           label.size = 0,
           label.r = grid::unit(0,"lines")
  ) +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 1)(value),
                 group = name),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             label.r = grid::unit(0,"lines"),
             hjust = -0.02) +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,.23),
                     labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult =)) +
  scale_y_discrete(limits = c("Female","Male", 
                              "",
                              "70 and above","50 to 69","30 to 49","16 to 29",
                              "",
                              "$100K or more","$60K to $99K","$35K to $59K","Less than $35K",
                              "",
                              "Other","Latino","Black","Asian","White")) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Share of residents who telecommute at least once a week",
             strip.text = element_text(hjust = 0.5,vjust = 1)) +
  cmap_fill_discrete(palette = "legislation")
  

wfh_p1_samplesize <-
  tc_summaries %>% 
  filter(name == "n") %>% 
  select(subtype,n = value) %>% 
  ungroup()

# Export finalized graphic
finalize_plot(wfh_p1,
              "Lower-income, Black, and Latino residents were the least likely 
              to telecommute prior to COVID-19.",
              caption = 
              paste0(
                "Note: Includes only employed residents age 16 and older from 
                the CMAP seven county region, Grundy, and DeKalb. See 'About the 
                data' for more information on race, ethnicity, and sex.
              <br><br>",
              "Sample size: Figures are based on a total of ",
              format(wfh_p1_samplesize %>% filter(subtype == "Overall") %>% 
                select(n) %>% as.numeric(),big.mark =","),
              " residents. 
              Across all categories, residents age 70 and above have the lowest 
              sample size, with ",
              format(wfh_p1_samplesize %>% filter(subtype == "70 and above") %>% 
                       select(n) %>% as.numeric(),big.mark =","),
              " individual residents.
                 <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data."),
              filename = "wfh_p1",
              mode = c("png","pdf"),
              height = 5,
              overwrite = T)

# Identify sample sizes ("Other" race/eth has the lowest)

wfh_p1_samplesize %>% 
  arrange(n)

################################################################################
# Data prep for charts of WFH/TC travel characteristics
################################################################################

# Specific analysis of work trips vs. non-work trips for individuals who report
# telecommuting or working from home (survey)
wfh_worktrip_status <-
  wfh_trips_mdt %>% # 83291 records
  # Flag trips that were part of a work trip chain
  mutate(worktrip = case_when(
    # Assign any work chain trips that don't have an associated out-of-home work
    # trip as 0 (this handles a few "work-from-home" trips that have significant
    # distances associated with them because they were the next step in a larger
    # trip chain that did not include a physical work trip).
    chain_c == "work" & wfo_today == 0 ~ 0,
    # Assign all other work chains as 1
    chain_c == "work" ~ 1,
    # Assign all other trips as 0
    TRUE ~ 0)) %>%
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") %>% # 83274 records
  # Remove "beginning" trips
  filter(mode != "beginning") %>% # 65833 records
  # Exclude trips with zero distance
  filter(distance_pg > 0) # 65793 records

# Collapse trips into total distances traveled per traveler
wfh_trips_person_level_mdt <-
  wfh_worktrip_status %>% 
  # Group by traveler and work trip status
  group_by(sampno,perno,worktrip) %>%
  # Collapse into traveler-level statistics
  summarize(pertrips = n(),
            income_c = first(income_c),
            geog = first(geog),
            home_county_chi = first(home_county_chi),
            home_county = first(home_county),
            home_tract = first(home_tract),
            home_long = first(home_long),
            home_lat = first(home_lat),
            tc_frequency = first(tc_frequency),
            wfh = first(wfh),
            combined_tc_wfh = first(combined_tc_wfh),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            weight = first(weight)) %>%
  ungroup()

# Calculate summary statistics for work trips
wfh_worktrips_summary <-
  wfh_trips_person_level_mdt %>%
  # Calculate based on TC status, work trip status, and geography
  group_by(tc_frequency,worktrip,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = weight),
            n = n()) %>%
  rename(flag = tc_frequency)

################################################################################
# Chart of work trips for individuals who report working from home sometimes but
# worked from the office today vs. those who do not telecommute but worked from
# the office today
################################################################################

wfh_p2 <-
  # Get data
  wfh_worktrips_summary %>%
  # Filter out other trips
  filter(worktrip == 1) %>%
  # Due to low sample size, exclude work trips for those that almost always 
  # telecommute
  filter(flag != 2) %>% 
  # Reformat and reorder
  mutate(flag = recode_factor(factor(flag),
                       "0" = "Does not regularly telecommute",
                       "1" = "Sometimes telecommutes (1-3 days/week)"),
         geog = factor(geog, levels = c("Collar and adjacent",
                                        "Suburban Cook",
                                        "Chicago"))) %>%
  
  
  # Create ggplot object
  ggplot(aes(x = distance_pg, str_wrap_factor(geog,18))) +
  geom_col(aes(fill = flag), position = position_dodge2(reverse = T)) +
  geom_label(aes(label = scales::label_number(accuracy = 1)(distance_pg),
                 group = flag),
             hjust = -.02,
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             position = position_dodge2(width= 0.9, reverse = T)) +
  
  # Adjust axis
  scale_x_continuous(limits = c(0,58),
                     expand = expansion(mult = c(0.05,0))) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             legend.max.columns = 1,
             vline = 0,
             xlab = "Mean total miles traveled on work trips by home jurisdiction",
             ) +
  cmap_fill_discrete(palette = "legislation")

wfh_p2_samplesize <-
  wfh_worktrips_summary %>% 
  ungroup() %>%
  filter(worktrip == 1) %>% 
  select(geog,flag,n)

# Export finalized graphic
finalize_plot(wfh_p2,
              "Part-time telecommuters who lived outside Chicago had 
              significantly longer journeys to and from work on days when they 
              worked outside the home.",
              paste0(
              "Note: Figures are for trips by employed residents age 16 and older from the 
              CMAP seven county region, Grundy, and DeKalb. Includes only 
              trips within, to, and/or from those counties. Mean mileage 
              accounts for all trips associated with a work trip chain 
              that included a non-home work destination. 
              <br><br>
              Sample size (Chicago/Suburban Cook/Collar and adjacent):
              <br>- Does not regularly telecommute (",
                     paste(wfh_p2_samplesize %>%
                             filter(geog == "Chicago",
                                    flag == 0) %>%
                             select(n) %>%
                             as.numeric(),
                           wfh_p2_samplesize %>%
                             filter(geog == "Suburban Cook",
                                    flag == 0) %>%
                             select(n) %>%
                             as.numeric(),
                           wfh_p2_samplesize %>%
                             filter(geog == "Collar and adjacent",
                                    flag == 0) %>%
                             select(n) %>%
                             as.numeric(),
                           sep = "/"),
                     ");
              <br>- Sometimes telecommutes (",
                     paste(wfh_p2_samplesize %>%
                             filter(geog == "Chicago",
                                    flag == 1) %>%
                             select(n) %>%
                             as.numeric(),
                           wfh_p2_samplesize %>%
                             filter(geog == "Suburban Cook",
                                    flag == 1) %>%
                             select(n) %>%
                             as.numeric(),
                           wfh_p2_samplesize %>%
                             filter(geog == "Collar and adjacent",
                                    flag == 1) %>%
                             select(n) %>%
                             as.numeric(),
                           sep = "/"),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "wfh_p2",
              height = 5.25,
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Chart of mode share comparing the habits of commuters by TC status
################################################################################

wfh_mode_share <-
  pct_calculator(
    wfh_worktrip_status %>%            # 65793
      # Keep only work trips
      filter(worktrip == 1) %>%        # 40539
      # Exclude missing modes
      filter(mode_c != "missing") %>%  # 40534
      # Exclude improbable walk trips
      filter(improbable_walk == 0) %>% # 40512
      # Due to low sample size, exclude work trips for those that almost always
      # telecommute
      filter(tc_frequency != 2),       # 39993 
    breakdown_by = "mode_c",
    second_breakdown = "tc_frequency",
    third_breakdown = "geog",
    weight = "weight")

wfh_p3 <-
  wfh_mode_share %>% 
  # Reformat and reorder
  mutate(tc_frequency = recode_factor(factor(tc_frequency,levels = c(0,1)),
                              "1" = "1-3 days/wk",
                              "0" = "0 days/wk"),
         mode_c = recode_factor(factor(mode_c,levels = mode_c_levels),
                                "driver" = "By car",
                                "passenger" = "By car",
                                "transit" = "Transit",
                                "walk" = "Walk",
                                "bike" = "Bicycle",
                                "other" = "Other")) %>%
  
  group_by(mode_c,tc_frequency,geog) %>% 
  summarize(pct = sum(pct)) %>% 
  
  # Create ggplot object
  ggplot(aes(x = pct, y = tc_frequency,
         # Only label bars that round to at least 5 percent
         label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) +
  geom_col(aes(fill = mode_c), position = position_stack(reverse = T)) +
  geom_text(position = position_stack(vjust = 0.5),
            color = "white") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.05,0))) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             legend.max.columns = 7,
             vline = 0,
             xlab = "Mode share for work trips by home location and telecommute status",
             axis.title.x = element_text(hjust = 0.5),
             strip.text = element_text(hjust = 0.5,vjust = 1,
                                       family = "Whitney Semibold")) +
  # Manually add colors
  scale_fill_discrete(type = c("#00665c","#6d8692","#36d8ca",
                               "#efa7a7","#0084ac")) +
  
  
  # Add faceting
  facet_wrap(~geog,ncol = 1)

wfh_p3_samplesize <-
  wfh_mode_share %>% 
  ungroup()

# Export finalized graphic
finalize_plot(wfh_p3,
              "Suburban part-time telecommuters were much more 
              likely to use transit on days when they worked outside the home, 
              while those in Chicago were more likely to walk to work.",
              paste0(
                "Note: Figures are for trips by employed residents age 16 and older from the 
              CMAP seven county region, Grundy, and DeKalb. Includes only 
              trips within, to, and/or from those counties. Mode share
              accounts for all work chain trips
              that included a non-home work destination. 
              Unlabeled bars have less than five percent mode share.
              <br><br>
              Sample size (Chicago/Suburban Cook/Collar and adjacent):
              <br>- 0 days/wk. (",
              paste(wfh_p3_samplesize %>% 
                      filter(geog == "Chicago", 
                             mode_c == "driver", tc_frequency == 0) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    wfh_p3_samplesize %>% 
                      filter(geog == "Suburban Cook", 
                             mode_c == "driver", tc_frequency == 0) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    wfh_p3_samplesize %>% 
                      filter(geog == "Collar and adjacent", 
                             mode_c == "driver", tc_frequency == 0) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    sep = "/"),
              ");
              <br>- 1-3 days/wk. (",
              paste(wfh_p3_samplesize %>% 
                      filter(geog == "Chicago", 
                             mode_c == "driver", tc_frequency == 1) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    wfh_p3_samplesize %>% 
                      filter(geog == "Suburban Cook", 
                             mode_c == "driver", tc_frequency == 1) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    wfh_p3_samplesize %>% 
                      filter(geog == "Collar and adjacent", 
                             mode_c == "driver", tc_frequency == 1) %>% 
                      select(total_n) %>% 
                      as.numeric(),
                    sep = "/"),
              ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "wfh_p3",
              height = 5.25,
              mode = c("png","pdf"),
              overwrite = T
              )

################################################################################
# Chart of all mileage for trips for individuals by TC status
################################################################################

# Calculate averages for all trips
wfh_alltraveler_trips <-
  # Start with the universe of all relevant respondents in MDT
  wfh_base_mdt %>% # 17656
  # Add distances traveled by individuals who had trips
  left_join(wfh_trips_person_level_mdt %>%
              # Combine work and non-work trips
              group_by(sampno,perno) %>%
              summarize(distance_pg = sum(distance_pg),
                        wfh_today = first(wfh_today),
                        wfo_today = first(wfo_today)) %>% 
              ungroup(),
            by = c("sampno","perno")) %>% 
  # Add trips for everyone who didn't travel today (these are included
  # in the averages as 0 distance)
  replace_na(list(distance_pg = 0,
                  wfh_today = 0,
                  wfo_today = 0)) %>%
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") # 17654 records

# Calculate average distances by Chicago/Suburban Cook/Other
wfh_alltraveler_trips_summary <-
  wfh_alltraveler_trips %>%
  # Calculate average distances
  group_by(tc_frequency,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = weight),
            n = n()) %>%
  rename(flag = tc_frequency) %>% 
  ungroup()

# Create chart
wfh_p4 <-
  # Get data
  wfh_alltraveler_trips_summary %>%
  # Reformat and reorder
  mutate(flag = recode_factor(factor(flag,levels = c(0,1,2)),
                              "0" = "Does not regularly telecommute",
                              "1" = "Sometimes telecommutes (1-3 days/week)",
                              "2" = "Almost always telecommutes (4+ days/week)"),
         
         geog = factor(geog, levels = c("Collar and adjacent",
                                        "Suburban Cook",
                                        "Chicago"))) %>%
  
  
  # Create ggplot object
  ggplot(aes(x = distance_pg, y = str_wrap_factor(geog,18), 
             label = scales::label_number(accuracy = .1)(distance_pg))) +
  geom_col(aes(fill = flag),
           position = position_dodge2(reverse = T,width = 0.9)) +

  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = 1)(distance_pg),
                 group = flag),
             position = position_dodge2(reverse = T,width = 0.9),
             label.size = 0,
             label.r = grid::unit(0,"lines"),
             hjust = -.02) +
  
  # Adjust axis
  scale_x_continuous(limits = c(0,51),
                     expand = expansion(mult = c(0.05,0))) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             vline = 0,legend.max.columns = 1,
             axis.title.x = element_text(hjust = .5),
             strip.text = element_text(hjust = 0.5),
             xlab = "Mean total miles traveled by home jurisdiction",
             ) +
  scale_fill_discrete(type = c("#00becc","#67ac00","#cc5f00"))

# Export finalized graphic
finalize_plot(wfh_p4,
              "On average, part-time telecommuters living outside Chicago 
              traveled the greatest distances every day.",
              paste0(
              "Note: Figures are for trips by employed residents age 16 and older from the
              CMAP seven county region, Grundy, and DeKalb. Includes only
              trips within, to, and/or from those counties. Individuals who did 
              not travel are included as having zero travel distance.
              <br><br>
              Sample size (Chicago/Suburban Cook/Collar and adjacent):
              <br>- Not regular (",
                     paste(wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Chicago", flag == 0) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Suburban Cook", flag == 0) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Collar and adjacent", flag == 0) %>% 
                             select(n) %>% 
                             as.numeric(),
                           sep = "/"),
                     ");
              <br>- Sometimes (",
                     paste(wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Chicago", flag == 1) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Suburban Cook", flag == 1) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Collar and adjacent", flag == 1) %>% 
                             select(n) %>% 
                             as.numeric(),
                           sep = "/"),
                     ");
              <br>- Almost always (",
                     paste(wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Chicago", flag == 2) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Suburban Cook", flag == 2) %>% 
                             select(n) %>% 
                             as.numeric(),
                           wfh_alltraveler_trips_summary %>% 
                             filter(geog == "Collar and adjacent", flag == 2) %>% 
                             select(n) %>% 
                             as.numeric(),
                           sep = "/"),
                     ").
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "wfh_p4",
              height = 4.25,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Backup - summary of work from home / outside the home behavior by TC/WFH frequency
################################################################################

wfh_alltraveler_trips %>% 
  mutate(flag = case_when(
    tc_freq_or_wfh %in% c(2,3) ~ 2,
    TRUE ~ tc_freq_or_wfh
  )) %>% 
  count(flag,wfo_today,wt = weight) %>% 
  group_by(flag) %>% 
  mutate(pct = n / sum(n))
# 76% of non-TC/WFHers worked outside the home, while only 70% of part-time TCers
# did and 21% of 4+ TCers/WFHers did.

wfh_alltraveler_trips %>% 
  mutate(flag = case_when(
    tc_freq_or_wfh %in% c(2,3) ~ 2,
    TRUE ~ tc_freq_or_wfh
  )) %>% 
  count(flag,wfh_today,wt = weight) %>% 
  group_by(flag) %>% 
  mutate(pct = n / sum(n))
# 2% of non-TCers reported working from home. 17% of part-time TCers did and 38%
# of 4+ TCers did. This does not capture individuals who may have worked from
# home but did not record any trips and are thus excluded from the trip-based
# analyses.


################################################################################
# Backup - summary of non-work chain trips (for prose)
################################################################################

# Calculate the average length of non-work travel by TC/WFH status and home
# jurisdiction

# Start with the universe of all relevant respondents in MDT
wfh_base_mdt %>% 
  # Add travel statistics for those with trips
  left_join(wfh_trips_person_level_mdt %>% 
              # Keep only non-work trips
              filter(worktrip == 0) %>% 
              select(sampno,perno,wfh_today,wfo_today,distance_pg),
            by = c("sampno","perno")) %>% 
  # Add non-work trips for everyone who didn't travel today
  # Mutate NAs for distance and wfo/wfh flags for non-travelers to be 0
  replace_na(list(wfh_today = 0, wfo_today = 0,distance_pg = 0)) %>% 
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") %>% # 82772 records
  # Calculate median distances
  group_by(combined_tc_wfh,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = weight),
            n = n()) %>%
  rename(flag = combined_tc_wfh) %>% 
  ungroup() %>% 
  arrange(geog)