# This script analyzes travel behavior for individuals that work from home
# and/or telecommute.

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)
library(sf)

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
extra_wfhers <- mdt %>%
  filter(wmode == 18 | wtrav == 0, 
         wplace != 2, 
         tpurp == "Worked at home (paid)") %>%
  mutate(identifier = paste(sampno,perno,sep = "_")) %>%
  select(identifier)

# Age bins
age_breaks <- c(-1,17,29, 49, 69, 150)
age_labels <- c("5 to 17","18 to 29", "30 to 49",  "50 to 69", "70 and above")

# Summary statistics (includes individuals with no trips)
wfh_mdt_all <-
  mdt_all_respondents %>% # 30,683 records
  # Keep only employed respondents (either those who report having 1+ jobs or
  # those who report being employed)
  filter(emply_ask == 1 | jobs > 0) %>% # 17,656 records
  # Create a flag for people who telecommute and/or work from home
  mutate(tc =
           case_when(
             tcdays >= 1 ~ 1,
             TRUE ~ 0),
         
         wfh = 
           case_when(
             wplace == 2 ~ 1,
             paste(sampno,perno,sep = "_") %in% extra_wfhers$identifier ~ 1,
             TRUE ~ 0),
         
         tc_frequency =
           case_when(
             tcdays < 1 ~ 0,
             tcdays >= 1 & tcdays <= 3 ~ 1,
             tcdays >= 4 ~ 2,
             TRUE ~ 0),
         
         tc_or_wfh =
           case_when(
             tc == 1 | wfh == 1 ~ 1,
             TRUE ~ 0
           )
         ) %>%
  # Create a consolidated flag for wfh and tc behavior
  mutate(combined_tc_wfh =
           case_when(
             tc_frequency == 1 ~ 1, # Flag all 1-3 day/week telecommuters
             tc_frequency == 2 ~ 2, # Then flag all 4-5 day/week telecommuters
             wfh == 1 ~ 2,          # Add any WFHers that are not reporting 
                                    #   themselves in either of the first two categories
             tc_frequency == 0 ~ 0  # Finally add any remaining to the "none" category
           )) %>%
  ungroup() %>% 
  # And add age bins
  mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels))

# Create working dataset for charts (relies on individuals with trips)
wfh_mdt <-
  mdt %>% # 125,103 records
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
  inner_join(wfh_mdt_all %>% # 83291 records
               select(sampno,perno,tc,wfh,tc_frequency,combined_tc_wfh,age_bin),
             by = c("sampno","perno"))

# Summarize data at the traveler level
wfh_person_level <-
  wfh_mdt %>% # 83291 records
  group_by(sampno,perno) %>% # 17524 groups
  summarize(wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE)) %>%
  ungroup()

#################################################
#                                               #
#                 Analysis                      #
#                                               #
#################################################

################################################################################
# Summary statistics about TC and WFH
################################################################################

# Overview of telecommuting
wfh_mdt_all %>%
  summarize(tc_pct = weighted.mean(x=tc,w=wtperfin),
            wfh_pct = weighted.mean(x=wfh,w=wtperfin),
            tc_or_wfh_pct = weighted.mean(x=tc_or_wfh,w=wtperfin),
            tc = sum(wtperfin*tc),
            wfh = sum(wtperfin*wfh),
            tc_or_wfh = format(sum(wtperfin*tc_or_wfh),scientific = F))

# Overall baseline statistics for plot
tcwfh_overall <-
  wfh_mdt_all %>% 
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  mutate(type = "Overall",
         subtype = "Overall")

# Breakdown of tc/wfh behavior by household income
tcwfh_income <-
  wfh_mdt_all %>% # 17,656 records
  filter(income_c != "missing") %>% # 17,516 records
  group_by(income_c) %>%
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  # Recode for ease of understanding
  mutate(type = "Household income",
         subtype = recode_factor(income_c,
                                 "low" = "Less than $35K",
                                 "middle-low" = "$35K to $59K",
                                 "middle-high" = "$60K to $99K",
                                 "high" = "$100K or more")) %>%
  select(-income_c)

# Breakdown of tc/wfh behavior by sex
tcwfh_sex <-
  wfh_mdt_all %>% # 17,656 records
  filter(sex >0) %>% # 17,516 records
  group_by(sex) %>%
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  # Recode for ease of understanding
  mutate(type = "Sex",
         subtype = recode_factor(sex,
                                 "1" = "Male",
                                 "2" = "Female")) %>%
  select(-sex)

# Breakdown of tc/wfh behavior by home jurisdiction
tcwfh_home <-
  wfh_mdt_all %>% # 17,656 records
  group_by(home_county_chi) %>%
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  # Remove individuals in DeKalb, Grundy, or those with 2+ homes
  filter(!(home_county_chi %in% c("DeKalb","Grundy","Homes in multiple counties"))) %>% 
  mutate(type = "Home jurisdiction") %>% 
  rename(subtype = home_county_chi)

# Breakdown of tc behavior by race and ethnicity
tcwfh_race_eth <-
  wfh_mdt_all %>% # 17,656 records
  group_by(race_eth) %>%
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  filter(race_eth != "missing") %>% 
  # Recode for ease of understanding
  mutate(type = "Race and ethnicity",
         subtype = recode_factor(race_eth,
                                 "white" = "White",
                                 "hispanic" = "Hispanic",
                                 "black" = "Black",
                                 "asian" = "Asian",
                                 "other" = "Other")) %>%
  select(-race_eth)

# Breakdown of tc behavior by race and ethnicity
tcwfh_age <-
  wfh_mdt_all %>% # 17,656 records
  group_by(age_bin) %>%
  summarize(pct = weighted.mean(tc_or_wfh,wtperfin),
            uw_pct = mean(tc_or_wfh),
            n = sum(wtperfin),
            tcwfhers = n * pct) %>% 
  # Remove individuals without a response
  filter(!is.na(age_bin),
         age_bin != "5 to 17") %>% 
  mutate(type = "Age") %>% 
  rename(subtype = age_bin)

# Combine different travel statistic calculations
tcwfh_summaries <-
  rbind(tcwfh_overall,
        tcwfh_sex,
        tcwfh_age,
        tcwfh_income,
        tcwfh_race_eth,
        tcwfh_home) %>% 
  # Keep relevant variables
  select(type,subtype,pct,uw_pct,n,tcwfhers) %>% 
  # Add levels
  mutate(type = factor(type,
                       levels = c("Sex","Race and ethnicity","Household income",
                                  "Age","Home jurisdiction","Overall"))) %>% 
  mutate(subtype = 
           factor(subtype,
                  levels = c("Overall",
                             "Male","Female",
                             "Asian","White","Other","Hispanic","Black",
                             "5 to 17","18 to 29","30 to 49","50 to 69",
                                "70 and above",
                             "Less than $35K","$35K to $59K","$60K to $99K",
                                "$100K or more",
                             "Chicago","Suburban Cook","DuPage","Kane","Kendall",
                                "McHenry","Lake","Will"
                  ))) %>% 
  # Pivot longer
  pivot_longer(cols = c(pct,n,tcwfhers,uw_pct))

# Extract values for regional averages, which will be graphed as value lines
tcwfh_summaries_vlines <-
  tcwfh_summaries %>%
  filter(type == "Overall" & name == "uw_pct") %>% 
  select(-subtype,-type) %>% 
  left_join(tibble(type = c("Sex","Race and ethnicity","Household income",
                            "Age")),by = character())


# Plot of trips and distances by demographic characteristics
wfh_p1 <-
  # Get data
  tcwfh_summaries %>%
  # Reverse factors
  mutate(subtype = factor(subtype,levels = rev(levels(subtype)))) %>% 
  # Exclude overall
  filter(!(type %in% c("Overall","Home jurisdiction"))) %>%
  # Keep only percents
  filter(name == "pct") %>% 
  
  # Create ggplot object
  ggplot(aes(x = value, y = subtype, fill = type)) +
  # Add columns
  geom_col(width = .8,show.legend = FALSE) +
  # Add lines for average trips per day and average distance per trip
  geom_vline(data = tcwfh_summaries_vlines,
             mapping = aes(xintercept = value, color = "Dashed lines represent regional share (18.9%)"),
             linetype = "dashed",
             size = .65
  ) +
  
  # Add labels
  geom_label(aes(label = scales::label_percent(accuracy = 1)(value),
                 group = name),
             fill = "white",
             label.size = 0,label.padding = unit(1.5,"bigpts"),
             hjust = 0) +
  
  # Adjust axes
  scale_x_continuous(limits = c(0,.32),labels = scales::label_percent(accuracy = 1)) +
  
  # Add CMAP theme
  theme_cmap(gridlines = "v",vline = 0,
             xlab = "Share of residents who telecommute and/or work from home",
             strip.text = element_text(hjust = 0.5)) +
  cmap_fill_discrete(palette = "legislation") +
  scale_color_discrete(type = "#181f22") +
  
  # Add faceting
  facet_wrap(~type,ncol = 2,scales = "free_y")

# Export finalized graphic
finalize_plot(wfh_p1,
              # sidebar_width = 0,
              "Lower-income, Black, and Hispanic residents are the least likely 
              to telecommute or work from home.",
              caption = "Note: Includes only employed residents. 'Hispanic' 
              includes respondents who identified as Hispanic of any racial 
              category. Other categories are non-Hispanic. For the 
              categorization by sex, the survey asked respondents whether they 
              were male or female. A small number of respondents chose not to 
              answer and are excluded based on small sample sizes.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My 
              Daily Travel data.",
              filename = "wfh_p1",
              mode = "png",
              # height = 6.5,
              overwrite = T)


################################################################################
# Data prep for charts of WFH/TC travel characteristics
################################################################################

# Specific analysis of work trips vs. non-work trips for individuals who report
# telecommuting or working from home (survey)
wfh_worktrips_relevant <-
  wfh_mdt %>% # 83291 records
  # Flag trips that were part of a work trip chain
  mutate(worktrip = case_when(
    # Assign any work chain trips that don't have an associated out-of-home work
    # trip as 0 (this handles a few "work-from-home" trips that have significant
    # distances associated with them because they were the next step in a larger
    # trip chain that did not include a physical work trip).
    chain %in% c("Work trip","Return home (work)") & wfo_today == 0 ~ 0,
    # Assign all other work chains as 1
    chain %in% c("Work trip","Return home (work)") ~ 1,
    # Assign all other trips as 0
    TRUE ~ 0)) %>%
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") %>% # 82772 records
  # Remove "beginning" trips
  filter(mode != "beginning") %>% # 65433 records
  # Exclude trips with zero distance
  filter(distance_pg > 0) # 65393 records
  # # Create a flag for travelers that have trips in both categories
  # group_by(sampno,perno) %>%
  # mutate(bothtrips = ifelse(min(worktrip) == 0 & max(worktrip) == 1,1,0)) %>%
  # filter(bothtrips == 1)

# Collapse trips into total distances traveled per traveler
wfh_worktrips_person_level <-
  wfh_worktrips_relevant %>% 
  # Group by traveler and work trip status
  group_by(sampno,perno,worktrip) %>%
  # Collapse into traveler-level statistics
  summarize(pertrips = n(),
            income_c = first(income_c),
            geog = first(geog),
            home_county = first(home_county),
            home_tract = first(home_tract),
            home_long = first(home_long),
            home_lat = first(home_lat),
            combined_tc_wfh = first(combined_tc_wfh),
            wfh_today = first(wfh_today),
            wfo_today = first(wfo_today),
            distance_pg = sum(distance_pg, na.rm = TRUE),
            travtime_pg = sum(travtime_pg, na.rm = TRUE),
            wtperfin = first(wtperfin)) %>%
  ungroup()

# Calculate summary statistics for work trips
wfh_worktrips_general <-
  wfh_worktrips_person_level %>%
  # Calculate based on TC/WFH status, work trip status, and geography
  group_by(combined_tc_wfh,worktrip,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            n = n()) %>%
  rename(flag = combined_tc_wfh)

################################################################################
# Chart of work trips for individuals who report working from home sometimes but
# worked from the office today vs. those who do not telecommute but worked from
# the office today
################################################################################

wfh_p2 <-
  # Get data
  wfh_worktrips_general %>%
  # Filter out other trips
  filter(worktrip == 1) %>%
  # Due to low sample size, exclude work trips for those that almost always work
  # from home/telecommute
  filter(flag != 2) %>% 
  # Reformat and reorder
  mutate(flag = recode_factor(factor(flag),
                       "0" = "Does not regularly telecommute or work from home",
                       "1" = "Sometimes telecommutes or works from home (1-3 days/week)"),
         geog = recode_factor(geog,
                              "Other suburban counties" = "Other counties",
                              "Suburban Cook" = "Suburban Cook",
                              "Chicago" = "Chicago")) %>%
  
  
  # Create ggplot object
  ggplot(aes(x = distance_pg, y = geog)) +
  geom_col(aes(fill = flag), position = position_dodge2(reverse = T)) +
  geom_label(aes(label = scales::label_number(accuracy = 0.1)(distance_pg),
                 group = flag),
             hjust = 0,
             label.size = 0,
             position = position_dodge2(width= 0.9, reverse = T)) +
  
  # Adjust axis
  scale_x_continuous(limits = c(0,60)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             legend.max.columns = 1,
             vline = 0,
             xlab = "Mean total miles traveled on work trips by home jurisdiction
             \n(excluding travelers with no trips to a work location)",
             axis.title.x = element_text(hjust = 0.5)) +
  cmap_fill_discrete(palette = "legislation")

# Export finalized graphic
finalize_plot(wfh_p2,
              "Part-time telecommuters who live outside Chicago have 
              significantly longer journeys to and from work on days when they 
              do go into the office.",
              "Note: These estimates are based on on answers given in both the
              survey and travel diary components of My Daily Travel. Mean 
              mileage accounts for all trips associated with a work trip chain 
              that included a work destination outside the home (both fixed and 
              non-fixed). Excludes work trips for travelers that
              telecommute 4+ days per week due to low sample sizes.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "wfh_p2",
              mode = "png",
              overwrite = T,
              # height = 4.5,
              # width = 8,
              # sidebar = 2.5
              # height = 6.3,
              # width = 11.3
              )


################################################################################
# Chart of all mileage for trips for individuals by TC status
################################################################################

# Calculate averages for all trips
wfh_alltraveler_trips_general <-
  wfh_mdt_all %>% 
  # Add non-work trips for everyone who didn't travel today (these are included
  # in the averages as 0 distance)
  left_join(wfh_worktrips_person_level %>% 
              group_by(sampno,perno) %>% 
              summarize(distance_pg = sum(distance_pg)) %>% 
              ungroup(),
            by = c("sampno","perno")) %>% 
  # Mutate NAs for distance for non-travelers to be 0
  replace_na(list(distance_pg = 0)) %>% 
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") %>% # 82772 records
  # Calculate average distances
  group_by(combined_tc_wfh,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            n = n()) %>%
  rename(flag = combined_tc_wfh) %>% 
  ungroup()

# Create chart
wfh_p3 <-
  # Get data
  wfh_alltraveler_trips_general %>%
  # Reformat and reorder
  mutate(flag = recode_factor(factor(flag,levels = c(0,1,2)),
                              "0" = "Does not regularly telecommute or work from home",
                              "1" = "Sometimes telecommutes or works from home (1-3 days/week)",
                              "2" = "Almost always telecommutes or works from home (4+ days/week)"),
         
         geog = recode_factor(geog,
                              "Other suburban counties" = "Other counties",
                              "Suburban Cook" = "Suburban Cook",
                              "Chicago" = "Chicago")) %>%
  
  
  # Create ggplot object
  ggplot(aes(x = distance_pg, y = geog, 
             label = scales::label_number(accuracy = .1)(distance_pg))) +
  geom_col(aes(fill = flag),
           position = position_dodge2(reverse = T,width = 0.9)) +

  # Add labels
  geom_label(aes(label = scales::label_number(accuracy = .1)(distance_pg),
                 group = flag),
             position = position_dodge2(reverse = T,width = 0.9),
             label.size = 0,
             hjust = 0) +
  
  # Adjust axis
  scale_x_continuous(limits = c(0,48)) +
  
  # Add CMAP style
  theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts"),
             vline = 0,legend.max.columns = 1,
             axis.title.x = element_text(hjust = .5),
             strip.text = element_text(hjust = 0.5),
             xlab = "Mean total miles traveled by home jurisdiction",
             ) +
  scale_fill_discrete(type = c("#00becc","#67ac00","#cc5f00"))

# Export finalized graphic
finalize_plot(wfh_p3,
              "On average, part-time telecommuters who live outside Chicago 
              travel the greatest distances every day.",
              "Note: These estimates are based on on answers given in both the
              survey and travel diary components of My Daily Travel. These 
              averages also account for employed individuals who did not travel 
              in the region on their assigned travel day (i.e., individuals who 
              work from home and did not travel outside the home on that day). 
              Those individuals are included as having zero travel distance.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              filename = "wfh_p3",
              mode = "png",
              overwrite = T,
              # height = 4.5,
              # width = 8,
              # sidebar = 2.5
              # height = 6.3,
              # width = 11.3
)

################################################################################
# Backup - summary of non-work chain trips (for prose)
################################################################################

# Calculate the average length of non-work travel by TC/WFH status and home
# jurisdiction
wfh_mdt_all %>% 
  # Add non-work trips for everyone who didn't travel today
  left_join(wfh_worktrips_person_level %>% 
              filter(worktrip == 0) %>% 
              select(sampno,perno,wfh_today,wfo_today,distance_pg),
            by = c("sampno","perno")) %>% 
  # Mutate NAs for distance and wfo/wfh flags for non-travelers to be 0
  replace_na(list(wfh_today = 0, wfo_today = 0,distance_pg = 0)) %>% 
  # Remove travelers with homes in multiple counties
  filter(home_county != "999") %>% # 82772 records
  # Calculate median distances
  group_by(combined_tc_wfh,geog) %>%
  summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
            n = n()) %>%
  rename(flag = combined_tc_wfh) %>% 
  ungroup() %>% 
  arrange(geog)

################################################################################
# Backup - individual trips explored for stories
################################################################################

# Understand the distribution of 1-3 day/week tc/wfh-ers by county

# Count the workers by tc/wfh behavior by home county
wfh_worktrips_person_level %>%
  group_by(combined_tc_wfh,home_county) %>%
  summarize(n = sum(wtperfin)) %>%
  mutate(total = sum(n)) %>%
  mutate(pct = n/total) %>%
  View()

# Create table for exporting location and tc/wfh behavior
tc_homes <-
  wfh_worktrips_person_level %>%
  select(combined_tc_wfh,home_lat,home_long,home_county,home_tract,income_c)

# Export for analysis in a GIS software
write.csv(tc_homes,"tc_homes.csv")

tc_od <-
  wfh_worktrips_person_level %>%
  filter(combined_tc_wfh == 1,
         home_county != 31,
         worktrip == 1)

# Identify the work trip chains closest to the mean length for 1-3 day
# tc/wfh-ers that live outside of Cook County
tc_od_average <-
  tc_od %>% 
  mutate(average = weighted.mean(distance_pg,wtperfin)) %>% 
  mutate(variance = distance_pg / average) %>% 
  filter(variance > .99 & variance < 1.01) %>% 
  mutate(id = paste(sampno,perno,sep = "_"))

# Extract the work trips with lat/long for locations
df <-
  wfh_worktrips_relevant %>% 
  mutate(id = paste(sampno,perno,sep = "_")) %>% 
  filter(id %in% tc_od_average$id)
  
# Export to csv for mapping
write.csv(df,"average_suburban_tc13_trips.csv")

################################################################################
#
# ARCHIVE
################################################################################
# 
# ################################################################################
# # Plot trip locations
# ################################################################################
# # Plot points in R (less useful without ability to select points)
# 
# chicago_map <- get_map("chicago, illinois", zoom = 9)
# 
# ggmap(chicago_map) +
#   geom_point(data = df, aes(x = longitude, y = latitude, color = id))
# 
# # Map the 1-3 day/week tc/wfh-ers by tract
# tract_sf <- sf::read_sf("V:/Demographic_and_Forecast/Census/2010/Geography/CMAP_Region_Projected/Tracts_CMAP_TIGER2010.shp")
# 
# tract_wfh_count <-
#   wfh_worktrips_person_level %>%
#   group_by(home_county,home_tract) %>%
#   summarize(n = n(),
#             weighted_n = sum(wtperfin))
# 
# tract_tc13 <-
#   tract_sf %>%
#   mutate(TRACTCE10 = as.integer(TRACTCE10),
#          COUNTYFP10 = as.integer(COUNTYFP10)) %>%
#   left_join(tract_wfh_count, by = c("TRACTCE10" = "home_tract",
#                                          "COUNTYFP10" = "home_county")) %>%
#   select(TRACTCE10,COUNTYFP10,n,weighted_n,geometry)
# 
# 
# wfh_tracts_map1 <-
#   tract_tc13 %>%
#   ggplot(aes(fill = weighted_n)) +
#   geom_sf() +
#   theme_cmap(axis.text = element_blank(),
#              legend.position = "right",
#              legend.direction = "vertical",
#              legend.key.height = grid::unit(20,"bigpts")) +
#   cmap_fill_continuous(palette = "seq_blues")
# 
# 
# finalize_plot(wfh_tracts_map1,
#               title = "Number of tc/wfh 1-3 days/week per tract (weighted).",
#               caption = "Source: CMAP analysis of MDT data.",
#               sidebar_width = 0,
#               legend_shift = FALSE,
#               filename = "wfh_tracts_map1",
#               # mode = "png",
#               overwrite = T
# )
# 
# map <- get_map("Chicago, Illinois")
# 
# coords_map <-
#   ggplot() +
#   geom_point(data = wfh_worktrips_person_level %>% filter(combined_tc_wfh == 1) %>% filter(home_county %in% cmap_seven_counties),
#              aes(x = home_long, y = home_lat),
#              fill = "red",
#              shape=23,
#              alpha = 0.8)
#   # geom_sf(data = tract_sf)
# 
# coords_map
# 
# 
# ################################################################################
# #
# # WFH/TC ALL TRIPS
# ################################################################################
# 
# ################################################################################
# # Create chart with travel characteristics for those who report working from
# # home on days when they do and do not work from home
# ################################################################################
# 
# wfh_with_today_status <-
#   wfh_person_level_plus_no_trips %>%
#   mutate(
#     category = case_when(
#       wfh_today == 1 & wfo_today == 1 ~ 1,
#       wfh_today == 1 ~ 2,
#       wfo_today == 1 ~ 3,
#       TRUE ~ 4),
#     wfh_andor_tc = case_when(
#       wfh == 1 ~ 1,
#       tc == 1 ~ 1,
#       TRUE ~ 0)) %>%
#   group_by(category,wfh_andor_tc) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "tc_with_today_status")
# 
# ################################################################################
# # Chart of trip distances for individuals depending on their WFH/TC status and
# # today's travel behavior
# ################################################################################
# 
# wfh_p1 <-
#   wfh_with_today_status %>%
#   filter(name != "travtime_pg") %>%
#   mutate(flag = recode_factor(wfh_andor_tc,
#                               "1" = "Telecommute at least one day a week and/or work from home",
#                               "0" = "Do not regularly telecommute or work from home"),
#          xmax = case_when(
#            name == "distance_pg" ~ 45,
#            name == "pertrips" ~ 5.95
#          ),
#          name = recode_factor(name,
#                               "distance_pg" = "Distance (mi.)",
#                               "pertrips" = "Trips")) %>%
#   mutate(category = recode_factor(category,
#                                   "2" = "Only worked at \nhome today",
#                                   "4" = "Did not work today",
#                                   "1" = "Worked both from \nhome and outside \nthe home today",
#                                   "3" = "Only worked outside \nthe home today")) %>%
#   mutate(category = factor(category, levels = c("Only worked at \nhome today",
#                                                 "Did not work today",
#                                                 "Worked both from \nhome and outside \nthe home today",
#                                                 "Only worked outside \nthe home today"))) %>%
#   filter(name == "Distance (mi.)") %>%
#   ggplot(aes(x = value, y = category, fill = flag)) +
#   geom_col(position = position_dodge2(reverse = T)) +
#   # facet_wrap(~name,scales = "free_x") +
#   theme_cmap(gridlines = "v",legend.max.columns = 1,
#              vline = 0, xlab = "Distance (mi.)") +
#   geom_label(aes(label = scales::label_number(accuracy = 0.1)(value),
#                  group = flag),
#              position = position_dodge2(reverse = T, width = 0.9),
#              label.size = 0,
#              hjust = 0,
#              fill = "white") +
#   geom_blank(aes(x = xmax)) +
#   cmap_fill_discrete(palette = "legislation")
# 
# finalize_plot(wfh_p1,
#               "Travel characteristics individuals who telecommute or work from
#               home vs. those who do not, on days when they are and are not
#               working from home.",
#               "Note: These estimates are based on on answers given in
#               both the survey and travel diary components of MDT.
#               <br><br>
#               Source: CMAP analysis of MDT data.",
#               title_width = 2.5,
#               width = 11.3,
#               height = 6.3,
#               filename = "wfh_p1",
#               mode = "png",
#               overwrite = T)
# 
# ################################################################################
# # Boxplot of work trips
# ################################################################################
# 
# wfh_p2_bp <-
#   wfh_worktrips_person_level %>%
#   mutate(flag = recode_factor(factor(combined_tc_wfh),
#                               "2" = "Almost always telecommutes or works from home (4+ days/week)",
#                               "1" = "Sometimes telecommutes or works from home (1-3 days/week)",
#                               "0" = "Does not telecommute or work from home"),
#          worktrip = recode_factor(factor(worktrip),
#                                   "1" = "Work trips outside the home",
#                                   "0" = "Other trips"),
#          cook_county = recode_factor(factor(cook_county),
#                                      "1" = "Cook County residents",
#                                      "0" = "Residents of other counties")
#   ) %>%
#   ggplot(aes(x = distance_pg, y = str_wrap_factor(flag,20))) +
#   geom_boxplot() +
#   facet_wrap(worktrip~cook_county) +
#   theme_cmap(gridlines = "v",
#              legend.position = "none")
# 
# wfh_p2_bp
# 
# ################################################################################
# # Miscellaneous analysis
# ################################################################################
# 
# # Create averages comparing those who report telecommuting vs. not
# wfh_general <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(tc) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "tc_general") %>%
#   rename(flag = tc)
# 
# # Create averages comparing those who worked from home today vs. those who did not
# wfh_today <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(wfh_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_today") %>%
#   rename(flag = wfh_today)
# 
# # Create averages comparing those who worked from outside the home today vs. not
# wfo_today <-
#   wfh_person_level_plus_no_trips %>%
#   group_by(wfo_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfo_today") %>%
#   rename(flag = wfo_today)
# 
# # Create averages comparing those who worked from outside the home today vs.
# # not, including only individuals with at least one work-related trip
# wfh_vs_wfo <-
#   wfh_person_level_plus_no_trips %>%
#   filter(wfh_today == 1 | wfo_today == 1) %>%
#   group_by(wfh_today) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin),
#             n = n()) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_vs_wfo") %>%
#   rename(flag = wfh_today)
# 
# # Combine into one dataset
# wfh_chart_inputs <-
#   rbind(wfh_general,
#         wfh_today,
#         wfo_today,
#         wfh_vs_wfo)
# 
# # Chart of travel characteristics and work from home behavior
# wfh_p3 <-
#   wfh_chart_inputs %>%
#   mutate(flag = recode(flag,
#                        "0" = "No",
#                        "1" = "Yes"),
#          type = recode_factor(type,
#                               "wfo_today" = "Worked outside of home (today)",
#                               "wfh_today" = "Worked from home (today)",
#                               "tc_general" = "Work from home (generally)"),
#          name = recode_factor(name,
#                               "distance_pg" = "Distance (mi.)",
#                               "pertrips" = "Trips",
#                               "travtime_pg" = "Travel time (min.)")) %>%
#   filter(type != "wfh_vs_wfo") %>%
#   ggplot(aes(x = value, y = type, fill = flag)) +
#   geom_col(position = position_dodge2()) +
#   facet_wrap(~name,scales = "free_x") +
#   theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
#   cmap_fill_discrete(palette = "legislation") +
#   scale_y_discrete(labels = c("Worked outside\nof home (today)",
#                               "Worked from\nhome (today)",
#                               "Worked from\nhome (survey)"))
# 
# finalize_plot(wfh_p3,
#               "Travel characteristics and work from home behavior.",
#               "Note: \"Worked from home (survey)\" is based on answers given in
#               the survey component of MDT. The other two categories are based on
#               observed trip behavior recorded in the travel diaries.
#               <br><br>
#               Source: CMAP analysis of MDT data.",
#               title_width = 2)
# 
# #### REPEAT BUT COUNTY-LEVEL
# 
# # Create averages comparing those who report working from home vs. not
# wfh_general_counties <-
#   wfh_person_level_plus_no_trips %>%
#   mutate(cook = case_when(
#     home_county == "31" ~ 1,
#     TRUE ~ 0
#   )) %>%
#   # Group by both tc status and Cook county status
#   group_by(tc,cook) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "tc_general") %>%
#   rename(flag = tc)
# 
# # Repeat for those who worked from home today vs. those who did not
# wfh_today_counties <-
#   wfh_person_level_plus_no_trips %>%
#   mutate(cook = case_when(
#     home_county == "31" ~ 1,
#     TRUE ~ 0
#   )) %>%
#   group_by(wfh_today,cook) %>%
#   summarize(distance_pg = weighted.mean(distance_pg, w = wtperfin),
#             pertrips = weighted.mean(pertrips, w = wtperfin),
#             travtime_pg = weighted.mean(travtime_pg, w = wtperfin)) %>%
#   pivot_longer(cols = distance_pg:travtime_pg) %>%
#   mutate(type = "wfh_today") %>%
#   rename(flag = wfh_today)
# 
# # Combine data
# wfh_chart_counties_inputs <-
#   rbind(wfh_general_counties,
#         wfh_today_counties)
# 
# # Chart of travel characteristics and work from home behavior by home location
# wfh_p4 <-
#   wfh_chart_counties_inputs %>%
#   mutate(flag = recode(flag,
#                        "0" = "Do not telecommute",
#                        "1" = "Telecommute at least one day a week"),
#          type = recode_factor(type,
#                               "wfh_today" = "Worked from home (today)",
#                               "tc_general" = "Work from home (generally)"),
#          name = recode_factor(name,
#                               "distance_pg" = "Distance (mi.)",
#                               "pertrips" = "Trips",
#                               "travtime_pg" = "Travel time (min.)"),
#          cook = recode_factor(cook,
#                               "0" = "Rest of the region",
#                               "1" = "Cook County")) %>%
#   filter(type == "Work from home (generally)") %>%
#   ggplot(aes(x = value, y = cook, fill = flag)) +
#   geom_col(position = position_dodge2()) +
#   facet_wrap(~name,scales = "free_x") +
#   theme_cmap(gridlines = "v",panel.spacing = unit(20,"bigpts")) +
#   cmap_fill_discrete(palette = "legislation")
# 
# finalize_plot(wfh_p4,
#               "Travel characteristics and work from home behavior by home location.",
#               "Note: These estimates are based on on answers given in
#               the survey component of MDT.
#               <br><br>
#               Source: CMAP analysis of MDT data.",
#               title_width = 2)