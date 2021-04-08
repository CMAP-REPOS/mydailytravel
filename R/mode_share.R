# This file produces overall charts on mode share in the CMAP region

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

library(ggplot2)
library(tidyverse)
library(cmapplot)
library(ggpattern)

#################################################
#                                               #
#                 Data Prep                     #
#                                               #
#################################################

setwd("~/GitHub/mydailytravel")
source("R/helper_fns.R")
source("R/data_cleaning.R")


# Create base dataset for mode analyses

mdt_base_3 <-
  mdt %>%                              # 125463 records
  # Keep only travelers >= 16 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 16 |                   # 108622
           (age < 0 & aage %in% c(4,5,6,7)) |
           (age < 0 & schol %in% c(5,6,7,8))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%    # 85022
  # Exclude trips with zero distance
  filter(distance_pg > 0) %>%          # 84969
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%      # 84932
  # Exlclude trips missing from residents outside the seven counties
  filter(home_state == 17 &            # 82732
           home_county %in% cmap_seven_counties) %>% 
  # Put school bus back into "other" category
  mutate(mode_c = as.character(mode_c)) %>%
  mutate(mode_c = case_when(
    mode_c == "schoolbus" ~ "other",
    TRUE ~ mode_c)) %>%
  mutate(mode_c = factor(mode_c,levels = mode_c_levels))

# # Note: The TT code is included but was not used in published analyses. We
# # decided to exclude this comparison because the overall mode shares between TT
# # and MDT are not "apples to apples" - in particular, the overall transit mode
# # share in MDT was weighted to approximate known transit ridership (from RTA
# # statistics). Since this was not done in TT, the transit mode share (and thus
# # the overall mode shares) are not directly comparable.
# tt_base_3 <-
#   tt %>%                               # 139769 records
#   # Keep only travelers >= 16 years old, either through age, age bucket, or
#   # school enrollment. Note that 99 is DK/RF for AGE.
#   filter((AGE >= 16 & AGE < 99) |                   # 117605
#            (AGE == 99 & SCHOL %in% c(5,6,7,8)) |
#            (AGE == 99 & AGEB == 2)) %>%
#   # Keep only trips with nonzero distance
#   filter(DIST > 0) %>%                 # 89463
#   # Exclude missing modes
#   filter(mode_c != "missing") %>%      # 89463
#   # Put school bus back into "other" category
#   mutate(mode_c = as.character(mode_c)) %>%
#   mutate(mode_c = case_when(
#     mode_c == "schoolbus" ~ "other",
#     TRUE ~ mode_c)) %>%
#   mutate(mode_c = factor(mode_c,levels = mode_c_levels))

#################################################
#                                               #
#                  Analysis                     #
#                                               #
#################################################

################################################################################
#
# MDT-only analyses
################################################################################

# Create baseline totals for percentage calculations
mdt_mode_all <-
  pct_calculator(mdt_base_3,
                 breakdown_by = "mode_c",
                 weight = "wtperfin")

# Analyze percents at the county and region-wide level
mdt_mode_counties <-
  pct_calculator(
    mdt_base_3,
    breakdown_by = "mode_c",
    second_breakdown = "home_county_chi",
    weight = "wtperfin") %>% 
  # Add the regional total calculated above
  rbind(mdt_mode_all %>% mutate(home_county_chi = "CMAP region"))

# Analyze percents by household income
mdt_mode_income <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mdt_base_3 %>% filter(hhinc > 0),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "hhinc",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(hhinc = 99)) %>% 
  # Recode for publication
  mutate(hhinc = recode_factor(factor(hhinc),
                             "10" = "$150,000 or more",
                             "9" = "$100,000 to $149,999",
                             "8" = "$75,000 to $99,999",
                             "7" = "$60,000 to $74,999",
                             "6" = "$50,000 to $59,999",
                             "5" = "$35,000 to $49,999",
                             "4" = "$30,000 to $34,999",
                             "3" = "$25,000 to $29,999",
                             "2" = "$15,000 to $24,999",
                             "1" = "Less than $15,000",
                             "99" = "CMAP region"))

# Do the same again, but for race and ethnicity
mdt_mode_race <-
  pct_calculator(
    # Keep all respondents with a reported race and ethnicity...
    mdt_base_3 %>% filter(race_eth != "missing"),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "race_eth",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(race_eth = "CMAP region")) %>% 
  # Recode for publication
  mutate(race_eth = recode_factor(factor(race_eth),
                                  "other" = "Other",
                                  "black" = "Black",
                                  "asian" = "Asian",
                                  "hispanic" = "Hispanic",
                                  "white" = "White",
                                  "CMAP region" = "CMAP region"))

# Do the same again, but for age

# Age bins
age_breaks <- c(-1,29, 49, 69, 150)
age_labels <- c("16 to 29", "30 to 49",  "50 to 69", "70 and above")

mdt_mode_age <-
  pct_calculator(
    # Add age bins
    mdt_base_3 %>% 
      filter(age > 0) %>% 
      mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "age_bin",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(age_bin = "CMAP region")) %>% 
  # Reorder factors for publication
  mutate(age_bin = fct_relevel(age_bin,"CMAP region")) %>%
  mutate(age_bin = fct_rev(factor(age_bin)))



# Do the same again, but for mileage

# Mileage bins
mileage_breaks <- c(-1,.5,1,2.5,5,25,100)
mileage_labels <- c("0.50 miles or less","0.51 to 1 mile", "1.01 to 2.50 miles", 
                 "2.51 to 5.00 miles","5.01 to 25.00 miles","More than 25 miles")

mdt_mode_mileage <-
  pct_calculator(
    # Add mileage bins
    mdt_base_3 %>% 
      mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks,labels=mileage_labels)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "mileage_bin",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(mileage_bin = "CMAP region")) %>% 
  # Reorder factors for publication
  mutate(mileage_bin = fct_relevel(mileage_bin,"CMAP region")) %>%
  mutate(mileage_bin = fct_rev(factor(mileage_bin)))

# Do the same for sex
mdt_mode_sex <-
  pct_calculator(
    mdt_base_3,
    breakdown_by = "mode_c",
    second_breakdown = "sex",
    weight = "wtperfin") %>% 
  # Remove DK/RF
  filter(sex > 0) %>%
  # Recode sex
  mutate(sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female")) %>% 
  # Add the regional total calculated above
  rbind(mdt_mode_all %>% mutate(sex = "CMAP region"))


################################################################################
# Chart of mode share by home county
################################################################################

# Create labels
mode_share_p1_labels <-
  mdt_mode_counties %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(home_county_chi) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p1 <-
  # Get data
  mdt_mode_counties %>%
  # Add labels
  left_join(mode_share_p1_labels, by = "home_county_chi") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct),
    # Add flag for CMAP region for pattern in display
    type = ifelse(home_county_chi == "CMAP region","1","0")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(home_county_chi,label))) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
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
  
  geom_label(data = mode_share_p1_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = home_county_chi),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, xlab = "Mode share by home jurisdiction") +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.61)) +

  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 6,override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p1,
              title = "Residents of Chicago and Cook County have by far the highest non-car 
              mode share in the CMAP region.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 4.5,
              # width = 8,
              filename = "mode_share_p1",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by income
################################################################################

# Create labels
mode_share_p2_labels <-
  mdt_mode_income %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(hhinc) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p2 <-
  # Get data
  mdt_mode_income %>%
  # Add labels
  left_join(mode_share_p2_labels, by = "hhinc") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct),
    # Add flag for CMAP region for pattern in display
    type = ifelse(hhinc == "CMAP region","1","0")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = hhinc)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
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
  geom_label(data = mode_share_p2_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = hhinc),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,xlab = "Mode share by household income") +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.68))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 6,override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p2,
              title = "The lowest-income households rely the most on non-car 
              modes, but high-income households also exceed the regional 
              average.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 4.5,
              # width = 8,
              filename = "mode_share_p2",
              mode = "png",
              overwrite = T)

################################################################################
# Backup - detailed mode by income
################################################################################

# Analyze percents by household income
mdt_mode_income_detailed <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mdt_base_3 %>% filter(hhinc > 0),
    # Execute the rest of the function
    breakdown_by = "mode",
    second_breakdown = "hhinc",
    weight = "wtperfin") %>% 
  mutate(hhinc_c = recode_factor(factor(hhinc),
                               "10" = "$150,000 or more",
                               "9" = "$100,000 to $149,999",
                               "8" = "$75,000 to $99,999",
                               "7" = "$60,000 to $74,999",
                               "6" = "$50,000 to $59,999",
                               "5" = "$35,000 to $49,999",
                               "4" = "$30,000 to $34,999",
                               "3" = "$25,000 to $29,999",
                               "2" = "$15,000 to $24,999",
                               "1" = "Less than $15,000")) %>%
  mutate(pct = round(pct,2)) %>% 
  arrange(mode,hhinc)

################################################################################
# Chart of mode share by race
################################################################################

# Create labels
mode_share_p3_labels <-
  mdt_mode_race %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(race_eth) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p3 <-
  # Get data
  mdt_mode_race %>%
  # Add labels
  left_join(mode_share_p3_labels, by = "race_eth") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct),
    # Add flag for CMAP region for pattern in display
    type = ifelse(race_eth == "CMAP region","1","0")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = reorder(str_wrap(race_eth,18),label))) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
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
  geom_label(data = mode_share_p3_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = race_eth),
             label.size = 0,
             hjust = 0,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,xlab = "Mode share by race and ethnicity") +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.55))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 6,override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p3,
              title = "White residents of the region are the likeliest to rely 
              on personal automobiles for their transportation.",
              caption = "Note: 'Hispanic' includes respondents who identified as 
              Hispanic of any racial category. Other categories are non-Hispanic. 
              Includes trips by residents of the region that  start and/or end 
              in the Illinois counties of Cook, DeKalb, DuPage, Grundy, Kane, 
              Kendall, Lake, McHenry, and Will. Excludes trips by travelers 
              younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p3",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by age
################################################################################

# Create labels
mode_share_p4_labels <-
  mdt_mode_age %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(age_bin) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p4 <-
  # Get data
  mdt_mode_age %>%
  # Add labels
  left_join(mode_share_p4_labels, by = "age_bin") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct),
    # Add flag for CMAP region for pattern in display
    type = ifelse(age_bin == "CMAP region","1","0")) %>%
  
    # Create ggplot object
  ggplot(aes(x = pct, y = age_bin)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
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
  geom_label(data = mode_share_p4_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = age_bin),
             label.size = 0,
             hjust = 0,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,
             xlab = "Mode share by age") +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.55))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 6,override.aes = list(pattern = "none")))

finalize_plot(mode_share_p4,
              title = "The region's younger travelers disproportionately rely on 
              non-car modes.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p4",
              mode = "png",
              overwrite = T)

################################################################################
# Chart of mode share by mileage
################################################################################

# Create labels
mode_share_p5_labels <-
  mdt_mode_mileage %>%
  filter(mode_c %in% c("walk","transit","bike","other")) %>%
  group_by(mileage_bin) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p5 <-
  # Get data
  mdt_mode_mileage %>%
  # Add labels
  left_join(mode_share_p5_labels, by = "mileage_bin") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","other")),
                           "driver" = "Driver",
                           "passenger" = "Passenger",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "other" = "Other"),
    # Make driver/passenger go on the left-hand-side of the graph
    pct = ifelse(mode_c %in% c("Driver","Passenger"),-1 *pct,pct),
    # Add flag for CMAP region for pattern in display
    type = ifelse(mileage_bin == "CMAP region","1","0")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = mileage_bin)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
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
  geom_label(data = mode_share_p5_labels,
             aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = mileage_bin),
             label.size = 0,
             hjust = 0,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  cmap_fill_discrete(palette = "mobility") +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.75,by = .25))),
                     limits = c(-1,.83))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 6,override.aes = list(pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p5,
              title = "Travelers rely most on non-car modes for the shortest and 
              the longest trips.",
              caption = "Note: Includes trips by residents of the region that 
              start and/or end in the Illinois counties of Cook, DeKalb, DuPage, 
              Grundy, Kane, Kendall, Lake, McHenry, and Will. Excludes trips by 
              travelers younger than 16.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data.",
              # # height = 5.5,
              # width = 11.3,
              filename = "mode_share_p5",
              mode = "png",
              overwrite = T)

################################################################################
# 
# ARCHIVE
################################################################################
# 
# ################################################################################
# # Archive - Chart of overall mode share
# ################################################################################
# 
# mode_share_p2 <-
#   mdt_mode_all %>%
#   mutate(mode_c = factor(mode_c,levels = c("driver","passenger","walk",
#                                            "transit","bike","other")),
#          survey = "mdt") %>%
# 
#   ggplot(aes(y = mdt_share, x = survey, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_y_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p2,
#               title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive).
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p2",
#               # mode = "png",
#               overwrite = T)
# 
# ################################################################################
# #
# # ARCHIVE - Comparisons of MDT and TT (note data problems)
# ################################################################################
# 
# tt_mode_all <-
#   tt_base_3 %>%
#   mutate(total = sum(weight)) %>%
#   group_by(mode_c) %>%
#   summarize(count = sum(weight),
#             tt_share = round((count/median(total))*100, digits = 2)) %>%
#   select(mode_c, tt_share, count)
#
# mode_all <- tt_mode_all %>%
#   select(-count) %>%
#   left_join(mdt_mode_all, by = "mode_c") %>%
#   select(-count) %>%
#   pivot_longer(cols = c("tt_share":"mdt_share"))
#
# mdt_mode_dist_all <-
#   mdt_base_3 %>%
#   mutate(total = sum(wthhfin*hdist_pg)) %>%
#   group_by(mode_c) %>%
#   summarise(distance = sum(wthhfin*hdist_pg),
#             mdt_share = round((distance/median(total))*100, digits = 2)) %>%
#   select(mode_c, mdt_share, distance)
#
# tt_mode_dist_all <-
#   tt_base_3 %>%
#   mutate(total = sum(weight*DIST)) %>%
#   group_by(mode_c) %>%
#   summarize(distance = sum(weight*DIST),
#             tt_share = round((distance/median(total))*100, digits = 2)) %>%
#   select(mode_c, tt_share, distance)
#
# mode_dist_all <- tt_mode_dist_all %>%
#   select(-distance) %>%
#   left_join(mdt_mode_dist_all, by = "mode_c") %>%
#   select(-distance) %>%
#   pivot_longer(cols = c("tt_share":"mdt_share"))
# 
# 
# ################################################################################
# # ARCHIVE - Chart of mode share, MDT vs. TT
# ################################################################################
# 
# mode_share_p3 <-
#   mode_all %>%
#   mutate(name = recode_factor(name,
#                               tt_share = "Travel Tracker ('08)",
#                               mdt_share = "My Daily Travel ('19)"),
#          mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
#   mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
#   ggplot(aes(x = value, y = name, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p3,
#               title = "Mode share for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive).
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p3",
#               mode = "png",
#               overwrite = T)
# 
# ################################################################################
# # ARCHIVE - Chart of travel distance, MDT vs. TT
# ################################################################################
# 
# mode_share_p4 <-
#   mode_dist_all %>%
#   mutate(name = recode_factor(name,
#                               tt_share = "Travel Tracker ('08)",
#                               mdt_share = "My Daily Travel ('19)"),
#          mode_c = factor(mode_c,levels = c("driver","passenger","walk","transit","bike","other"))) %>%
#   mutate(value = if_else(mode_c %in% c("driver","passenger"), value * -1, value)) %>%
#   ggplot(aes(x = value, y = name, fill = mode_c)) +
#   geom_col(position = position_stack(reverse = T)) +
#   theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 10) +
#   cmap_fill_discrete(palette = "mobility") +
#   scale_x_continuous(n.breaks = 6, labels = scales::label_percent(scale = 1))
#
# finalize_plot(mode_share_p4,
#               title = "Share of travel distance for trips in the CMAP region, 2008 vs. 2019.",
#               caption = "Note: Includes all trips in the CMAP region made by
#               travelers from ages 16 to 89 (inclusive). Distances are \"as the crow files.\"
#               <br><br>
#               Source: Chicago Metropolitan Agency for Planning analysis of My
#               Daily Travel and Travel Tracker data.",
#               height = 5.5,
#               width = 11.3,
#               filename = "mode_share_p4",
#               mode = "png",
#               overwrite = T)

