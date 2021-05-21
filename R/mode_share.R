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

mode_share_base_mdt <-
  mdt %>%                              # 125463 records
  # Keep only travelers >= 5 years old, either through age, age bucket, or
  # school enrollment
  filter(age >= 5 |                   # 125447
           (age < 0 & aage %in% c(2,3,4,5,6,7)) |
           (age < 0 & schol %in% c(4,5,6,7,8))) %>%
  # Exclude "beginning" trips
  filter(mode_c != "beginning") %>%   # 97635
  # Exclude trips with no travel distance. Note this is a different difference
  # calculation than that used in TT (great circle vs. actual travel distance).
  # We chose to do this since the published graphics do not involve any
  # comparison between TT and MDT. However, if we instead filter out those trips
  # that have a nonzero haversine distance from MDT, the results are similar.
  filter(distance_pg > 0) %>%         # 97307
  # Exclude trips with a "missing" mode
  filter(mode_c != "missing") %>%     # 97270
  # Exclude improbable walk trips
  filter(improbable_walk == 0)        # 97230

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
  pct_calculator(mode_share_base_mdt,
                 breakdown_by = "mode_c",
                 weight = "wtperfin") %>% 
  ungroup()

## Export overall mode share for prose
mdt_mode_all %>% select(mode_c,pct)



# Analyze percents at the county and region-wide level
mdt_mode_counties <-
  pct_calculator(
    mode_share_base_mdt %>% 
      # For percentage calculations (for display) only calculate the seven
      # counties. This excludes DeKalb, Grundy, and the household that has a
      # home in both Chicago and Suburban Cook.
      filter(home_county %in% cmap_seven_counties),
    breakdown_by = "mode_c",
    second_breakdown = "home_county_chi",
    weight = "wtperfin") %>% 
  # Add the regional total calculated above
  rbind(mdt_mode_all %>% mutate(home_county_chi = "CMAP region")) %>% 
  ungroup()

# Analyze percents by household income
mdt_mode_income <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0),
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
                             "99" = "CMAP region")) %>% 
  ungroup()

# Do the same again, but for race and ethnicity
mdt_mode_race <-
  pct_calculator(
    # Keep all respondents with a reported race and ethnicity...
    mode_share_base_mdt %>% filter(race_eth != "missing"),
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
                                  "latino" = "Latino",
                                  "white" = "White",
                                  "CMAP region" = "CMAP region")) %>% 
  ungroup()

# Do the same again, but for age

# Age bins
age_breaks <- c(-1,17,29, 49, 69, 150)
age_labels <- c("5 to 17","18 to 29", "30 to 49",  "50 to 69", "70 and above")

mdt_mode_age <-
  pct_calculator(
    # Add age bins
    mode_share_base_mdt %>% 
      filter(age > 0) %>% 
      mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "age_bin",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(age_bin = "CMAP region")) %>% 
  # Reorder factors for publication
  mutate(age_bin = factor(age_bin,levels = c("70 and above",
                                             "50 to 69",
                                             "30 to 49",
                                             "18 to 29",
                                             "5 to 17",
                                             "CMAP region"))) %>%
  ungroup()



# Do the same again, but for mileage

# Mileage bins
mileage_breaks <- c(-1,.5,1,2.5,5,25,100)
mileage_labels <- c("0.50 miles or less","0.51 to 1.0 miles", "1.01 to 2.50 miles", 
                 "2.51 to 5.00 miles","5.01 to 25.00 miles","More than 25 miles")


mdt_mode_mileage <-
  pct_calculator(
    # Add mileage bins
    mode_share_base_mdt %>% 
      mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks,labels=mileage_labels)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "mileage_bin",
    weight = "wtperfin") %>%
  # Add baseline totals
  rbind(mdt_mode_all %>% mutate(mileage_bin = "CMAP region")) %>% 
  # Reorder factors for publication
  mutate(mileage_bin = fct_relevel(mileage_bin,"CMAP region")) %>%
  mutate(mileage_bin = fct_rev(factor(mileage_bin))) %>% 
  ungroup()



# Do the same for sex
mdt_mode_sex <-
  pct_calculator(
    mode_share_base_mdt,
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
  rbind(mdt_mode_all %>% mutate(sex = "CMAP region")) %>% 
  ungroup()


################################################################################
# Chart of mode share by home county
################################################################################

# NOTE: County-level estimates should be treated with some caution for outer
# region counties. The survey's weighting was developed based on 11 travel zones
# (documented in Technical Memo 6.1), some of which line up with county
# boundaries but some of which do not. Chicago, Cook, and Will Counties have one
# or more zones that primarily make them up (although a small portion of NW Cook
# County is included in an outer-region zone). Lake County is entirely contained
# within another zone. However, other regional counties have zones that span
# across them. This is particularly relevant for Kane, Kendall, and McHenry
# Counties, as well as Grundy and DeKalb (which are only partially covered). As
# a result, minor differences between overlapping zones, e.g., Kane and Kendall
# County, are unlikely to be significant.

# Create labels
mode_share_p1_labels <-
  mdt_mode_counties %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
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
                                      "transit","bike","schoolbus","other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
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
             hjust = -.02,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, xlab = "Mode share by home jurisdiction") +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +

  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.61)) +

  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac"),
                                                 pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p1,
              title = "Residents of Chicago and Cook County had by far the highest non-car 
              mode share in the CMAP region.",
              caption = 
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              Residents of Grundy and DeKalb counties are not shown but are 
              included in regional averages.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              McHenry County has the lowest sample size, with ",
                     format(mdt_mode_counties %>% 
                              filter(home_county_chi == "McHenry") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p1",
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chart of mode share by mileage
################################################################################

# Create labels
mode_share_p2_labels <-
  mdt_mode_mileage %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(mileage_bin) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p2 <-
  # Get data
  mdt_mode_mileage %>%
  # Add labels
  left_join(mode_share_p2_labels, by = "mileage_bin") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus",
                                      "other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
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
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = mileage_bin),
             label.size = 0,
             hjust = -.02,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6) +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.75,by = .25))),
                     limits = c(-1,.83))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac"),
                                                 pattern = "none")))

# Export finalized graphic
finalize_plot(mode_share_p2,
              title = "Travelers relied most on non-car modes for the shortest and 
              the longest trips.",
              caption = 
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              Distances are 'network distances' and 
              capture the total distance traveled along the route, not just the 
              distance from origin to destination.
              <br><br>
              Sample size: Figures are based on a total of ",
                       format(nrow(mode_share_base_mdt),big.mark = ","),
                       " recorded trips.
              Trips of 25 miles or more have the lowest sample size, with ",
                       format(mdt_mode_mileage %>% 
                                filter(mileage_bin == "More than 25 miles") %>% 
                                select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                       " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p2",
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Chart of mode share by mileage (more visual)
################################################################################

# Mileage bins
mileage_breaks2 <- c(-1,.25,.5,1,2.5,5,10,25,50,100)
mileage_labels2 <- c("<0.25","0.25 to 0.5","0.5 to 1","1 to 2.5","2.5 to 5",
                     "5 to 10","10 to 25","25 to 50","50 to 100")

mdt_mode_mileage2 <-
  pct_calculator(
    # Add mileage bins
    mode_share_base_mdt %>% 
      mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks2,labels=mileage_labels2)) %>% 
      filter(!is.na(mileage_bin)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "mileage_bin",
    weight = "wtperfin") %>%
  mutate(mileage_bin = factor(mileage_bin)) %>% 
  ungroup()

# Create labels
mode_share_p2a_labels <-
  mdt_mode_mileage2 %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(mileage_bin) %>%
  summarize(label = sum(pct))



# Create plot
mode_share_p2a <-
  # Get data
  mdt_mode_mileage2 %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus",
                                      "other")),
                           "driver" = "By car",
                           "passenger" = "By car",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
                           "other" = "Other")) %>% 
  group_by(mode_c,mileage_bin) %>% 
  summarize(pct = sum(pct)) %>% 
  mutate(pct = ifelse(mode_c == "By car",-1*pct,pct)) %>% 
  # Join labels
  left_join(mode_share_p2a_labels,by = c("mileage_bin")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = mileage_bin,y = pct,
         # Only label bars that are at least 5 percent
         label = ifelse(abs(pct) >=.05,scales::label_percent(accuracy = 1)(abs(pct)),""))) +
  geom_col(aes(fill = mode_c),width = 11,position = position_stack(reverse = T)) +
  geom_text(aes(group = mode_c),
            position = position_stack(vjust = 0.5,reverse = T),
            color = "white") +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(label),y = label),
             vjust = -.04,label.size = 0,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "none",hline = 0, legend.max.columns = 7,
             axis.text.y = element_blank(),
             xlab = "Mileage traveled from origin to destination") +
  # Add colors
  scale_fill_discrete(type = c("#00665c","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac")) +
  
  # Adjust axis
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     breaks = c(0,1)
                     ) +
  
  scale_x_discrete(limits = c("<0.25",
                              rep("",13),
                              "0.25 to 0.5",
                              rep("",14),
                              "0.5 to 1",
                              rep("",15),
                              "1 to 2.5",
                              rep("",16),
                              "2.5 to 5",
                              rep("",17),
                              "5 to 10",
                              rep("",18),
                              "10 to 25",
                              rep("",19),
                              "25 to 50",
                              rep("",20),
                              "50 to 100"))
  
  #+
  
  # scale_x_discrete(limits = c("",mileage_labels2)) +
  #
  # coord_polar("y")

# mode_share_p2a

# Export finalized graphic
finalize_plot(mode_share_p2a,
              title = "Travelers relied most on non-car modes for the shortest trips.",
              caption = 
                paste0("<br>Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              Distances are 'network distances' and 
              capture the total distance traveled along the route, not just the 
              distance from origin to destination. Unlabeled bars have less than 
              5 percent mode share.
              <br><br>
              Sample size: Figures are based on a total of ",
                       format(nrow(mode_share_base_mdt),big.mark = ","),
                       " recorded trips.
              Trips of 50 to 100 miles have the lowest sample size, with ",
                       format(mdt_mode_mileage2 %>% 
                                filter(mileage_bin == "50 to 100") %>% 
                                select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                       " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p2a",
              sidebar_width = 0,
              height = 8,
              # width = 6,
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Backup - detailed trip purpose by distance
################################################################################

detailed_transit_mileage <-
  pct_calculator(
  # Add mileage bins
  mode_share_base_mdt %>% 
    mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks,labels=mileage_labels)),
  # Execute the rest of the function
  subset = "transit",
  subset_of = "mode_c",
  breakdown_by = "chain",
  second_breakdown = "mileage_bin",
  weight = "wtperfin")

# Note: The significant increase in transit for 25+ mile trips is primarily
# driven by increased work chain trips

################################################################################
# Chart of mode share by race
################################################################################

# Create labels
mode_share_p3_labels <-
  mdt_mode_race %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
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
                                      "transit","bike","schoolbus",
                                      "other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
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
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = race_eth),
             label.size = 0,
             hjust = -.02,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,xlab = "Mode share by race and ethnicity") +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.55))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac"),
                                                 pattern = "none")))
# Export finalized graphic
finalize_plot(mode_share_p3,
              title = "White residents of the region were the likeliest to rely 
              on personal automobiles for their transportation.",
              caption = 
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              'Latino' includes respondents who identified as 
              Latino or Hispanic, regardless of racial category. Other categories are non-Latino. 
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              'Other' travelers have the lowest sample size, with ",
                     format(mdt_mode_race %>% 
                              filter(race_eth == "Other") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p3",
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chart of mode share by income
################################################################################

# Create labels
mode_share_p4_labels <-
  mdt_mode_income %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(hhinc) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p4 <-
  # Get data
  mdt_mode_income %>%
  # Add labels
  left_join(mode_share_p4_labels, by = "hhinc") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus",
                                      "other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
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
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = hhinc),
             label.size = 0,
             hjust = -.02,
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,xlab = "Mode share by household income") +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.68))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac"),
                                                 pattern = "none")))
# Export finalized graphic
finalize_plot(mode_share_p4,
              title = "Households with less than $30,000 in income were the most
              reliant  on non-car modes, but the highest-income households also
              exceeded the regional average.",
              caption = 
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              <br><br>
              Sample size: Figures are based on a total of ",
                  format(nrow(mode_share_base_mdt),big.mark = ","),
                  " recorded trips. 
              Travelers with household incomes from $25,000 to $25,999 have the 
              lowest sample size, with ",
                  format(mdt_mode_income %>% 
                           filter(hhinc == "$25,000 to $29,999") %>% 
                           select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                  " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p4",
              # mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Backup - detailed mode by income
################################################################################

# Analyze percents by household income for transit ridership
mdt_mode_income_detailed <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0) %>% filter(mode_c == "transit"),
    # Execute the rest of the function
    breakdown_by = "mode",
    second_breakdown = "income_c",
    weight = "wtperfin") %>% 
  mutate(pct = round(pct,2)) %>% 
  arrange(mode)


# Analyze percents by household income for trip chain
mdt_transit_chain_income <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0) %>% 
      # Keep only transit trips
      filter(mode_c == "transit"),
    # Execute the rest of the function
    breakdown_by = "chain_c",
    second_breakdown = "income_c",
    weight = "wtperfin") %>% 
  arrange(chain_c)


# Create chart of transit use by income and trip chain
mode_share_p4a <-
  # Get data
  mdt_transit_chain_income %>% 
  # Rename for presentation
  mutate(chain_c = recode_factor(chain_c,
                                 "work" = "Work",
                                 "shop" = "Shopping",
                                 "other" = "Other"),
         income_c = recode_factor(income_c,
                                  "high" = "$100K or more",
                                  "middle-high" = "$60K to $99K",
                                  "middle-low" = "$35K to $59K",
                                  "low" = "Less than $35K")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = income_c, fill = chain_c,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) + 
  geom_col(position = position_stack(reverse = T)) +
  geom_text(aes(group = chain_c), 
            position = position_stack(reverse = T,vjust = 0.5),
            color = "white") +
  
  # Add CMAP theme
  theme_cmap(xlab = "Share of transit trips by household income and trip chain purpose",
             gridlines = "v") +
  cmap_fill_discrete(palette = "friday") +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1))

mode_share_p4a_samplesize <-
  mdt_transit_chain_income %>% 
  ungroup() %>% 
  select(income_c,n = total_n) %>% 
  distinct()

finalize_plot(mode_share_p4a,
              "Travelers from low-income households use transit for a much wider 
              range of trips than those from other households.",
              paste0(
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties. 
              Unlabeled bars are less than 5%.
              <br><br>
              Sample size:
              <br>- <$35K (",
                mode_share_p4a_samplesize %>% filter(income_c == "low") %>% select(n),
                ")
              <br>- <$35-59K (",
                mode_share_p4a_samplesize %>% filter(income_c == "middle-low") %>% select(n),
                ")
              <br>- <$60-99K (",
                mode_share_p4a_samplesize %>% filter(income_c == "middle-high") %>% select(n),
                ")
              <br>- >$100K (",
                mode_share_p4a_samplesize %>% filter(income_c == "high") %>% select(n),
                ")
                <br><br>
                Source: Chicago Metropolitan Agency for Planning analysis of My 
                Daily Travel data."),
              filename = "mode_share_p4a",
              mode = c("png","pdf"),
              overwrite = T)



################################################################################
# Chart of mode share by age
################################################################################

# Create labels
mode_share_p5_labels <-
  mdt_mode_age %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(age_bin) %>%
  summarize(label = sum(pct))

# Create plot
mode_share_p5 <-
  # Get data
  mdt_mode_age %>%
  # Add labels
  left_join(mode_share_p5_labels, by = "age_bin") %>%
  # Make changes for graphing
  mutate(
    # Reorder factors and capitalize
    mode_c = recode_factor(factor(mode_c,levels = 
                                    c("driver","passenger","walk",
                                      "transit","bike","schoolbus",
                                      "other")),
                           "passenger" = "Passenger",
                           "driver" = "Driver",
                           "walk" = "Walk",
                           "transit" = "Transit",
                           "bike" = "Bike",
                           "schoolbus" = "School bus",
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
  geom_label(aes(label = scales::label_percent(accuracy = 0.1)(label),
                 x = label, y = age_bin),
             label.size = 0,
             hjust = -.02,
             fill = "white") +

  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,
             xlab = "Mode share by age") +
  # Add colors
  scale_fill_manual(values = c("#e5bd72","#8c0000","#36d8ca","#6d8692","#efa7a7","#3d6600","#0084ac"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.55))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#8c0000","#e5bd72","#36d8ca",
                                                          "#6d8692","#efa7a7","#3d6600",
                                                          "#0084ac"),
                                                 pattern = "none")))
finalize_plot(mode_share_p5,
              title = "Children and young adults relied more on non-car modes.",
              caption = 
              paste0("Note: Includes trips by residents age 5 and older of the 
              CMAP seven county region (Cook, DuPage, Kane, Kendall, Lake, 
              McHenry, and Will), as well as Grundy and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              Travelers 70 and older have the lowest sample size, with ",
                     format(mdt_mode_age %>% 
                              filter(age_bin == "70 and above") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "mode_share_p5",
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Backup - detailed mode by age
################################################################################


mdt_mode_age_detailed <-
  pct_calculator(
    # Add age bins
    mode_share_base_mdt %>% 
      filter(age > 0) %>% 
      mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)),
    # Execute the rest of the function
    breakdown_by = "mode",
    second_breakdown = "age_bin",
    weight = "wtperfin") %>% 
  mutate(pct = round(pct,4)) %>% 
  arrange(age_bin,-pct)


################################################################################
# Backup - bike and bike share mode share
################################################################################

pct_calculator(
  mode_share_base_mdt,
  breakdown_by = "mode",
  # second_breakdown = "geog",
  weight = "wtperfin") %>% 
  mutate(pct = round(pct,4)) %>% 
  filter(mode %in% c("bike share","personal bike")) 


