# This file produces overall charts on mode share in the CMAP region. It is
# referenced in Policy Briefs #1, #2, and #4.

#################################################
#                                               #
#                 Libraries                     #
#                                               #
#################################################

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
  # Recode income buckets for publication
  mutate(hhinc_c = recode(hhinc,
                        "10" = "$150K or more",
                        "9" = "$100K to $149K",
                        "8" = "$60K to $99K",
                        "7" = "$60K to $99K",
                        "6" = "$35K to $59K",
                        "5" = "$35K to $59K",
                        "4" = "$15K to $34K",
                        "3" = "$15K to $34K",
                        "2" = "$15K to $34K",
                        "1" = "Less than $15K",
                        .default = "")) %>% 
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
mode_all_mdt <-
  pct_calculator(mode_share_base_mdt,
                 breakdown_by = "mode_c",
                 weight = "weight") %>% 
  ungroup()

mode_detailed_all_mdt <-
  pct_calculator(mode_share_base_mdt,
                 breakdown_by = "mode",
                 weight = "weight") %>% 
  ungroup()

## Export overall mode share for prose
mode_all_mdt %>% select(mode_c,pct) %>% mutate(pct = round(pct * 100))



# Analyze percents at the county and region-wide level
mode_counties_mdt <-
  pct_calculator(
    mode_share_base_mdt %>% 
      # For percentage calculations (for display) only calculate the seven
      # counties. This excludes DeKalb, Grundy, and the household that has a
      # home in both Chicago and Suburban Cook.
      filter(home_county %in% cmap_seven_counties),
    breakdown_by = "mode_c",
    second_breakdown = "home_county_chi",
    weight = "weight") %>% 
  # Add the regional total calculated above
  rbind(mode_all_mdt %>% mutate(home_county_chi = "CMAP region")) %>% 
  ungroup()

# Analyze percents by household income
mode_income_mdt <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "hhinc_c",
    weight = "weight") %>%
  # Add baseline totals
  rbind(mode_all_mdt %>% mutate(hhinc_c = "CMAP region")) %>% 
  ungroup() %>% 
  mutate(hhinc_c = factor(hhinc_c,levels = c("$150K or more",
                                         "$100K to $149K",
                                         "$60K to $99K",
                                         "$35K to $59K",
                                         "$15K to $34K",
                                         "Less than $15K",
                                         "CMAP region")))

# Do the same again, but for race and ethnicity
mode_race_mdt <-
  pct_calculator(
    # Keep all respondents with a reported race and ethnicity...
    mode_share_base_mdt %>% filter(race_eth != "missing"),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "race_eth",
    weight = "weight") %>%
  # Add baseline totals
  rbind(mode_all_mdt %>% mutate(race_eth = "CMAP region")) %>% 
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

mode_age_mdt <-
  pct_calculator(
    # Add age bins
    mode_share_base_mdt %>% 
      filter(age > 0) %>% 
      mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "age_bin",
    weight = "weight") %>%
  # Add baseline totals
  rbind(mode_all_mdt %>% mutate(age_bin = "CMAP region")) %>% 
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
mileage_breaks <- c(-1,.25,.5,1,2.5,5,10,25,50,100)
mileage_labels <- c("<=0.25","0.25 to 0.5","0.5 to 1","1 to 2.5","2.5 to 5",
                     "5 to 10","10 to 25","25 to 50","50 to 100")

mode_mileage_mdt <-
  pct_calculator(
    # Add mileage bins
    mode_share_base_mdt %>% 
      mutate(mileage_bin=cut(distance_pg,breaks=mileage_breaks,
                             labels=mileage_labels)) %>% 
      filter(!is.na(mileage_bin)),
    # Execute the rest of the function
    breakdown_by = "mode_c",
    second_breakdown = "mileage_bin",
    weight = "weight") %>%
  mutate(mileage_bin = factor(mileage_bin)) %>% 
  ungroup()


# Do the same for sex
mode_sex_mdt <-
  pct_calculator(
    mode_share_base_mdt,
    breakdown_by = "mode_c",
    second_breakdown = "sex",
    weight = "weight") %>% 
  # Remove DK/RF
  filter(sex > 0) %>%
  # Recode sex
  mutate(sex = recode(sex,
                      "1" = "Male",
                      "2" = "Female")) %>% 
  # Add the regional total calculated above
  rbind(mode_all_mdt %>% mutate(sex = "CMAP region")) %>% 
  ungroup()


################################################################################
# Mode share by home county
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

###### TABLE

mode_share_t1 <-
  mode_counties_mdt %>% 
  select(mode_c,home_county_chi,total_n,pct) %>% 
  pivot_wider(names_from =mode_c,
              values_from = pct) %>% 
  select(Jurisdiction = home_county_chi,
         Driver = driver,
         Passenger = passenger,
         Walk = walk,
         Transit = transit,
         "School bus" = schoolbus,
         Bike = bike,
         Other = other,
         "Sample size" = total_n
         ) %>% 
  mutate(across(Driver:Other,~scales::percent(.,accuracy = 0.1)))

write.csv(mode_share_t1,"mode_share_t1.csv")

###### CHART

# Create labels
figure1_1_labels <-
  mode_counties_mdt %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(home_county_chi) %>%
  summarize(label = sum(pct))

# Create plot
figure1_1 <-
  # Get data
  mode_counties_mdt %>%
  # Add labels
  left_join(figure1_1_labels, by = "home_county_chi") %>%
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
  ggplot(aes(x = pct, y = reorder(home_county_chi,label),group = mode_c)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_size = 0.2,
                              pattern_spacing = 0.02,
                              position = position_stack(reverse = T),
                              width = 0.85) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0,
             xlab = "Driving                                Alternatives to driving\nMode share by home jurisdiction") +
  # Add colors
  scale_fill_manual(values = c("#38B2D8","#2C2B7F","#D8BA37","#43B649","#D93636","#7451A1","#D88134"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +

  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.5),
                     expand = expansion(mult = c(.05,0))
                     ) +

  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#2C2B7F","#38B2D8","#D8BA37",
                                                          "#43B649","#D93636","#7451A1",
                                                          "#D88134"),
                                                 pattern = "none")))

# Export finalized graphic
finalize_plot(figure1_1,
              title = "Residents of Chicago and Cook County had by far the highest non-car 
              mode share in the CMAP region.",
              caption = 
              paste0(
              "Note: Includes trips by residents age 5 and older of the 
              CMAP seven-county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              McHenry County has the lowest sample size, with ",
                     format(mode_counties_mdt %>% 
                              filter(home_county_chi == "McHenry") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure1_1",
              height = 3.75,
              sidebar_width = 2.5,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chart of mode share by mileage (vertical)
################################################################################

# Create labels
figure1_2_labels <-
  mode_mileage_mdt %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(mileage_bin) %>%
  summarize(label = sum(pct))

# Create plot
figure1_2 <-
  # Get data
  mode_mileage_mdt %>%
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
  left_join(figure1_2_labels,by = c("mileage_bin")) %>% 
  
  # Create ggplot object
  ggplot(aes(x = mileage_bin,y = pct,group = mode_c,
         # Only label bars that are at least 5 percent
         label = ifelse(abs(pct) >=.05,scales::label_percent(accuracy = 1)(abs(pct)),""))) +
  geom_col(aes(fill = mode_c),
           position = position_stack(reverse = T),
           width = 0.85) +
  geom_text(aes(group = mode_c),
            position = position_stack(vjust = 0.5,reverse = T),
            color = "white") +
  geom_label(aes(label = scales::label_percent(accuracy = 1)(label),y = label),
             vjust = -.04,label.size = 0,
             label.r = grid::unit(0,"lines"),
             fill = "white") +
  
  # Add CMAP style
  theme_cmap(gridlines = "h",hline = 0, legend.max.columns = 7,
             axis.text = element_text(size = 11),
             xlab = "Mileage traveled from origin to destination",
             ylab = "Mode share\nDriving                Alternatives to driving") +
  # Add colors
  scale_fill_discrete(type = c("#38B2D8","#D8BA37","#43B649","#D93636","#7451A1","#D88134")) +
  
  # Adjust axis
  scale_y_continuous(breaks = seq(-1,.75,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.75,by = .25))),
                     limits = c(-1,.8)
  )
  
# Export finalized graphic
finalize_plot(figure1_2,
              title = "Travelers relied most on non-car modes for the shortest trips.",
              caption = 
                paste0(
              "Note: Includes trips by residents age 5 and older of the
              CMAP seven-county region, Grundy, and DeKalb. Includes only
              trips that were within, to, and/or from one of those counties.
              Distances capture the total distance traveled along the route, not just the
              distance from origin to destination. Unlabeled bars have less than
              5 percent mode share. \"By car\" includes trips as either a driver or a passenger
              of a personal vehicle (not including services like taxis or TNCs).
              <br><br>
              Sample size: Figures are based on a total of ",
              format(nrow(mode_share_base_mdt),big.mark = ","),
              " recorded trips.
              Trips of 50 to 100 miles have the lowest sample size, with ",
              format(mode_mileage_mdt %>%
              filter(mileage_bin == "50 to 100") %>%
              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
              " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure1_2",
              sidebar_width = 2.25,
              height = 5,
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
  breakdown_by = "chain_c",
  second_breakdown = "mileage_bin",
  weight = "weight")

# Note: The significant increase in transit for 25+ mile trips is primarily
# driven by increased work chain trips

################################################################################
# Chart of mode share by race
################################################################################

# Create labels
figure2_5_labels <-
  mode_race_mdt %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(race_eth) %>%
  summarize(label = sum(pct))

# Create plot
figure2_5 <-
  # Get data
  mode_race_mdt %>%
  # Add labels
  left_join(figure2_5_labels, by = "race_eth") %>%
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
  ggplot(aes(x = pct, y = reorder(str_wrap(race_eth,18),label),group = mode_c)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_size = 0.2,
                              pattern_spacing = 0.02,
                              position = position_stack(reverse = T),
                              width = 0.85) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,
             xlab = "Driving                                Alternatives to driving\nMode share by race and ethnicity") +
  # Add colors
  scale_fill_manual(values = c("#38B2D8","#2C2B7F","#D8BA37","#43B649","#D93636","#7451A1","#D88134"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.5),
                     expand = expansion(mult = c(.05,0)))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#2C2B7F","#38B2D8","#D8BA37",
                                                          "#43B649","#D93636","#7451A1",
                                                          "#D88134"),
                                                 pattern = "none")))
# Export finalized graphic
finalize_plot(figure2_5,
              title = "White residents in the region were the likeliest to rely 
              on personal automobiles for their transportation.",
              caption = 
              paste0(
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven-county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties. 
              See \"About the data\" for more information on race and ethnicity.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              \"Other\" travelers have the lowest sample size, with ",
                     format(mode_race_mdt %>% 
                              filter(race_eth == "Other") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure2_5",
              height = 4,
              sidebar_width = 2.5,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chart of mode share by income
################################################################################

# Create labels
figure2_6_labels <-
  mode_income_mdt %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(hhinc_c) %>%
  summarize(label = sum(pct))

# Create plot
figure2_6 <-
  # Get data
  mode_income_mdt %>%
  # Add labels
  left_join(figure2_6_labels, by = "hhinc_c") %>%
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
    type = ifelse(hhinc_c == "CMAP region","1","0")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = hhinc_c,group = mode_c)) +
  # Use "geom_col_pattern" to add texture to a subset of columns
  ggpattern::geom_col_pattern(aes(fill = mode_c,pattern = type),
                              pattern_color = "white",
                              pattern_fill = "white",
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_size = 0.2,
                              pattern_spacing = 0.02,
                              position = position_stack(reverse = T),
                              width = 0.85) +   
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,
             xlab = "Driving                                Alternatives to driving\nMode share by household income") +
  # Add colors
  scale_fill_manual(values = c("#38B2D8","#2C2B7F","#D8BA37","#43B649","#D93636","#7451A1","#D88134"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.5),
                     expand = expansion(mult = c(.05,0)))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#2C2B7F","#38B2D8","#D8BA37",
                                                          "#43B649","#D93636","#7451A1",
                                                          "#D88134"),
                                                 pattern = "none")))
# Export finalized graphic
finalize_plot(figure2_6,
              title = "Households with less than $35,000 in income were the most
              reliant  on non-car modes, but the highest-income households also
              neared the regional average.",
              caption = 
              paste0(
                "Note: Includes trips by residents age 5 and older of the
              CMAP seven-county region, Grundy, and DeKalb. Includes only
              trips that were within, to, and/or from one of those counties.
              <br><br>
              Sample size: Figures are based on a total of ",
                  format(nrow(mode_share_base_mdt),big.mark = ","),
                  " recorded trips. 
              Travelers with household incomes below $15,000 have the 
              lowest sample size, with ",
                  format(mode_income_mdt %>% 
                           filter(hhinc_c == "Less than $15K") %>% 
                           select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                  " records.
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure2_6",
              mode = c("png","pdf"),
              height = 4.5,
              sidebar_width = 2.5,
              overwrite = T)

################################################################################
# Backup - detailed mode by income
################################################################################

# Analyze percents by household income for transit ridership
mode_income_detailed_mdt <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0) %>% filter(mode_c == "transit"),
    # Execute the rest of the function
    breakdown_by = "mode",
    second_breakdown = "hhinc_c",
    weight = "weight") %>% 
  mutate(pct = round(pct,2)) %>% 
  arrange(mode)

################################################################################
# Chart of transit use by trip chain and income
################################################################################

# Analyze percents by household income for trip chain
transit_chain_income_mdt <-
  pct_calculator(
    # Keep all respondents with a reported household income
    mode_share_base_mdt %>% filter(hhinc > 0) %>% 
      # Keep only transit trips
      filter(mode_c == "transit"),
    # Execute the rest of the function
    breakdown_by = "chain_c",
    second_breakdown = "hhinc_c",
    weight = "weight") %>% 
  arrange(chain_c) %>% 
  # Reorder
  mutate(hhinc_c = factor(hhinc_c,levels = c("$150K or more",
                                             "$100K to $149K",
                                             "$60K to $99K",
                                             "$35K to $59K",
                                             "$15K to $34K",
                                             "Less than $15K")))



# Create chart of transit use by income and trip chain
figure2_7 <-
  # Get data
  transit_chain_income_mdt %>% 
  # Rename for presentation
  mutate(chain_c = recode_factor(chain_c,
                                 "work" = "Work",
                                 "shop" = "Shopping",
                                 "other" = "Other")) %>%
  
  # Create ggplot object
  ggplot(aes(x = pct, y = hhinc_c, fill = chain_c,
             # Only label bars that round to at least 5 percent
             label = ifelse(pct >=.05,scales::label_percent(accuracy = 1)(pct),""))) + 
  geom_col(position = position_stack(reverse = T), width = 0.85) +
  geom_text(aes(group = chain_c), 
            position = position_stack(reverse = T,vjust = 0.5),
            color = "white") +
  
  # Add CMAP theme
  theme_cmap(xlab = "Share of transit trips by household income and trip chain purpose",
             gridlines = "v") +
  scale_fill_manual(values = c("#2C2B7F","#38B2D8","#43B649")) +
  
  # Adjust axis
  scale_x_continuous(labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(.05,0)))

figure2_7_samplesize <-
  transit_chain_income_mdt %>% 
  ungroup() %>% 
  select(hhinc_c,n = total_n) %>% 
  distinct()

finalize_plot(figure2_7,
              "Travelers from low-income households use transit for a much wider 
              range of trips than those from other households.",
              paste0(
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven-county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties. 
              Unlabeled bars have less than 5 percent mode share.
              <br><br>
              Sample size:
              <br>- <$15K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "Less than $15K") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
              <br>- $15-34K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "$15K to $34K") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
              <br>- $35-59K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "$35K to $59K") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
              <br>- $60-99K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "$60K to $99K") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
              <br>- $100-149K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "$100K to $149K") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
              <br>- >=$150K (",
                format(figure2_7_samplesize %>% 
                         filter(hhinc_c == "$150K or more") %>% 
                         select(n) %>% 
                         as.numeric(),
                       big.mark = ","),
                ")
                <br><br>
                Source: Chicago Metropolitan Agency for Planning analysis of My 
                Daily Travel data."),
              filename = "figure2_7",
              height = 4.5,
              sidebar_width = 2.5,
              mode = c("png","pdf"),
              overwrite = T)

################################################################################
# Chart of mode share by age
################################################################################

# Create labels
figure2_8_labels <-
  mode_age_mdt %>%
  filter(mode_c %in% c("walk","transit","bike","schoolbus","other")) %>%
  group_by(age_bin) %>%
  summarize(label = sum(pct))

# Create plot
figure2_8 <-
  # Get data
  mode_age_mdt %>%
  # Add labels
  left_join(figure2_8_labels, by = "age_bin") %>%
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
                              pattern_angle = 45,
                              pattern_density = 0.125,
                              pattern_size = 0.2,
                              pattern_spacing = 0.02,
                              position = position_stack(reverse = T),
                              width = 0.85) +  
  # Re-assign patterns manually
  scale_pattern_manual(values = c("1" = "stripe",
                                  "0" = "none"),
                       guide = "none") +
  
  # Add CMAP style
  theme_cmap(gridlines = "v", vline = 0, legend.max.columns = 6,
             xlab = "Driving                                Alternatives to driving\nMode share by age") +
  # Add colors
  scale_fill_manual(values = c("#38B2D8","#2C2B7F","#D8BA37","#43B649","#D93636","#7451A1","#D88134"),
                    labels = c("Driver","Passenger","Walk","Transit","Bike","School bus","Other")) +
  
  # Adjust axis
  scale_x_continuous(breaks = seq(-1,.5,by = .25), 
                     labels = scales::label_percent()(abs(seq(-1,.5,by = .25))),
                     limits = c(-1,.5),
                     expand = expansion(mult = c(.05,0)))+
  
  # Adjust legend for formatting
  guides(fill = guide_legend(ncol = 7,
                             override.aes = list(fill = c("#2C2B7F","#38B2D8","#D8BA37",
                                                          "#43B649","#D93636","#7451A1",
                                                          "#D88134"),
                                                 pattern = "none")))
finalize_plot(figure2_8,
              title = "Children and young adults relied more on non-car modes.",
              caption = 
              paste0(
                "Note: Includes trips by residents age 5 and older of the 
              CMAP seven-county region, Grundy, and DeKalb. Includes only 
              trips that were within, to, and/or from one of those counties.
              <br><br>
              Sample size: Figures are based on a total of ",
                     format(nrow(mode_share_base_mdt),big.mark = ","),
                     " recorded trips. 
              Travelers 70 and older have the lowest sample size, with ",
                     format(mode_age_mdt %>% 
                              filter(age_bin == "70 and above") %>% 
                              select(total_n) %>% distinct() %>% as.numeric(),big.mark = ","),
                     " records. 
              <br><br>
              Source: Chicago Metropolitan Agency for Planning analysis of My
              Daily Travel data."),
              filename = "figure2_8",
              height = 3.5,
              sidebar_width = 2.5,
              mode = c("png","pdf"),
              overwrite = T)


################################################################################
# Backup - detailed mode by age
################################################################################

mode_age_detailed_mdt <-
  pct_calculator(
    # Add age bins
    mode_share_base_mdt %>% 
      filter(age > 0) %>% 
      mutate(age_bin=cut(age,breaks=age_breaks,labels=age_labels)),
    # Execute the rest of the function
    breakdown_by = "mode",
    second_breakdown = "age_bin",
    weight = "weight") %>% 
  mutate(pct = round(pct,4)) %>% 
  arrange(age_bin,-pct)


################################################################################
# Backup - bike and bike share mode share
################################################################################

pct_calculator(
  mode_share_base_mdt,
  breakdown_by = "mode",
  # second_breakdown = "geog",
  weight = "weight") %>% 
  mutate(pct = round(pct,4)) %>% 
  filter(mode %in% c("bike share","personal bike")) 

