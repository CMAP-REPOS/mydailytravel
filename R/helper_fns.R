library(tidyverse)
library(lubridate)
library(slider)

################################################################################
# Purpose: A wrapper for stringr::str_wrap that enables text wrapping of
# string factors in charts.
################################################################################
# Inputs: * x, a vector of strings.
#         * ..., arguments for the str_wrap function
################################################################################
str_wrap_factor <- function(x, ...) {
  levels(x) <- stringr::str_wrap(levels(x), ...)
  x
}


################################################################################
# Purpose: Calculate percentage breakdowns of data based on chosen fields.
#
################################################################################
# Inputs: * data, the table to be analyzed.
#         * subset_of and subset, a string and a vector of strings. Defaults to
#           NULL. If included, the function will filter the column "subset_of"
#           for inclusion in the string(s) given in "subset".
#         * breakdown_by, a string. This should be the name of the column upon
#           which the percentage breakdown calculations should be performed.
#         * weight, a string. Defaults to NULL. If included, this should be the
#           name of the column in which data weights can be found. If it remains
#           NULL, the implied weight of each record will be 1.
#         * second_breakdown and third_breakdown, both strings. Default to NULL. 
#           If included, these enable the user to add a second and third 
#           breakdown column for calculation.
#         * survey, a string. Defaults to NULL. If included, the output table
#           will include a column called "survey" with the provided value as the
#           entry for each record.
################################################################################
# Outputs: This function calculates the rolling average of trips in motion
#          within a specified set of categories (e.g., by mode) over the course
#          of a single day, running from 3am to 3am.
################################################################################
pct_calculator <- function(data,
                           subset_of = NULL,
                           subset = NULL,
                           breakdown_by,
                           weight = NULL,
                           second_breakdown = NULL,
                           third_breakdown = NULL,
                           survey = NULL) {

  # Confirm that if there is a third breakdown there is also every breakdown
  # above it.
  if (!is.null(third_breakdown) & is.null(second_breakdown)) {
    stop("You cannot enter a third breakdown without specifying a second breakdown.")
  }
  
  # Load data
  relevant_data <- data
  # Create helper weight name variable
  weight_name <- weight

  # If there is a subset filter the data
  if (!is.null(subset)) {
    relevant_data <- relevant_data %>%
      # Filter data to relevant data
      filter(.data[[subset_of]] %in% subset)
  }

  # If there is no weight specified, add a column with a uniform weight of 1
  if (is.null(weight)) {
    relevant_data <- relevant_data %>% 
      mutate(uniform_weight = 1)
    
    # Load this name into the helper weight name variable
    weight_name <- "uniform_weight"
  }
    
  # Now we calculate the percents. First, if there is no second breakdown.
  if (is.null(second_breakdown)) {
    percents <-
      relevant_data %>%
      # Create totals
      mutate(total = sum(.data[[weight_name]]),
             total_n = n()) %>%
      # Calculate percentages
      group_by(across(all_of(breakdown_by))) %>%
      summarize(breakdown_total = sum(.data[[weight_name]]),
                total = median(total),
                total_n = median(total_n),
                breakdown_n = n()) %>%
      mutate(pct = breakdown_total / total)
  }
  # Next, if there is a second breakdown but not a third breakdown.
  else if (is.null(third_breakdown)) {
    percents <-
      relevant_data %>%
      # Create totals
      group_by(across(all_of(second_breakdown))) %>%
      mutate(total = sum(.data[[weight_name]]),
             total_n = n()) %>%
      # Calculate percentages
      group_by(across(all_of(c(breakdown_by,second_breakdown)))) %>%
      summarize(breakdown_total = sum(.data[[weight_name]]),
                total = median(total),
                total_n = median(total_n),
                breakdown_n = n()) %>%
      mutate(pct = breakdown_total / total)
    
    # Finally, if there is a third breakdown
  } else {
    percents <-
      relevant_data %>%
      # Create totals
      group_by(across(all_of(c(second_breakdown,third_breakdown)))) %>%
      mutate(total = sum(.data[[weight_name]]),
             total_n = n()) %>%
      # Calculate percentages
      group_by(across(all_of(c(breakdown_by,second_breakdown,third_breakdown)))) %>%
      summarize(breakdown_total = sum(.data[[weight_name]]),
                total = median(total),
                total_n = median(total_n),
                breakdown_n = n()) %>%
      mutate(pct = breakdown_total / total)
  }

  # If there is a survey marker, add it.
  percents <-
    if (is.null(survey)) {
      percents
    } else {
      percents %>%
        mutate(survey = survey)
    }

  # If there was no weight, remove the duplicate calculations.
  if (is.null(weight)) {
    percents <- 
      percents %>% 
      select(-total_n,-breakdown_n)
  }
  
  # Return the data.
  return(percents)
}


################################################################################
# Purpose: Calculate trip times within the intervals that meet given criteria.
#
################################################################################
# Inputs: * data, the base data set for analysis.
#         * weights, a string representing the name of a weight column. Defaults
#           to NULL, in which case a uniform weight of 1 is applied. The most
#           common use of this will be the MDT weight column, `wtperfin`,
#           although others can be applied.
#         * trip_interval, a vector of date-time objects. This defaults to the
#           full list of trip intervals in the tim_mdt_wip table, but can be
#           modified to use a shorter list for subset analyses.
#         * criteria, a vector of strings. This will be the categorization into
#           which trip counts are distributed (e.g., mode categories). Takes at
#           most two criteria. A second criterion  will significantly increase
#           processing times for large datasets.
#         * interval, numeric. This is the width of the time interval in
#           minutes in which trips will be counted. This must be a whole number
#           and a divisor of the total number of minutes in the day (i.e.,
#           values like 2, 5, 10, 30, 60). Defaults to 5.
#         * rolling_window, numeric. This is the length of the rolling average
#           window, in minutes. Defaults to 25 minutes. The rolling average is
#           calculated as a straddling rolling average, with the interval plus a
#           symmetric window on either side. rolling_window should be a whole 
#           number and a multiple of interval by an odd number.
################################################################################
# Outputs: This function calculates the rolling average of trips in motion
#          within a specified set of categories (e.g., by mode) over the course
#          of a single day, running from 3am to 3am.
################################################################################
tim_calculator <- function(data,
                           weights = NULL,
                           trip_interval = "trip_interval",
                           criteria,
                           interval = 5,
                           rolling_window = 25) {

  # Allow at most two criteria
  if (length(criteria) > 2) {
    stop("Only at most two criteria are allowed")
  }
  
  # Require rolling_window and interval to be integers
  if (interval %% 1 != 0 | rolling_window %% 1 != 0) {
    stop("Both `rolling_window` and `interval` must be whole numbers.")
  }
  
  # Require `interval` to be a divisor of the number of seconds in a day
  if (((60*60*24) %% (interval * 60)) != 0) {
    stop("The specified value for `interval` is not a divisor of the number of minutes in the day.")
  }
  
  # Require rolling_window to be a multiple of interval by an odd number
  if ((rolling_window %% interval) != 0 | (rolling_window / interval) %% 2 == 0) {
    stop("The value for `rolling_window` should be a multiple of the value for `interval`. Dividing the two should also yield an odd number to allow for an even distribution of intervals for the rolling average calculations.")
  }
  
  # Extract relevant columns
  
  # First, create a helper joint column for two-criteria function calls
  if (length(criteria) == 2) {
    wip_data <- data %>% 
      mutate(criteria_wip = 
               paste(.data[[criteria[1]]],
                     .data[[criteria[2]]],
                     sep = "-"))}
  else {
    wip_data <- data %>% 
      mutate(criteria_wip = .data[[criteria]])
  }
  
  # Now create helper columns for extracting trip intervals
  wip_data <-
    wip_data %>% 
    mutate(trip_interval = .data[[trip_interval]])
  
  # Finally, extract weights (when called for) or create a uniform weight of 1
  if(!is.null(weights)) {
    wip_data <-
      wip_data %>% 
      mutate(weights = .data[[weights]])
  } else {
    wip_data <-
      wip_data %>% 
      mutate(weights = 1)
  }
  
  # Create a helper tibble of possible criteria
  possibilities <- tibble(identifier = unique(wip_data$criteria_wip))

  # Create tibble of all possible trip intervals
  trip_times <-
    # Establish sequence of times over the day (in specified minute increments)
    tibble(time_band = seq.POSIXt(
      from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
      to = as.POSIXct("2020-01-02 03:00:00", tz = "America/Chicago") - interval * 60,
      # Interval using input above, defaults to 5
      by = paste0(interval," min"))) %>%
    mutate(time_band_interval = interval(time_band, time_band + interval * 60 - 1, tz = "America/Chicago"))

  # Calculate number of intervals on either side of the interval needed for
  # rolling average
  rolling_window_calc <- ((rolling_window / interval) - 1) / 2

  # Calculate trips in motion within the intervals meeting criteria
  raw_count <-
    # Start with all trip intervals
    trip_times %>%
    # Add all possible modes and/or purposes and/or chains
    full_join(possibilities, by = character()) %>%
    # Sum the weights that meet both criteria
    mutate(count = 
             mapply(function(x,y) 
               # Sum the weights that...
               sum(wip_data$weights[which(
                 # Overlap the specified time interval, and
                 lubridate::int_overlaps(x,wip_data$trip_interval) &
                   # Are the correct category of mode/chain/purpose/etc.
                   y == wip_data$criteria_wip)]),
               # x is the sequence of intervals defined above
               time_band_interval,
               # y is the list of unique possibilities for mode/chain/purpose
               # identified above
               identifier
    ))

  # If there are two criteria, need to split identifier and group by both
  # columns. Otherwise, group by identifier.
  if (length(criteria) != 2) {
    grouped_count <-
      raw_count %>%
      group_by(identifier) %>% 
      # Use the {{ }} := construction to extract the criteria name for column
      # naming (see https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html)
      rename({{criteria}} := identifier)
  } else {
    grouped_count <-
      raw_count %>%
      # Split the data using the '-' as a separator
      separate(identifier,into = c(criteria[1],criteria[2]) , sep = "-") %>%
      # Group by name
      group_by(across(all_of(criteria)))
  }

  # Calculate rolling averages
  output <-
    grouped_count %>%
    # Calculate rolling average
    mutate(rolling_count = slide_dbl(count, mean,
                                     .before = rolling_window_calc,
                                     .after = rolling_window_calc)) %>%
    ungroup()

  # Finally, return output
  return(output)

}

