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
#           for inclusion in the strings given in "subset".
#         * breakdown_by, a string. This should be the name of the column upon
#           which the percentage breakdown calculations should be performed.
#         * weight, a string. Defaults to NULL. If included, this should be the
#           name of the column in which data weights can be found.
#         * second_breakdown, a string. Defaults to NULL. If included, this
#           enables the user to add a second breakdown column for calculation.
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
                           survey = NULL) {

  relevant_data <- data

  if (!is.null(subset)) {
    relevant_data <- relevant_data %>%
      # Filter data to relevant data
      filter(.data[[subset_of]] %in% subset)
  }

  if (!is.null(weight)) {
    if (is.null(second_breakdown)) {
      percents <-
        relevant_data %>%
        # Create totals
        mutate(total = sum(.data[[weight]]),
               total_n = n()) %>%
        # Calculate percentages
        group_by(.data[[breakdown_by]]) %>%
        summarize(breakdown_total = sum(.data[[weight]]),
                  total = median(total),
                  total_n = median(total_n),
                  breakdown_n = n()) %>%
        mutate(pct = breakdown_total / total)
    }
    else {
      percents <-
        relevant_data %>%
        # Create totals
        group_by(.data[[second_breakdown]]) %>%
        mutate(total = sum(.data[[weight]]),
               total_n = n()) %>%
        # Calculate percentages
        group_by(.data[[breakdown_by]],.data[[second_breakdown]]) %>%
        summarize(breakdown_total = sum(.data[[weight]]),
                  total = median(total),
                  total_n = median(total_n),
                  breakdown_n = n()) %>%
        mutate(pct = breakdown_total / total)

    }
  } else {
    if (is.null(second_breakdown)) {
      percents <-
        relevant_data %>%
        # Create totals
        mutate(total = n()) %>%
        # Calculate percentages
        group_by(.data[[breakdown_by]]) %>%
        summarize(total = median(total),
                  breakdown = n()) %>%
        mutate(pct = breakdown / total)
    }
    else {
      percents <-
        relevant_data %>%
        # Create totals
        group_by(.data[[second_breakdown]]) %>%
        mutate(total = n()) %>%
        # Calculate percentages
        group_by(.data[[breakdown_by]],.data[[second_breakdown]]) %>%
        summarize(total = median(total),
                  breakdown = n()) %>%
        mutate(pct = breakdown / total)
    }
  }

  percents <-
    if (is.null(survey)) {
      percents
    } else {
      percents %>%
        mutate(survey = survey)
    }

  return(percents)
}


################################################################################
# Purpose: Calculate trip times within the intervals that meet given criteria.
#
################################################################################
# Inputs: * base_weights, a numeric vector. This defaults to the full list of
#           weights in the tim_mdt_wip table, but can be modified to use a
#           shorter list (for subset analyses) or a different column (e.g., for
#           unweighted analyses)
#         * trip_interval, a vector of date-time objects. This defaults to the
#           full list of trip intervals in the tim_mdt_wip table, but can be
#           modified to use a shorter list for subset analyses.
#         * criteria, a vector of strings. This will be the categorization into
#           which trip counts are distributed (e.g., mode categories or trip
#           chains by mode).
#         * rolling_interval, numeric. This is the width of the time interval in
#           minutes in which trips will be counted. Defaults to 5.
#         * rolling_n, numeric. This is the length of the rolling average
#           window, in minutes. Defaults to 25 minutes. The rolling average is
#           calculated as a straddling rolling average, with the interval plus a
#           symmetric window on either side. rolling_n should be an odd multiple
#           of rolling_interval.
#         * crosstab, bool. Defaults to FALSE. this should be set to TRUE if the
#           criteria includes two buckets (e.g., chain and mode category).
#         * crosstab1, crosstab2, both strings. This should be supplied when
#           crosstab = T. Each string will be outputted as the variable name for
#           the final trips in motion calculation. For example, a crosstab
#           analysis of tim_wip_mdt$mode_tpurp_c should have a value of
#           crosstab1 = "mode" and crosstab2 = "tpurp_c"
################################################################################
# Outputs: This function calculates the rolling average of trips in motion
#          within a specified set of categories (e.g., by mode) over the course
#          of a single day, running from 3am to 3am.
################################################################################
tim_calculator <- function(base_weights = tim_mdt_wip$wtperfin,
                           trip_interval = tim_mdt_wip$trip_interval,
                           criteria,
                           rolling_interval = 5,
                           rolling_n = 25,
                           crosstab = F,
                           crosstab1 = NULL,
                           crosstab2 = NULL) {

  # Create a helper tibble of possible criteria
  possibilities <- tibble(identifier = unique(criteria))

  # Create tibble of all possible trip intervals
  trip_times <-
    # Establish sequence of times over the day (in five minute increments)
    tibble(time_band = seq.POSIXt(
      from = as.POSIXct("2020-01-01 03:00:00", tz = "America/Chicago"),
      to = as.POSIXct("2020-01-02 02:55:00", tz = "America/Chicago"),
      # Interval using input above, defaults to 5
      by = paste0(rolling_interval," min"))) %>%
    mutate(time_band_interval = interval(time_band, time_band + 5 * 60 - 1, tz = "America/Chicago"))

  # Calculate number of intervals on either side of the interval needed for
  # rolling average
  rolling_n_calc <- ((rolling_n / rolling_interval) - 1) / 2

  # Calculate trips in motion within the intervals meeting criteria
  raw_count <-
    # Start with all trip intervals
    trip_times %>%
    # Add all possible modes and/or purposes and/or chains
    full_join(possibilities, by = character()) %>%
    # Sum the weights that meet both criteria
    mutate(count = mapply(function(x,y) sum(base_weights[which(
      int_overlaps(x,trip_interval) &
        y == criteria)]),
      time_band_interval,
      identifier
    ))

  # If crosstab, need to split identifier and group by both columns. Otherwise,
  # group by identifier.
  if (!crosstab) {
    grouped_count <-
      raw_count %>%
      group_by(identifier)
  } else {
    grouped_count <-
      raw_count %>%
      # Split the data using the '-' as a separator
      separate(identifier,into = c(crosstab1,crosstab2) , sep = "-") %>%
      # using the .data[[crosstabx]] format allows us to use the string as a
      # variable name
      group_by(.data[[crosstab1]],.data[[crosstab2]])
  }

  # Calculate rolling averages
  output <-
    grouped_count %>%
    # Calculate rolling average
    mutate(rolling_count = slide_dbl(count, mean,
                                     .before = rolling_n_calc,
                                     .after = rolling_n_calc)) %>%
    ungroup()

  return(output)

}

