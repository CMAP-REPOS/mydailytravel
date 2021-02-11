
pct_calculator <- function(data,
                           subset = NULL,
                           subset_of = NULL,
                           breakdown_by,
                           weight,
                           second_breakdown = NULL,
                           survey = NULL) {

  relevant_data <- data

  if (!is.null(subset)) {
    relevant_data <- relevant_data %>%
    # Filter data to relevant data
    filter(.data[[subset_of]] %in% subset)
  }

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

  percents <-
    if (is.null(survey)) {
      percents
      } else {
        percents %>%
          mutate(survey = survey)
      }

  return(percents)
}


# pct_calculator(mdt_base_2,"bike","mode_c","tpurp_c",weight = "wtperfin",survey = "mdt")
