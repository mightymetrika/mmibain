#' Convert a Character String to a List
#'
#' This internal function converts a character string representing R arguments
#' into a list of arguments. It is primarily used to facilitate the passing of
#' additional arguments from the Shiny UI to internal functions within the app.
#'
#' @param arg_str A character string representing R arguments.
#'
#' @return A list containing the arguments represented by \code{arg_str}. If
#'   \code{arg_str} is not a valid representation of R arguments, the function
#'   will throw an error.
#'
#' @keywords internal
str2list <- function(arg_str) {
  # Evaluate the string in a new environment to convert it to a list
  eval(parse(text = paste0("list(", arg_str, ")")), envir = new.env())
}

#' Generate Descriptive Statistics for Study Groups
#'
#' @description
#' This internal helper function generates descriptive statistics for each group
#' within the original study data. It calculates the sample size, mean, standard
#' deviation, median, median absolute deviation, minimum, maximum, skewness, and
#' kurtosis for the values associated with each group.
#'
#' @param df A data frame containing the study data.
#' @param group_var The name of the column in `df` that contains the group labels.
#'
#' @details
#' The function iterates over each unique group found in the `group_var` column
#' of the data frame `df`. For each group, it computes the descriptive statistics
#' using base R functions and functions from the `psych` and `e1071` packages for
#' skewness and kurtosis, respectively.
#'
#' The results are returned as a data frame where each row corresponds to a group
#' and each column to a descriptive statistic.
#'
#' @note
#' As this is an internal function intended for use within process_original_study
#' function of the package.
#'
#' @keywords internal
generate_descriptives <- function(df, group_var) {
  # List of unique groups
  groups <- unique(df[[group_var]])

  # Function to calculate descriptives for each group
  calc_descriptives <- function(group_data) {
    group_ColVals <- group_data$ColVals

    list(
      n = length(group_ColVals),
      mean = mean(group_ColVals, na.rm = TRUE),
      sd = stats::sd(group_ColVals, na.rm = TRUE),
      median = stats::median(group_ColVals, na.rm = TRUE),
      mad = stats::mad(group_ColVals, na.rm = TRUE),
      min = min(group_ColVals, na.rm = TRUE),
      max = max(group_ColVals, na.rm = TRUE),
      skew = psych::skew(group_ColVals, na.rm = TRUE),
      kurtosis = e1071::kurtosis(group_ColVals, na.rm = TRUE)
    )
  }

  # Apply the function to each group
  descriptives_list <- lapply(groups, function(group) {
    group_data <- df[df[[group_var]] == group, ]
    c(list(Group = group), calc_descriptives(group_data))
  })

  # Convert list to data frame
  descriptives_df <- do.call(rbind.data.frame, descriptives_list)

  return(descriptives_df)
}
