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

concatenate_relations <- function(greater_less, equals) {
  if (greater_less == "" && equals == "") {
    return("")
  } else if (greater_less == "") {
    return(equals)
  } else if (equals == "") {
    return(greater_less)
  } else {
    return(paste(greater_less, equals, sep = " & "))
  }
}

generate_descriptives <- function(df, group_var) {
  # List of unique groups
  groups <- unique(df[[group_var]])

  # Function to calculate descriptives for each group
  calc_descriptives <- function(group_data) {
    group_ColVals <- group_data$ColVals # Assuming 'Feuchte' column is equivalent to 'ColVals'

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
