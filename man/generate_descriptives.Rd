% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{generate_descriptives}
\alias{generate_descriptives}
\title{Generate Descriptive Statistics for Study Groups}
\usage{
generate_descriptives(df, group_var)
}
\arguments{
\item{df}{A data frame containing the study data.}

\item{group_var}{The name of the column in \code{df} that contains the group labels.}
}
\description{
This internal helper function generates descriptive statistics for each group
within the original study data. It calculates the sample size, mean, standard
deviation, median, median absolute deviation, minimum, maximum, skewness, and
kurtosis for the values associated with each group.
}
\details{
The function iterates over each unique group found in the \code{group_var} column
of the data frame \code{df}. For each group, it computes the descriptive statistics
using base R functions and functions from the \code{psych} and \code{e1071} packages for
skewness and kurtosis, respectively.

The results are returned as a data frame where each row corresponds to a group
and each column to a descriptive statistic.
}
\note{
As this is an internal function intended for use within process_original_study
function of the package.
}
\keyword{internal}
