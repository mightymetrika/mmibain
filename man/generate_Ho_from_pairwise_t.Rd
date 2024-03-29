% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rc_game_mgmt.R
\name{generate_Ho_from_pairwise_t}
\alias{generate_Ho_from_pairwise_t}
\title{Generate Null Hypothesis from Pairwise t-test Results}
\usage{
generate_Ho_from_pairwise_t(original_study_results)
}
\arguments{
\item{original_study_results}{A list containing the results of an original study,
including a \code{pairwise_t} element if pairwise t-tests were conducted.}
}
\value{
A string representing the null hypothesis for the study.
}
\description{
This function generates a null hypothesis statement (Ho) from the results of
pairwise t-tests. If pairwise t-tests are not available, it uses the hypothesis
from the original study results.
}
\details{
The function checks if the \code{pairwise_t} element is present in the
\code{original_study_results} list. If present, it extracts the p-values and the
corresponding column and row names to identify all unique variables involved
in the pairwise comparisons.

The unique variables are prefixed with "ColLabs" to denote the column labels
from the original dataset. These variables are then concatenated with an
equality sign to form the null hypothesis statement, which assumes no difference
between any of the groups.

If the \code{pairwise_t} element is not present, indicating that no pairwise
comparisons were made, the function returns the hypothesis generated by the
original study results.
}
\examples{
Ho <- generate_Ho_from_pairwise_t(
          original_study_results = process_original_study(
                                   df = generate_study_data(
                                   x = deal_cards_to_rc_grid(n = 3),
                                   sample_size = 30),
                                   alpha = 0.05))
}
