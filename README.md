
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mmibain

<!-- badges: start -->
<!-- badges: end -->

The Mighty Metrika Interface to BAIN (‘mmibain’) R package provides
Shiny apps to explore basic functionality of the
[‘bain’](https://informative-hypotheses.sites.uu.nl/software/bain/)
package for BAyesian INformative Hypotheses Evaluation.

## Installation

You can install the released version of ‘mmibain’ from
[CRAN](https://CRAN.R-project.org):

``` r
install.packages("mmibain")
```

You can install the development version of ‘mmibain’ from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mightymetrika/mmibain")
```

## Play RepliCrisis

‘RepliCrisis’ is a Shiny app game that simulates evalutating replication
studies based on the framework presented in [Hoijtink, Mulder, van Lissa
& Gu (2019)](https://doi.org/10.1037/met0000201). Follow these steps to
play:

- Set your sample size (for groups within study), difficulty, alpha
  level, and seed for reproducibility.
- Define thresholds for the Bayes Factor and Posterior Model Probability
  to assess evidence in favor of the original study.
- Conduct the original study to generate data and form a hypothesis.
- Show diagnostics and descriptives to understand statistical results
  and hypotheses.
- Conduct a replication study, using swap controls to match the original
  study’s results.
- Run replication analysis to evaluate the results against the original
  hypothesis.
- Start a new game by conducting a new original study.

To play, load ‘mmibain’ and call the RepliCrisis() function:

``` r
library(mmibain)
RepliCrisis()
```

## mmibain Shiny App

The package also includes a Shiny app for running basic bain::bain()
models:

- Upload your data in CSV format.
- Choose your modeling engine (lm, t_test, lavaan).
- Input your model and any additional arguments.
- Fit the model and input hypotheses for evaluation.
- Adjust settings such as the fraction parameter, standardized
  regression coefficients, and confidence intervals.
- Set a seed for reproducible results.
- Run the Bayesian Informative Hypotheses Evaluation.

Launch the app with:

``` r
mmibain()
```

# References

Hoijtink, H., Mulder, J., van Lissa, C., & Gu, X. (2019). A tutorial on
testing hypotheses using the Bayes factor. Psychological methods, 24(5),
539–556. <https://doi.org/10.1037/met0000201>
