
# abTestPower

Minimal R package to illustrate the difference between Frequentist and Bayesian approach to A/B testing.

## Installation

You can install the development version of abTestPower like so:

``` r
devtools::install(build_vignettes = TRUE)
```

## Documentation

You can check the methods documentation:

``` r
library(abTestPower)
?abTestPower
```

There is a basic example which shows the difference between Frequentist and Bayesian A/B testing in the context of planning the experimental validation of a therapeutic target prediction tool.

``` r
vignette("ABtesting", package = "abTestPower")
```

