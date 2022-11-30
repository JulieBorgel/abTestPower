  <!-- badges: start -->
[![R-CMD-check](https://github.com/JulieBorgel/abTestPower/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JulieBorgel/abTestPower/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/JulieBorgel/abTestPower/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/JulieBorgel/abTestPower/actions/workflows/test-coverage.yaml)
  <!-- badges: end -->
  
# abTestPower

Minimal R package to illustrate the difference between Frequentist and Bayesian approaches to A/B testing.

## Installation

You can install the development version of abTestPower like so:

* From a local clone:

``` r
devtools::install(build_vignettes = TRUE)
```

* Directly from github private repository:

Make sure to provide a personal access token (PAT)  as this repository is private. Check the documentation for [devtools::install_github](https://www.rdocumentation.org/packages/devtools/versions/1.13.6/topics/install_github) to know more. I have added my PAT to the `GITHUB_PAT` environment variable to be able to directly use the command below. 

``` r
devtools::install_github("JulieBorgel/abTestPower", build_vignettes = TRUE)
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

