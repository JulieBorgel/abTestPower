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
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install(build_vignettes = TRUE)
```

* Directly from github repository:

``` r
if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github("JulieBorgel/abTestPower", build_vignettes = TRUE)
```

## Documentation

The package vignettes & documention can always be accessed online at https://julieborgel.github.io/abTestPower/.

Alternatively you can access the documentation locally after the package has been installed locally :

``` r
library(abTestPower)
?abTestPower
```

``` r
vignette("ABtesting", package = "abTestPower")
```

