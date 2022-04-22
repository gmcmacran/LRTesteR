
<!-- README.md is generated from README.Rmd. Please edit that file -->

# MLTesteR

<!-- badges: start -->

[![R build
status](https://github.com/gmcmacran/MLTesteR/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/MLTesteR/actions)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/MLTesteR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/gmcmacran/MLTesteR?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/MLTesteR)](https://cran.r-project.org/package=MLTesteR)
<!-- badges: end -->

MLTester is a work in progress. The goal is to implement the likelihood
ratio, wald and score tests for all common families. Currently a few
likelihood ratio tests are implemented. A thorough evaluation (type 1
error rate, publication comparisons, thorough test creation, etc) have
not been done yet.

Package is mostly of theoretical interest. Exact tests are implemented
in other R packages. These tests are all approximations.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gmcmacran/MLTesteR")
```

## Example

This is a basic example which shows how to test the mean of a Gaussian
distribution with the likelihood ratio test:

``` r
library(MLTesteR)

# Null is true
set.seed(1)
x <- rnorm(100, 0, 1)
gaussian_mu_lr_test(x, 0, "two.sided")
#> $statistic
#> [1] 1.469635
#> 
#> $p.value
#> [1] 0.2254033
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> attr(,"class")
#> [1] "mltest"

# Null is false
set.seed(1)
x <- rnorm(100, 3, 1)
gaussian_mu_lr_test(x, 0, "greater")
#> $statistic
#> [1] 34.61244
#> 
#> $p.value
#> [1] 8.209628e-263
#> 
#> $alternative
#> [1] "greater"
#> 
#> attr(,"class")
#> [1] "mltest"
```
