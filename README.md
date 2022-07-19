
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRTesteR Overview

<!-- badges: start -->

[![R build
status](https://github.com/gmcmacran/LRTesteR/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/LRTesteR/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/LRTesteR)](https://cran.r-project.org/package=LRTesteR)
<!-- badges: end -->

LRTesteR implements the likelihood ratio test for many common
distributions. All tests rely on the chi-square approximation even when
exact sampling distributions are known. Tests require a sample size of
at least 50. Estimated asymptotic type I and type II error rates can be
found [here](https://github.com/gmcmacran/TypeOneTypeTwoSim).

# Implemented Tests

-   beta
    -   shape 1 test
    -   shape 2 test
-   binomial
    -   p test
-   exponential
    -   rate test
-   gamma
    -   rate test
    -   scale test
    -   shape test
-   gaussian
    -   mu test
    -   variance test
-   negative binomial
    -   p test
-   poisson
    -   lambda test

# Example 1: Test lambda of a poisson distribution

Lets create some data to work with.

``` r
library(LRTesteR)

set.seed(1)
x <- rpois(n = 100, lambda = 1)
```

To test lambda, simply call poisson_lambda_lr_test.

``` r
poisson_lambda_lr_test(x = x, lambda = 1, alternative = "two.sided")
#> $statistic
#> [1] 0.009966832
#> 
#> $p.value
#> [1] 0.9204761
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> attr(,"class")
#> [1] "lrtest"
```

Because we generated the data, we know the true value of lambda is one.
The p value is above 05%.

# Example 2: Test rate of an exponential distribution

This time, lets run a test where the null is false. Below the true rate
is 3 and the rejected null tests a rate of 1.

``` r
library(LRTesteR)

set.seed(1)
x <- rexp(n = 100, rate = 3)
exponentail_rate_lr_test(x = x, rate = 1, alternative = "two.sided")
#> $statistic
#> [1] 82.39116
#> 
#> $p.value
#> [1] 1.116523e-19
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> attr(,"class")
#> [1] "lrtest"
```
