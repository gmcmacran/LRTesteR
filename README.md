
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRTesteR

<!-- badges: start -->

[![R build
status](https://github.com/gmcmacran/LRTesteR/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/LRTesteR/actions)
[![Codecov test
coverage](https://codecov.io/gh/gmcmacran/LRTesteR/branch/main/graph/badge.svg)](https://codecov.io/gh/gmcmacran/LRTesteR?branch=main)
[![CRAN
status](https://www.r-pkg.org/badges/version/LRTesteR)](https://cran.r-project.org/package=LRTesteR)
<!-- badges: end -->

LRTesteR implements the likelihood ratio test for many common
distributions. All tests rely on chi-square approximation even when
exact sampling distributions are known. Tests require a sample size of
at least 50. Estimated asymptotic type I and type II error rates can be
found [here](https://github.com/gmcmacran/TypeOneTypeTwoSim).

-   beta
    -   shape1 test
    -   shape2 test
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
-   geometric
    -   p test
-   negative binomial
    -   p test
-   poisson
    -   lambda test

# Test mu of a gaussian distribution.

``` r
library(LRTesteR)

# Null is true
set.seed(1)
x <- rnorm(100, 0, 1)
gaussian_mu_lr_test(x, 0, "two.sided")
#> $statistic
#> [1] 1.473569
#> 
#> $p.value
#> [1] 0.2247834
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> attr(,"class")
#> [1] "lrtest"

# Null is false
set.seed(1)
x <- rnorm(100, 3, 1)
gaussian_mu_lr_test(x, 0, "greater")
#> $statistic
#> [1] 16.03966
#> 
#> $p.value
#> [1] 3.376365e-58
#> 
#> $alternative
#> [1] "greater"
#> 
#> attr(,"class")
#> [1] "lrtest"
```
