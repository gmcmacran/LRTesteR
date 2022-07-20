
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRTesteR Overview

<!-- badges: start -->

[![R build
status](https://github.com/gmcmacran/LRTesteR/workflows/R-CMD-check/badge.svg)](https://github.com/gmcmacran/LRTesteR/actions)
[![CRAN
status](https://www.r-pkg.org/badges/version/LRTesteR)](https://cran.r-project.org/package=LRTesteR)
<!-- badges: end -->

LRTesteR provides likelihood ratio test and associated confidence
intervals for many common distributions. All tests and CIs rely on the
chi-square approximation even when exact sampling distributions are
known. Tests require a sample size of at least 50. Estimated asymptotic
type I and type II error rates can be found
[here](https://github.com/gmcmacran/TypeOneTypeTwoSim).

All functions match popular tests in R. If you are familiar with t.test
and binom.test, you already know how to use these functions.

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
-   Gaussian
    -   mu test
    -   variance test
-   negative binomial
    -   p test
-   Poisson
    -   lambda test
-   Cauchy
    -   location
    -   scale

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
#> $conf.int
#> [1] 0.8256173 1.2199816
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
#> $conf.int
#> [1] 2.376868 3.519063
#> 
#> $alternative
#> [1] "two.sided"
#> 
#> attr(,"class")
#> [1] "lrtest"
```

# Mathematical Details

The strength of the likelihood ratio test is its generality. It is a a
recipe to create hypothesis tests and confidence intervals in many
different settings. Sometimes the test is the only known procedure. When
there are many procedures, likelihood ratio tests tend to have very good
**asymptotic** type I error rates.

The weakness of the likelihood ratio test is it depends on two
assumptions:

-   N is sufficiently large.
-   Parameters are in the interior of the parameter space.

For the first condition, type I error rates and confidence interval
coverage rates improve as N increases. For a given N, type I error rates
are **close** to alpha and coverage rates are **close** to the
confidence level. This is the reason all tests require a sample size of
at least 50. For the second condition, the parameter must not be near
the boundary of the parameter space. How near is too near depends on N.
There is no clear cut off point.

As implemented, all functions depend on the
*χ*<sup>2</sup>
approximation. To get a sense of performance, lets compare the
likelihood method to the exact method. Here, X is normally distributed
with mu equal to 3 and standard deviation equal to 2.

``` r
set.seed(1)
x <- rnorm(n = 50, mean = 3, sd = 2)
exactTest <- t.test(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
likelihoodTest <- gaussian_mu_lr_test(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
```

The two intervals are similar.

``` r
as.numeric(exactTest$conf.int)
#> [1] 2.728337 3.673456
likelihoodTest$conf.int
#> [1] 2.735731 3.666063
```

Lets compare tests for variance. Again, confidence intervals are
similar.

``` r
sigma2 <- 1.5^2 # Variance, not standard deviation.
exactTest <- EnvStats::varTest(x = x, sigma.squared = sigma2,  alternative = "two.sided", conf.level = .95)
likelihoodTest <- gaussian_variance_lr_test(x = x, sigma.squared = sigma2, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 1.929274 4.293414
likelihoodTest$conf.int
#> [1] 1.875392 4.121238
```

The greatest strength of the likelihood method is the generality. There
are a total of 12 tests and 12 confidence intervals implemented in this
package. Some tests (cauchy, beta, gamma, poisson) don’t have other well
known options.

Estimated asymptotic type I and type II error rates can be found
[here](https://github.com/gmcmacran/TypeOneTypeTwoSim).
