
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
![\chi^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cchi%5E2 "\chi^2")
approximation even when exact sampling distributions are known. Tests
require a sample size of at least 50. Estimated asymptotic type I and
type II error rates can be found
[here](https://github.com/gmcmacran/TypeOneTypeTwoSim).

All functions match popular tests in R. If you are familiar with t.test
and binom.test, you already know how to use these functions.

# Implemented Tests and Confidence Intervals

-   beta
    -   shape 1
    -   shape 2
-   binomial
    -   p
-   exponential
    -   rate
-   gamma
    -   rate
    -   scale
    -   shape
-   Gaussian
    -   mu
    -   variance
-   negative binomial
    -   p
-   Poisson
    -   lambda
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

To test lambda, simply call poisson_lambda_one_sample.

``` r
poisson_lambda_one_sample(x = x, lambda = 1, alternative = "two.sided")
#> [1] "Likelihood Statistic: 0.01"
#> [1] "p value: 0.92"
#> [1] "Confidence Level: 95%"
#> [1] "Confidence Interval: (0.826, 1.22)"
```

Because we generated the data, we know the true value of lambda is one.
The p value is above 5%.

# Example 2: Confidence Interval

To get a confidence interval, set the conf.level to the desired
confidence. Below gets a two sided 90% confidence interval for scale
from a Cauchy random variable.

``` r
set.seed(1)
x <- rcauchy(n = 100, location = 3, scale = 5)
cauchy_scale_one_sample(x = x, scale = 5, alternative = "two.sided", conf.level = .90)
#> [1] "Likelihood Statistic: 1.21"
#> [1] "p value: 0.271"
#> [1] "Confidence Level: 90%"
#> [1] "Confidence Interval: (4.64, 7.284)"
```

Setting alternative to “less” gets a lower one sided interval. Setting
it to “greater” gets an upper one sided interval.

# Example 3: One Way Analysis

One way ANOVA is generalized to all distributions. Here gamma random
variables are created with different shapes. The one way test has a
small p value and provides confidence intervals with 95% confidence for
the whole set.

``` r
set.seed(1)
x <- c(rgamma(50, 1, 2), rgamma(50, 2, 2), rgamma(50, 3, 2))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
gamma_shape_one_way(x, fctr, .95)
#> [1] "Likelihood Statistic: 68.59"
#> [1] "p value: 0"
#> [1] "Confidence Level Of Set: 95%"
#> [1] "Individual Confidence Level: 98.3%"
#> [1] "Confidence Interval For Group 1: (0.65, 1.515)"
#> [1] "Confidence Interval For Group 2: (1.376, 3.376)"
#> [1] "Confidence Interval For Group 3: (1.691, 4.192)"
```

# Mathematical Details

The strength of the likelihood ratio test is its generality. It is a
recipe to create hypothesis tests and confidence intervals in many
different settings. Sometimes the test is the only known procedure. When
there are many procedures, likelihood ratio tests have very competitive
**asymptotic** power.

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

As implemented, all functions depend on the
![\chi^2](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cchi%5E2 "\chi^2")
approximation. To get a sense of performance, lets compare the
likelihood method to the exact method. Here, X is normally distributed
with mu equal to 3 and standard deviation equal to 2.

``` r
set.seed(1)
x <- rnorm(n = 50, mean = 3, sd = 2)
exactTest <- t.test(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
likelihoodTest <- gaussian_mu_one_sample(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
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
likelihoodTest <- gaussian_variance_one_sample(x = x, sigma.squared = sigma2, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 1.929274 4.293414
likelihoodTest$conf.int
#> [1] 1.875392 4.121238
```

Changing to p for a binomial distribution, the confidence intervals are
similar yet again.

``` r
exactTest <- stats::binom.test(x = 10, n = 50,  p = .50, alternative = "two.sided", conf.level = .95)
likelihoodTest <- binomial_p_one_sample(x = 10, n = 50,  p = .50, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 0.1003022 0.3371831
likelihoodTest$conf.int
#> [1] 0.1056842 0.3242910
```

When exact methods are known, use them. The utility of the likelihood
based approach is its generality. Many tests in this package (cauchy,
beta, gamma, poisson) don’t have other well known options.

Asymptotic type I and type II error rates can be found
[here](https://github.com/gmcmacran/TypeOneTypeTwoSim).
