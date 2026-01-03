
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LRTesteR Overview

<!-- badges: start -->

[![R-CMD-check](https://github.com/gmcmacran/LRTesteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gmcmacran/LRTesteR/actions/workflows/R-CMD-check.yaml)
[![CRAN
status](https://www.r-pkg.org/badges/version/LRTesteR)](https://cran.r-project.org/package=LRTesteR)
<!-- badges: end -->

LRTesteR provides likelihood ratio tests and confidence intervals for
many common distributions.

# Example 1: Test lambda of a poisson distribution

To test lambda, simply call poisson_lambda_one_sample.

``` r
library(LRTesteR)

set.seed(1)
x <- rpois(n = 100, lambda = 1)
poisson_lambda_one_sample(x = x, lambda = 1, alternative = "two.sided")
#> Log Likelihood Statistic: 0.01
#> p value: 0.92
#> Confidence Level: 95%
#> Confidence Interval: (0.826, 1.22)
```

# Example 2: Confidence Interval

To get a confidence interval, set the conf.level to the desired
confidence. Below gets a two sided 90% confidence interval for scale
from a Cauchy random variable.

``` r
set.seed(1)
x <- rcauchy(n = 100, location = 3, scale = 5)
cauchy_scale_one_sample(x = x, scale = 5, alternative = "two.sided", conf.level = .90)
#> Log Likelihood Statistic: 1.21
#> p value: 0.271
#> Confidence Level: 90%
#> Confidence Interval: (4.64, 7.284)
```

Setting alternative to “less” gets a lower one sided interval.

``` r
cauchy_scale_one_sample(x = x, scale = 5, alternative = "less", conf.level = .90)
#> Log Likelihood Statistic: 1.1
#> p value: 0.865
#> Confidence Level: 90%
#> Confidence Interval: (0, 6.93)
```

Setting it to “greater” gets an upper one sided interval.

``` r
cauchy_scale_one_sample(x = x, scale = 5, alternative = "greater", conf.level = .90)
#> Log Likelihood Statistic: 1.1
#> p value: 0.135
#> Confidence Level: 90%
#> Confidence Interval: (4.878, Inf)
```

# Example 3: One-way Analysis

One-way ANOVA is generalized to all distributions. Here gamma random
variables are created with different shapes. The one way test has a
small p value and provides confidence intervals with 95% confidence for
the whole set.

``` r
set.seed(1)
x <- c(rgamma(n = 50, shape = 1, rate = 2), rgamma(n = 50, shape = 2, rate = 2), rgamma(n = 50, shape = 3, rate = 2))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
gamma_shape_one_way(x = x, fctr = fctr, conf.level = .95)
#> Log Likelihood Statistic: 68.59
#> p value: 0
#> Confidence Level Of Set: 95%
#> Individual Confidence Level: 98.3%
#> Confidence Interval For Group 1: (0.65, 1.515)
#> Confidence Interval For Group 2: (1.376, 3.376)
#> Confidence Interval For Group 3: (1.691, 4.192)
```

# Example 4: Empirical Likelihood

The empirical likelihood tests do not require any distributional
assumptions and work with less data.

``` r
set.seed(1)
x <- rnorm(n = 25, mean = 1, sd = 1)
empirical_mu_one_sample(x = x, mu = 1, alternative = "two.sided")
#> Log Likelihood Statistic: 0.73
#> p value: 0.392
#> Confidence Level: 95%
#> Confidence Interval: (0.752, 1.501)
```

# The χ<sup>2</sup> approximation

As implemented, all functions depend on the asymptotic χ<sup>2</sup>
approximation. To get a sense of accuracy of this approximation for
large samples, the likelihood tests are compared to the exact tests.

X is normally distributed with μ equal to 3 and standard deviation equal
to 2. The two intervals for μ are similar.

``` r
set.seed(1)
x <- rnorm(n = 500, mean = 3, sd = 2)
exactTest <- t.test(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
likelihoodTest <- gaussian_mu_one_sample(x = x, mu = 2.5, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 2.867461 3.223115
likelihoodTest$conf.int
#> [1] 2.867729 3.222847
```

The confidence intervals for variance are similar as well.

``` r
set.seed(1)
x <- rnorm(n = 500, mean = 3, sd = 2)
sigma2 <- 1.5^2 # Variance, not standard deviation.
exactTest <- EnvStats::varTest(x = x, sigma.squared = sigma2, alternative = "two.sided", conf.level = .95)
likelihoodTest <- gaussian_variance_one_sample(x = x, sigma.squared = sigma2, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 3.631734 4.655834
likelihoodTest$conf.int
#> [1] 3.620303 4.639384
```

Changing to p for a binomial random variable, the confidence intervals
are similar yet again.

``` r
exactTest <- stats::binom.test(x = 250, n = 500, p = .50, alternative = "two.sided", conf.level = .95)
likelihoodTest <- binomial_p_one_sample(x = 250, n = 500, p = .50, alternative = "two.sided", conf.level = .95)
as.numeric(exactTest$conf.int)
#> [1] 0.4552856 0.5447144
likelihoodTest$conf.int
#> [1] 0.4562579 0.5437421
```

When sample size is small, similarity will decrease. When exact methods
are available, they are the better option. The utility of the likelihood
based approach is its generality. Many tests in this package don’t have
other well known options.

Estimated asymptotic type I and type II error rates can be found
[here](https://github.com/gmcmacran/TypeOneTypeTwoSim).
