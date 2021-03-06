% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/beta_tests.R
\name{beta_shape2_lr_test}
\alias{beta_shape2_lr_test}
\title{Test the shape2 parameter of a beta distribution using the likelihood ratio test.}
\source{
\url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
}
\usage{
beta_shape2_lr_test(x, shape2, alternative = "two.sided", conf.level = 0.95)
}
\arguments{
\item{x}{a numeric vector of at least 50 data values.}

\item{shape2}{a number indicating the tested value of the shape2 parameter.}

\item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}

\item{conf.level}{confidence level of the likelihood interval.}
}
\value{
An S3 class containing the test statistic, p value, likelihood based confidence interval, and alternative
hypothesis.
}
\description{
Test the shape2 parameter of a beta distribution using the likelihood ratio test.
}
\examples{
library(LRTesteR)

# Null is true
set.seed(1)
x <- rbeta(100, shape1 = 1, shape2 = 1)
beta_shape2_lr_test(x, 1, "two.sided")

# Null is false
set.seed(1)
x <- rbeta(100, shape1 = 1, shape2 = 3)
beta_shape2_lr_test(x, 1, "greater")
}
