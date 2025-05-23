% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/poisson_tests.R
\name{poisson_lambda_one_sample}
\alias{poisson_lambda_one_sample}
\title{Test the lambda parameter of a poisson distribution.}
\source{
\itemize{
\item \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
\item Yudi Pawitan. In All Likelihood. Oxford University Press.
\item Hodd, McKean, and Craig. Introduction to Mathematical Statistics. Pearson.
}
}
\usage{
poisson_lambda_one_sample(
  x,
  lambda,
  alternative = "two.sided",
  conf.level = 0.95
)
}
\arguments{
\item{x}{a numeric vector of data.}

\item{lambda}{a number indicating the tested value of lambda}

\item{alternative}{a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".}

\item{conf.level}{confidence level of the likelihood interval.}
}
\value{
An S3 class containing the test statistic, p value, likelihood based confidence interval, and alternative
hypothesis.
}
\description{
Test the lambda parameter of a poisson distribution.
}
\examples{
library(LRTesteR)

# Null is true
set.seed(1)
x <- rpois(100, 1)
poisson_lambda_one_sample(x, 1, "two.sided")

# Null is false
set.seed(1)
x <- rpois(100, 2)
poisson_lambda_one_sample(x, 1, "greater")
}
