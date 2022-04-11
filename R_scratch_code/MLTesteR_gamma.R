########################################################
# Overview
#
# Script to test gamma lr test
# Per wiki, xbar ~ (1/n)*sum(X)
# https://en.wikipedia.org/wiki/Gamma_distribution
#
# If X ~ Gamma(1, 1/λ) (in the shape–scale parametrization), then X has an exponential distribution with rate parameter λ.
########################################################

library(MLTesteR)

# If X ~ Gamma(1, 1/theta) (in the shape–scale parametrization), then X has an exponential distribution with rate parameter theta.
# See page 254 of in all likelihood book.

set.seed(2)
B <- 5000
N <- 5
shape <- 1
thetas <- c(.35, .55, 2.50, 4.00)

for (theta in thetas) {
  testStats <- vector(mode = "numeric", length = B)
  scale  <- 1/theta
  for (i in 1:B) {
    x <- rgamma(n = N, shape = shape, scale  = scale ) # exponential
    test <- gamma_scale_lr_test(x = x, scale  = scale , alternative = "two.sided")
    testStats[i] <- test$statistic
  }
  print("____________________")
  print(round(mean(testStats), 2))
  print("____________________")
  print("")
}


