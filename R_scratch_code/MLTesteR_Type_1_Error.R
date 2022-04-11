#############################################
# Overview
#
# A script to test the type I error rate
# of likelihood ratio tests.
#############################################

library(MLTesteR)

################
# Beta
################
B <- 5000
N <- 200
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rbeta(N, shape1 = 2, shape2 = 2)
  test <- beta_shape1_lr_test(x, 2, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 200
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rbeta(N, shape1 = 2, shape2 = 2)
  test <- beta_shape2_lr_test(x, 2, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

################
# gamma
################
# x <- rgamma(n = N, shape = 10, rate = 3)
# x <- rgamma(n = N, shape = 10, scale = 3)
# x <- rgamma(n = N, shape = 10, scale = 3)

B <- 5000
N <- 200
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = 50, rate = 2)
  test <- gamma_rate_lr_test(x, 2, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 200
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = 50, scale = .5)
  test <- gamma_scale_lr_test(x, .5, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 200
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = 50, scale = .5)
  test <- gamma_shape_lr_test(x, 50, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

################
# gaussian
################
B <- 5000
N <- 500
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rnorm(N)
  test <- gaussian_mean_lr_test(x, 0, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 500
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rnorm(N)
  test <- gaussian_variance_lr_test(x, 1, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

################
# poisson
################
B <- 5000
N <- 500
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rpois(N, 1)
  test <- poisson_lambda_lr_test(x, 1, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)
