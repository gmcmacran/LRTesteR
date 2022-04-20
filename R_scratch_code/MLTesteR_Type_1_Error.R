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
B <- 5000
N <- 100
shape <- 50
rate <- 2
scale <- 1 / rate

pvalues <- vector(mode = "numeric", length = B)
set.seed(2)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = shape, rate = rate)
  test <- gamma_rate_lr_test(x, rate, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 100
pvalues <- vector(mode = "numeric", length = B)
set.seed(2)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = shape, scale = scale)
  test <- gamma_scale_lr_test(x, scale, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)

B <- 5000
N <- 100
pvalues <- vector(mode = "numeric", length = B)
set.seed(1)
for (i in seq_along(1:B)) {
  x <- rgamma(n = N, shape = shape, scale = scale)
  test <- gamma_shape_lr_test(x, shape, "two.sided")
  pvalues[i] <- test$p.value
}
mean(pvalues <= .05)
head(pvalues)

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
