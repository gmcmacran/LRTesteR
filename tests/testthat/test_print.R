###############################################
# Print a few things
###############################################
args_00 <- names(formals(base::print))
args_01 <- names(formals(print.lrtest))
test_that("Arguments correct", {
  expect_true(all(args_00 == args_01))
})

#############
# one sample case one
#############
set.seed(1)
x <- rnorm(100, 0, 1)
test <- gaussian_mu_one_sample(x, 0, "two.sided")

test_that("works for one sample.", {
  expect_output(print(test))
})

#############
# one sample case two
#############
test <- binomial_p_one_sample(52, 100, .50, "two.sided")

test_that("works for one sample.", {
  expect_output(print(test))
})

#############
# one sample case three
#############
set.seed(1)
x <- rnorm(25, 0, 1)
test <- empirical_mu_one_sample(x, 0, "two.sided")

test_that("works for one sample.", {
  expect_output(print(test))
})

#############
# one way case one
#############
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gaussian_mu_one_way(x, fctr, .95)

test_that("works for one way.", {
  expect_output(print(test))
})

#############
# one way case two
#############
set.seed(1)
x <- rbinom(3, 50, .5)
n <- rep(50, length(x))
fctr <- factor(seq(1, length(x)))
test <- binomial_p_one_way(x, n, fctr, .95)

test_that("works for one way.", {
  expect_output(print(test))
})

#############
# one way case three
#############
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_mu_one_way(x, fctr, .95)

test_that("works for one way.", {
  expect_output(print(test))
})
