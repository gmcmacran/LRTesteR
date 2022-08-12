###############################################
# Print a few things
###############################################
args_00 <- names(formals(base::print))
args_01 <- names(formals(print.lrtest))
test_that("Arguments correct", {
  expect_true(all(args_00 == args_01))
})

set.seed(1)
x <- rnorm(100, 0, 1)
test <- gaussian_mu_one_sample(x, 0, "two.sided")

test_that("works for one sample.", {
  expect_output(print(test))
})

set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gaussian_mu_one_way(x, fctr, .95)

test_that("works for one way.", {
  expect_output(print(test))
})
