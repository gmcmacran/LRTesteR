# The sum of a N iid poisson random variables
# is a poisson random variable with lambda of sum = N*lambda
exact_test <- function(x, lambda, alternative) {
  # sum of x
  lambda <- lambda * length(x)
  x <- sum(x)

  calc_two_sided_p_value <- function(x, lambda) {
    relErr <- 1 + 1e-07
    d <- dpois(x, lambda)
    m <- lambda
    if (x == m) {
      1
    } else if (x < m) {
      nearInf <- ceiling(m * 20)
      i <- seq.int(from = ceiling(m), to = nearInf)
      y <- sum(dpois(i, lambda) <= d * relErr)
      ppois(x, lambda) + ppois(pmax(nearInf - y, 0), lambda, lower.tail = FALSE)
    } else {
      i <- seq.int(from = 0, to = floor(m))
      y <- sum(dpois(i, lambda) <= d * relErr)
      ppois(y - 1, lambda) + ppois(x - 1, lambda, lower.tail = FALSE)
    }
  }


  calc_left_p_value <- function(x, lambda) {
    ppois(q = x, lambda = lambda, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, lambda) {
    ppois(q = x - 1, lambda = lambda, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(x, lambda)
  }
  if (alternative == "greater") {
    p.value <- calc_right_p_value(x, lambda)
  }
  if (alternative == "less") {
    p.value <- calc_left_p_value(x, lambda)
  }
  return(list(p.value = p.value))
}



###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rpois(200, 1)
  test <- poisson_lambda_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 1, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .03)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), poisson_lambda_lr_test(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), poisson_lambda_lr_test(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rpois(2000, 3)
  test <- poisson_lambda_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 1, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), poisson_lambda_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), poisson_lambda_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rpois(200, 1)
  test <- poisson_lambda_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 3, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), poisson_lambda_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), poisson_lambda_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(poisson_lambda_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(poisson_lambda_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("lambda input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), c(1, 2)), "The tested parameter should have length one.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), "foo"), "The tested parameter should be numeric.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, 1), "Argument alternative should be a character.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "less", 0), "conf.level should between zero and one.")
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "less", 1), "conf.level should between zero and one.")
})
