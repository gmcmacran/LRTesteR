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
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 1, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .03)
  })
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
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 1, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rpois(200, 1)
  test <- poisson_lambda_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x = x, lambda = 3, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(poisson_lambda_lr_test(c()), NULL)
  expect_error(poisson_lambda_lr_test(rep("foo", 50)), NULL)
  expect_error(poisson_lambda_lr_test(rpois(49, lambda = 1)), NULL)
})

set.seed(1)
test_that("lambda input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), c(1, 2)), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), "foo"), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 0), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, c("two.sided", "less")), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, 1), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "lesss"), NULL)
})
