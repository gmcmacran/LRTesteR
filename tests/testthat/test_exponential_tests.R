# The sum of iid exponential r.v. is gamma w/
# shape = n and rate = rate of exponential
exact_test <- function(x, rate, alternative) {
  calc_two_sided_p_value <- function(x, rate) {
    2 * pmin(
      stats::pgamma(q = sum(x), shape = length(x), rate = rate, lower.tail = TRUE),
      stats::pgamma(q = sum(x), shape = length(x), rate = rate, lower.tail = FALSE)
    )
  }

  calc_left_p_value <- function(x, rate) {
    stats::pgamma(q = sum(x), shape = length(x), rate = rate, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, rate) {
    stats::pgamma(q = sum(x), shape = length(x), rate = rate, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(x, rate)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(x, rate)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(x, rate)
  }
  return(list(p.value = p.value))
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rexp(200, 1)
  test <- exponentail_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x, 1, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .02)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rexp(200, 3)
  test <- exponentail_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x, 1, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rexp(200, 1)
  test <- exponentail_rate_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(x, 3, alt)
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
  expect_error(exponentail_rate_lr_test(c()), NULL)
  expect_error(exponentail_rate_lr_test(rep("foo", 50)), NULL)
  expect_error(exponentail_rate_lr_test(rexp(49)), NULL)
})

set.seed(1)
test_that("rate input checking works", {
  expect_error(exponentail_rate_lr_test(rexp(50), c(1, 2)), NULL)
  expect_error(exponentail_rate_lr_test(rexp(50), "foo"), NULL)
  expect_error(exponentail_rate_lr_test(rexp(50), 0), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(exponentail_rate_lr_test(rexp(50), 1, c("two.sided", "less")), NULL)
  expect_error(exponentail_rate_lr_test(rexp(50), 1, 1), NULL)
  expect_error(exponentail_rate_lr_test(rexp(50), 1, "lesss"), NULL)
})
