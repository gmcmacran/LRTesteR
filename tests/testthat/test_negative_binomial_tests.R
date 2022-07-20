exact_test <- function(num_failures, num_success, p, alternative) {
  calc_two_sided_p_value <- function(x, size, prob) {
    if (prob == 1) {
      (as.numeric(x == 0))
    } else {
      relErr <- 1 + 1e-07
      d <- dnbinom(x, size, prob)
      m <- size * (1 - prob) / prob
      if (x == m) {
        1
      } else if (x < m) {
        nearInf <- ceiling(m * 20)
        i <- seq.int(from = ceiling(m), to = nearInf)
        y <- sum(dnbinom(i, size, prob) <= d * relErr)
        pnbinom(x, size, prob) + pnbinom(pmax(nearInf - y, 0), size, prob, lower.tail = FALSE)
      } else {
        i <- seq.int(from = 0, to = floor(m))
        y <- sum(dnbinom(i, size, prob) <= d * relErr)
        pnbinom(y - 1, size, prob) + pnbinom(x - 1, size, prob, lower.tail = FALSE)
      }
    }
  }

  calc_left_p_value <- function(x, size, prob) {
    pnbinom(q = x, size = size, prob = prob, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, size, prob) {
    pnbinom(q = x - 1, size = size, prob = prob, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(num_failures, num_success, p)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(num_failures, num_success, p)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(num_failures, num_success, p)
  }
  return(list(p.value = p.value))
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  test <- negative_binomial_p_lr_test(50, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(50, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .04)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), negative_binomial_p_lr_test(50, 50, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), negative_binomial_p_lr_test(50, 50, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- negative_binomial_p_lr_test(10, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(10, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), negative_binomial_p_lr_test(10, 50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), negative_binomial_p_lr_test(10, 50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  test <- negative_binomial_p_lr_test(90, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test(90, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), negative_binomial_p_lr_test(90, 50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), negative_binomial_p_lr_test(90, 50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Input checking
###############################################

test_that("failure input checking works", {
  expect_error(negative_binomial_p_lr_test("foo"), "First argument should be numeric.")
  expect_error(negative_binomial_p_lr_test(c(5, 4)), "First argument should have length 1.")
  expect_error(negative_binomial_p_lr_test(2.5), "First argument should be an integer.")
  expect_error(negative_binomial_p_lr_test(-1), "First argument should be 0 or above.")
})

test_that("success input checking works", {
  expect_error(negative_binomial_p_lr_test(1, "foo"), "Second argument should be numeric.")
  expect_error(negative_binomial_p_lr_test(1, c(5, 4)), "Second argument should have length 1.")
  expect_error(negative_binomial_p_lr_test(1, 2.5), "Second argument should be an integer.")
  expect_error(negative_binomial_p_lr_test(1, -1), "Second argument should be 0 or above.")
})

test_that("p input checking works", {
  expect_error(negative_binomial_p_lr_test(1, 1, "foo"), "Argument p should be numeric.")
  expect_error(negative_binomial_p_lr_test(1, 1, c(.5, .6)), "Argument p should have length one.")
  expect_error(negative_binomial_p_lr_test(1, 1, -.1), "Argument p should be between 0 and 1.")
  expect_error(negative_binomial_p_lr_test(1, 1, 1.01), "Argument p should be between 0 and 1.")
})

test_that("alternative input checking works", {
  expect_error(negative_binomial_p_lr_test(5, 6, .5, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(negative_binomial_p_lr_test(5, 6, .5, 1), "Argument alternative should be a character.")
  expect_error(negative_binomial_p_lr_test(5, 6, .5, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(negative_binomial_p_lr_test(5, 10, .5, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(negative_binomial_p_lr_test(5, 10, .5, "less", "foo"), "conf.level should be numeric.")
  expect_error(negative_binomial_p_lr_test(5, 10, .5, "less", 0), "conf.level should between zero and one.")
  expect_error(negative_binomial_p_lr_test(5, 10, .5, "less", 1), "conf.level should between zero and one.")
})
