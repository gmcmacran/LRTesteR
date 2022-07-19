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
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(50, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .04)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- negative_binomial_p_lr_test(10, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(10, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  test <- negative_binomial_p_lr_test(90, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(90, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Input checking
###############################################

test_that("failure input checking works", {
  expect_error(negative_binomial_p_lr_test("foo"), "Argument num_failures should be numeric.")
  expect_error(negative_binomial_p_lr_test(c(5, 4)), "Argument num_failures should have length 1.")
  expect_error(negative_binomial_p_lr_test(-2), "Argument num_failures should be 0 or positive.")
})

test_that("success input checking works", {
  expect_error(negative_binomial_p_lr_test(5, "foo"), "Argument num_success should be numeric.")
  expect_error(negative_binomial_p_lr_test(5, c(5, 4)), "Argument num_success should have length 1.")
  expect_error(negative_binomial_p_lr_test(5, -1), "Argument num_success should be 0 or positive.")
})

set.seed(1)
test_that("p input checking works", {
  expect_error(negative_binomial_p_lr_test(5, 6, "foo"), "Argument p should be numeric.")
  expect_error(negative_binomial_p_lr_test(5, 6, c(.5, .6)), "Argument p should have length one.")
  expect_error(negative_binomial_p_lr_test(5, 6, 0), "Argument p should be positive.")
  expect_error(negative_binomial_p_lr_test(5, 6, 1.01), "Argument p should be less than or equal to 1.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(negative_binomial_p_lr_test(5, 6, .5, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(negative_binomial_p_lr_test(5, 6, .5, 1), "Argument alternative should be a character.")
  expect_error(negative_binomial_p_lr_test(5, 6, .5, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})
