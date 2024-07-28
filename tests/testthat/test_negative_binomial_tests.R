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
  test <- negative_binomial_p_one_sample(50, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_two", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_02 <- exact_test(50, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .04)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), negative_binomial_p_one_sample(50, 50, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), negative_binomial_p_one_sample(50, 50, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- negative_binomial_p_one_sample(10, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_two", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_02 <- exact_test(10, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), negative_binomial_p_one_sample(10, 50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), negative_binomial_p_one_sample(10, 50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  test <- negative_binomial_p_one_sample(90, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_two", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_02 <- exact_test(90, 50, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), negative_binomial_p_one_sample(90, 50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), negative_binomial_p_one_sample(90, 50, CI2, alt)$p.value, .05)
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
  expect_error(negative_binomial_p_one_sample("foo"), "First argument should be numeric.")
  expect_error(negative_binomial_p_one_sample(c(5, 4)), "First argument should have length 1.")
  expect_error(negative_binomial_p_one_sample(2.5), "First argument should be an integer.")
  expect_error(negative_binomial_p_one_sample(-1), "First argument should be 0 or above.")
})

test_that("success input checking works", {
  expect_error(negative_binomial_p_one_sample(1, "foo"), "Second argument should be numeric.")
  expect_error(negative_binomial_p_one_sample(1, c(5, 4)), "Second argument should have length 1.")
  expect_error(negative_binomial_p_one_sample(1, 2.5), "Second argument should be an integer.")
  expect_error(negative_binomial_p_one_sample(1, -1), "Second argument should be 0 or above.")
  expect_error(negative_binomial_p_one_sample(1, 10), "num_failures plus num_successes should be at least 30 for likelihood ratio test.")
  expect_error(negative_binomial_p_one_sample(50, 0), "There must be at least one success.")
})

test_that("p input checking works", {
  expect_error(negative_binomial_p_one_sample(1, 50, "foo"), "Argument p should be numeric.")
  expect_error(negative_binomial_p_one_sample(1, 50, c(.5, .6)), "Argument p should have length one.")
  expect_error(negative_binomial_p_one_sample(1, 50, -.1), "Argument p should be between 0 and 1.")
  expect_error(negative_binomial_p_one_sample(1, 50, 1.01), "Argument p should be between 0 and 1.")
})

test_that("alternative input checking works", {
  expect_error(negative_binomial_p_one_sample(5, 50, .5, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(negative_binomial_p_one_sample(5, 50, .5, 1), "Argument alternative should be a character.")
  expect_error(negative_binomial_p_one_sample(5, 50, .5, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(negative_binomial_p_one_sample(5, 50, .5, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(negative_binomial_p_one_sample(5, 50, .5, "less", "foo"), "conf.level should be numeric.")
  expect_error(negative_binomial_p_one_sample(5, 50, .5, "less", 0), "conf.level should between zero and one.")
  expect_error(negative_binomial_p_one_sample(5, 50, .5, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
num_failures <- rnbinom(3, 50, .5)
num_success <- rep(50, length(num_failures))
fctr <- factor(seq(1, length(num_failures)))
test <- negative_binomial_p_one_way(num_failures, num_success, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_two", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})


test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- negative_binomial_p_one_sample(num_failures[which(fctr == 1)], 50, .50, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################
set.seed(1)
num_failures <- rnbinom(3, 50, c(.25, .50, .75))
num_success <- rep(50, length(num_failures))
fctr <- factor(seq(1, length(num_failures)))
test <- negative_binomial_p_one_way(num_failures, num_success, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_two", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value <= .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- negative_binomial_p_one_sample(num_failures[which(fctr == 1)], 50, .50, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("First argument input checking works", {
  expect_error(negative_binomial_p_one_way(c(), c()), "First argument should have positive length.")
  expect_error(negative_binomial_p_one_way("foo", "bar"), "First argument should be numeric.")
  expect_error(negative_binomial_p_one_way(1.5, 3), "First argument should only contain integers.")
  expect_error(negative_binomial_p_one_way(-1L, 3), "All elements in first argument should be 0 or above.")
})

test_that("Second argument input checking works", {
  expect_error(binomial_p_one_way(c(10, 10), 20), "The first two arguments should have the same length.")
  expect_error(negative_binomial_p_one_way(10, "foo"), "Second argument should be numeric.")
  expect_error(negative_binomial_p_one_way(10, 1.5), "Second argument should only contain integers.")
  expect_error(negative_binomial_p_one_way(10, -1L), "All elements in second argument should be 0 or above.")
  expect_error(negative_binomial_p_one_way(10, 39), "num_failures plus num_successes should be at least 60 for likelihood ratio test.")
})


fctr1 <- factor(rep(1, 100))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)))
test_that("fctr input checking works", {
  expect_error(negative_binomial_p_one_way(10, 50, "foo"), "Argument fctr should be a factor.")
  expect_error(negative_binomial_p_one_way(10, 50, rep("foo", 100)), "Argument fctr should have same length as first argument.")
  expect_error(negative_binomial_p_one_way(10, 50, fctr1), "Argument fctr should have same length as first argument.")
  expect_error(negative_binomial_p_one_way(10, 50, fctr2), "Argument fctr should have same length as first argument.")
})
rm(fctr1, fctr2)

set.seed(1)
num_failures <- rnbinom(3, 50, .50)
num_success <- rep(50, length(num_failures))
fctr <- factor(seq(1, length(num_failures)))
test_that("conf.level input checking works", {
  expect_error(negative_binomial_p_one_way(num_failures, num_success, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(negative_binomial_p_one_way(num_failures, num_success, fctr, "foo"), "conf.level should be numeric.")
  expect_error(negative_binomial_p_one_way(num_failures, num_success, fctr, 0), "conf.level should between zero and one.")
  expect_error(negative_binomial_p_one_way(num_failures, num_success, fctr, 1), "conf.level should between zero and one.")
})
