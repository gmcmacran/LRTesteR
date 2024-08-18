###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- empirical_quantile_one_sample(x, .50, 0, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
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
    expect_true(ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .5, CI1, alt)$p.value, .05) >= .04)
    expect_true(ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .5, CI2, alt)$p.value, .05) >= .04)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- empirical_quantile_one_sample(x, .50, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- empirical_quantile_one_sample(x, .50, -2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_quantile_one_sample(c()), "Argument x should have positive length.")
  expect_error(empirical_quantile_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("Q input checking works", {
  expect_error(empirical_quantile_one_sample(x, c(1, 2)), "The Q parameter should have length one.")
  expect_error(empirical_quantile_one_sample(x, "foo"), "The Q parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, 0), "Q should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, 1), "Q should between zero and one.")
})

test_that("value input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, c(1, 2)), "The value parameter should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, "foo"), "he value parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, .50, min(x)), "The value parameter must be greater than the min of x.")
  expect_error(empirical_quantile_one_sample(x, .50, max(x)), "The value parameter must be less than the max of x.")
})


test_that("alternative input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_quantile_one_way(x, .50, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_quantile_one_sample(x[which(fctr == 1)], .50, 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(2)
x <- c(rnorm(50, 1.5, 1), rnorm(50, 2, 1), rnorm(50, 2.25, 1))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_quantile_one_way(x, .50, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_quantile_one_sample(x[which(fctr == 1)], .50, 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_quantile_one_way(c()), "Argument x should have positive length.")
  expect_error(empirical_quantile_one_way("foo"), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("Q input checking works", {
  expect_error(empirical_quantile_one_way(x, c(1, 2)), "The Q parameter should have length one.")
  expect_error(empirical_quantile_one_way(x, "foo"), "The Q parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, 0), "Q should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, 1), "Q should between zero and one.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, "foo"), "Argument fctr should have same length as x.")
  expect_error(empirical_quantile_one_way(x, .50, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(empirical_quantile_one_way(x, .50, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(empirical_quantile_one_way(x, .50, fctr1), "Argument fctr should have at least two unique values.")
})
rm(fctr1)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, "foo"), "conf.level should be numeric.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, 0), "conf.level should between zero and one.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, 1), "conf.level should between zero and one.")
})

x <- 1:10
fctr <- c(rep(1, 5), rep(2, 5))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, fctr, .95), "Every group in x must have at least one data point less than the tested quantile.")
})
rm(x, fctr)

x <- 1:10
x[5] <- 4
x[6] <- 3
fctr <- c(rep(1, 5), rep(2, 5))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, fctr, .95), "Every group in x must have at least one data point greater than the tested quantile.")
})
rm(x, fctr)
