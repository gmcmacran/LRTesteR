###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rnorm(200, 0, 1)
  test <- empirical_skewness_one_sample(x, 0, alt)

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

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), empirical_skewness_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), empirical_skewness_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rexp(200, 1)
  test <- empirical_skewness_one_sample(x, 0, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_skewness_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_skewness_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- -1 * rexp(200, 1)
  test <- empirical_skewness_one_sample(x, 0, alt)

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Invariance
###############################################
# The test statistic is location and scale invariant.
set.seed(5)
x <- rgamma(30, 3, 2)
test_that("Check invariance", {
  expect_equal(
    empirical_skewness_one_sample(x, .5)$statistic,
    empirical_skewness_one_sample(10 + 7 * x, .5)$statistic,
    tolerance = .1^4
  )
})

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_skewness_one_sample(c(1, 2, 3)), "Argument x should have at least four observations.")
  expect_error(empirical_skewness_one_sample(rep("foo", 50)), "Argument x should be numeric.")
  expect_error(empirical_skewness_one_sample(rep(1, 50), 0), "Argument x should have at least two unique values.")
})

set.seed(1)
x <- rnorm(50)
test_that("skewness input checking works", {
  expect_error(empirical_skewness_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(empirical_skewness_one_sample(x, "foo"), "The tested parameter should be numeric.")
})

test_that("alternative input checking works", {
  expect_error(empirical_skewness_one_sample(x, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_skewness_one_sample(x, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_skewness_one_sample(x, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_skewness_one_sample(x, 0, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_skewness_one_sample(x, 0, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_skewness_one_sample(x, 0, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_skewness_one_sample(x, 0, "less", 1), "conf.level should between zero and one.")
})
