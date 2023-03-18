###############################################
# Null True (mean test)
###############################################
stat.fun <- function(x) {
  return(mean(x))
}

for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- bootstrap_one_sample(x, 0, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_mu_one_sample(x, 0, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .02)
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
    expect_true(ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05) >= .0498)
    expect_true(ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05) >= .0498)
  })
  rm(CI1, CI2)
}

###############################################
# Null False (mean test)
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- bootstrap_one_sample(x, 0, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_mu_one_sample(x, 0, alt)
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
    ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05),
    ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- bootstrap_one_sample(x, -2, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_mu_one_sample(x, 0, alt)
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
    ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05),
    ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Null True (variance test)
###############################################
stat.fun <- function(x) {
  return(sum((x - mean(x))^2) / length(x))
}

for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 3)
  test <- bootstrap_one_sample(x, 9, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_variance_one_sample(x, 9, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .02)
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
    expect_true(ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05) >= .045)
    expect_true(ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05) >= .045)
  })
  rm(CI1, CI2)
}

###############################################
# Null False (variance test)
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(2)
  x <- rnorm(200, 0, 3)
  test <- bootstrap_one_sample(x, 8, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_variance_one_sample(x, 8, alt)
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
    ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05),
    ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 3)
  test <- bootstrap_one_sample(x, 10, stat.fun, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_four", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- gaussian_variance_one_sample(x, 10, alt)
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
    ifelse(is.finite(CI1), bootstrap_one_sample(x, CI1, stat.fun, alt)$p.value, .05),
    ifelse(is.finite(CI2), bootstrap_one_sample(x, CI2, stat.fun, alt)$p.value, .05)
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
  expect_error(bootstrap_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("mu input checking works", {
  expect_error(bootstrap_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(bootstrap_one_sample(x, "foo"), "The tested parameter should be numeric.")
})

helper_00 <- function(bar) {}
helper_01 <- function(x, bar) {}
test_that("mu input checking works", {
  expect_error(bootstrap_one_sample(x, 0, "foo"), "Argument stat.fun must be a function.")
  expect_error(bootstrap_one_sample(x, 0, helper_00), "stat.fun's first argument is not x.")
  expect_error(bootstrap_one_sample(x, 0, helper_01), "stat.fun has too many arguments.")
})

test_that("alternative input checking works", {
  expect_error(bootstrap_one_sample(x, 0, stat.fun, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(bootstrap_one_sample(x, 0, stat.fun, 1), "Argument alternative should be a character.")
  expect_error(bootstrap_one_sample(x, 0, stat.fun, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(bootstrap_one_sample(x, 1, stat.fun, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(bootstrap_one_sample(x, 1, stat.fun, "less", "foo"), "conf.level should be numeric.")
  expect_error(bootstrap_one_sample(x, 1, stat.fun, "less", 0), "conf.level should between zero and one.")
  expect_error(bootstrap_one_sample(x, 1, stat.fun, "less", 1), "conf.level should between zero and one.")
})
