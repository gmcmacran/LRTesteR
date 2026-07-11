# For kurtosis test and t distribution, need 8th moment to be finite.
# This means kurtosis is between .1 and 1.4.
calc_df <- function(kurtosis) {
  v <- 4 + (6 / kurtosis)
  return(v)
}

calc_kurtosis <- function(v) {
  kurtosis <- (6 / (v - 4))
  return(kurtosis)
}


###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- empirical_kurtosis_one_sample(x, 0, alt)

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
    expect_true(ifelse(is.finite(CI1), empirical_kurtosis_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), empirical_kurtosis_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rt(300, calc_df(1.4))
  test <- empirical_kurtosis_one_sample(x, .1, alt)

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
    ifelse(is.finite(CI1), empirical_kurtosis_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_kurtosis_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rt(200, calc_df(1))
  test <- empirical_kurtosis_one_sample(x, 1.5, alt)

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
    empirical_kurtosis_one_sample(x, 1)$statistic,
    empirical_kurtosis_one_sample(10 + 7 * x, 1)$statistic,
    tolerance = .1^4
  )
})

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_kurtosis_one_sample(c(1, 2, 3, 4)), "Argument x should have at least five observations.")
  expect_error(empirical_kurtosis_one_sample(rep("foo", 50)), "Argument x should be numeric.")
  expect_error(empirical_kurtosis_one_sample(rep(1, 50), 0), "Argument x should have at least two unique values.")
})

set.seed(1)
x <- rt(300, calc_df(1.4))
test_that("kurtosis input checking works", {
  expect_error(empirical_kurtosis_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(empirical_kurtosis_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(empirical_kurtosis_one_sample(x, -2), "The tested parameter must be greater than negative two.")
})

test_that("alternative input checking works", {
  expect_error(empirical_kurtosis_one_sample(x, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_kurtosis_one_sample(x, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_kurtosis_one_sample(x, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_kurtosis_one_sample(x, 0, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_kurtosis_one_sample(x, 0, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_kurtosis_one_sample(x, 0, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_kurtosis_one_sample(x, 0, "less", 1), "conf.level should between zero and one.")
})

###############################################
# One Way: Null True
###############################################
set.seed(1)
x <- rt(n = 75, df = calc_df(.1))
fctr <- factor(c(rep(1, 25), rep(2, 25), rep(3, 25)), levels = c("1", "2", "3"))
test <- empirical_kurtosis_one_way(x, fctr, .95)

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
tempX <- x[which(fctr == 1)]
obs_kurtosis <- mean((tempX - mean(tempX))^4) / mean((tempX - mean(tempX))^2)^2 - 3
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_kurtosis_one_sample(tempX, obs_kurtosis, "two.sided", test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2, tempX, obs_kurtosis)

###############################################
# One Way: Null False
###############################################
set.seed(1)
x <- c(runif(25, -1, 1), runif(25, -1, 1), rt(25, calc_df(1.4)))
test <- empirical_kurtosis_one_way(x, fctr, .95)

test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
})

###############################################
# One Way: Input checking
###############################################
set.seed(1)
x <- rt(n = 75, df = calc_df(.1))
test_that("one way input checking works", {
  expect_error(empirical_kurtosis_one_way(c()), "Argument x should have positive length.")
  expect_error(empirical_kurtosis_one_way(rep("foo", 75)), "Argument x should be numeric.")
  expect_error(empirical_kurtosis_one_way(x, fctr[1:50]), "Argument fctr should have same length as x.")
  expect_error(empirical_kurtosis_one_way(x, as.character(fctr)), "Argument fctr should be a factor.")
  expect_error(empirical_kurtosis_one_way(x, factor(rep(1, 75))), "Argument fctr should have at least two unique values.")
  expect_error(empirical_kurtosis_one_way(x, factor(c(rep(1, 4), rep(2, 71)))), "Each group in x should have at least five observations.")
  expect_error(empirical_kurtosis_one_way(c(rep(1, 25), x[26:75]), fctr), "Each group in x should have at least two unique values.")
  expect_error(empirical_kurtosis_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_kurtosis_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(empirical_kurtosis_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(empirical_kurtosis_one_way(x, fctr, 1), "conf.level should between zero and one.")
})
