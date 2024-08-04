###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- empirical_mu_one_sample(x, 0, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)


  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- empirical_mu_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- empirical_mu_one_sample(x, -2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(empirical_mu_one_sample(c()), "Argument x should have positive length.")
  expect_error(empirical_mu_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("mu input checking works", {
  expect_error(empirical_mu_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(empirical_mu_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(empirical_mu_one_sample(x, min(x)), "The tested parameter must be greater than the min of x.")
  expect_error(empirical_mu_one_sample(x, max(x)), "The tested parameter must be less than the max of x.")
})

test_that("alternative input checking works", {
  expect_error(empirical_mu_one_sample(x, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_mu_one_sample(x, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_mu_one_sample(x, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_mu_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_mu_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_mu_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
# Owen 1991 lists out stat
# Equation 6.1 and 6.2.
# Only left hand side of 6.1 so testing for approximate match. Not exact.
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_mu_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

dat <- data.frame(fctr = fctr, x = x)
model_00 <- lm(x ~ 1, data = dat)
model_01 <- lm(x ~ fctr, data = dat)

test_02 <- lmtest::lrtest(model_00, model_01)
test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
  expect_true(abs(test$p.value - test_02[["Pr(>Chisq)"]][2]) < .02)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_mu_one_sample(x[which(fctr == 1)], 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2, dat, model_00, model_01)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rnorm(50, 1.5, 1), rnorm(50, 2, 1), rnorm(50, 2.25, 1))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_mu_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

dat <- data.frame(fctr = fctr, x = x)
model_00 <- lm(x ~ 1, data = dat)
model_01 <- lm(x ~ fctr, data = dat)

test_02 <- lmtest::lrtest(model_00, model_01)
test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
  expect_true(abs(test$p.value - test_02[["Pr(>Chisq)"]][2]) < .02)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_mu_one_sample(x[which(fctr == 1)], 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2, dat, model_00, model_01)

###############################################
# Check statistic against publication.
###############################################
calc_reference_stat <- function(x, fctr) {
  si2 <- vector(mode = "numeric", length = length(levels(fctr)))
  means <- vector(mode = "numeric", length = length(levels(fctr)))
  nis <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(si2)) {
    index <- fctr == levels(fctr)[i]
    tempX <- x[index]
    means[i] <- mean(tempX)
    nis[i] <- length(tempX)
    si2[i] <- sum((tempX - means[i])^2) / nis[i]
  }

  numerator <- vector(mode = "numeric", length = length(levels(fctr)))
  denominator <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(numerator)) {
    numerator[i] <- nis[i] * means[i] / si2[i]
    denominator[i] <- nis[i] / si2[i]
  }
  muHat <- sum(numerator) / sum(denominator)

  testStatElements <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(testStatElements)) {
    testStatElements[i] <- nis[i] * (means[i] - muHat)^2 / si2[i]
  }
  testStat <- sum(testStatElements)
  return(testStat)
}


set.seed(1)
x <- rnorm(300, 1.5, 1)
fctr <- c(rep(1, 150), rep(2, 150))
fctr <- factor(fctr, levels = c("1", "2"))
ts_01 <- empirical_mu_one_way(x, fctr, .95)$statistic
ts_02 <- calc_reference_stat(x, fctr)
test_that("Check statistic", {
  expect_true(abs(ts_01 - ts_02) < .1)
})

set.seed(1)
x <- rgamma(900, 1.5, 1)
fctr <- c(rep(1, 300), rep(2, 300), rep(3, 300))
fctr <- factor(fctr, levels = c("1", "2", "3"))
ts_01 <- empirical_mu_one_way(x, fctr, .95)$statistic
ts_02 <- calc_reference_stat(x, fctr)
test_that("Check statistic", {
  expect_true(abs(ts_01 - ts_02) < .1)
})

rm(x, fctr, ts_01, ts_02, calc_reference_stat)

###############################################
# Check against emplik2
###############################################
library(emplik2)
library(datasets)

x <- c(USJudgeRatings$ORAL, USJudgeRatings$WRIT)
fctr <- c(rep(1, length(USJudgeRatings$ORAL)), rep(2, length(USJudgeRatings$WRIT)))
fctr <- factor(fctr, levels = c("1", "2"))

t1 <- empirical_mu_one_way(x, fctr, .95)
t2 <- el2.cen.EMs(
  x = x[fctr == "1"],
  dx = rep(1, length(x[fctr == "1"])),
  y = x[fctr == "2"],
  dy = rep(1, length(x[fctr == "2"])),
  fun = function(x, y) {
    x - y
  },
  mean = 0,
  maxit = 100
)

test_that("Check statistic", {
  expect_true(abs(t1$statistic - t2$`-2LLR`) < .01)
  expect_true(abs(t1$p.value - t2$Pval) < .01)
})

set.seed(1)
x <- c(rnorm(100, 1, 1), rnorm(100, 1, 1))
fctr <- c(rep(1, 100), rep(2, 100))
fctr <- factor(fctr, levels = c("1", "2"))

t1 <- empirical_mu_one_way(x, fctr, .95)
t2 <- el2.cen.EMs(
  x = x[fctr == "1"],
  dx = rep(1, length(x[fctr == "1"])),
  y = x[fctr == "2"],
  dy = rep(1, length(x[fctr == "2"])),
  fun = function(x, y) {
    x - y
  },
  mean = 0,
  maxit = 100
)

test_that("Check statistic", {
  expect_true(abs(t1$statistic - t2$`-2LLR`) < .01)
  expect_true(abs(t1$p.value - t2$Pval) < .01)
})

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_mu_one_way(c()), "Argument x should have positive length.")
  expect_error(empirical_mu_one_way("foo"), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(empirical_mu_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(empirical_mu_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(empirical_mu_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(empirical_mu_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
})
rm(fctr1)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_mu_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(empirical_mu_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(empirical_mu_one_way(x, fctr, 1), "conf.level should between zero and one.")
})

set.seed(1)
x <- sort(rnorm(100))
x2 <- x
x2[100] <- -2
fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_way(x, fctr, .95), "Every group in x must have at least one data point less than the grand mean.")
})

x <- 1:10
x2 <- x
x2[6] <- 5
fctr <- c(rep(1, 5), rep(2, 5))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_way(x, fctr, .95), "Every group in x must have at least one data point less than the grand mean.")
  expect_error(empirical_mu_one_way(x2, fctr, .95), "Every group in x must have at least one data point greater than the grand mean.")
})
rm(x, x2, fctr)
