library(MLTesteR)

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- gaussian_mean_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- gaussian_mean_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- gaussian_mean_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- gaussian_mean_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rnorm(200, 0, 3)
  test <- gaussian_variance_lr_test(x, 8, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with chi square test for variance
  test_02 <- EnvStats::varTest(x, alternative = alt, sigma.squared = 8)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 3)
  test <- gaussian_variance_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with chi square test for variance
  test_02 <- EnvStats::varTest(x, alternative = alt, sigma.squared = 10)
  test_that("Check contents", {
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}
