###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- gaussian_mu_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- gaussian_mu_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- gaussian_mu_lr_test(x, 0, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
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
  expect_error(gaussian_mu_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(gaussian_mu_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("mu input checking works", {
  expect_error(gaussian_mu_lr_test(rnorm(50), c(1, 2)), "The tested parameter should have length one.")
  expect_error(gaussian_mu_lr_test(rnorm(50), "foo"), "The tested parameter should be numeric.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(gaussian_mu_lr_test(rnorm(50), 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gaussian_mu_lr_test(rnorm(50), 0, 1), "Argument alternative should be a character.")
  expect_error(gaussian_mu_lr_test(rnorm(50), 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(1000, 0, 3)
  test <- gaussian_variance_lr_test(x, 9, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with chi square test for variance
  test_02 <- EnvStats::varTest(x, alternative = alt, sigma.squared = 9)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(2)
  x <- rnorm(100, 0, 3)
  test <- gaussian_variance_lr_test(x, 8, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with chi square test for variance
  test_02 <- EnvStats::varTest(x, alternative = alt, sigma.squared = 8)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 3)
  test <- gaussian_variance_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with chi square test for variance
  test_02 <- EnvStats::varTest(x, alternative = alt, sigma.squared = 10)
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
  expect_error(gaussian_variance_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(gaussian_variance_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("variance input checking works", {
  expect_error(gaussian_variance_lr_test(rnorm(50), c(1, 2)), "The tested parameter should have length one.")
  expect_error(gaussian_variance_lr_test(rnorm(50), "foo"), "The tested parameter should be numeric.")
  expect_error(gaussian_variance_lr_test(rnorm(50), 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(gaussian_variance_lr_test(rnorm(50), 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gaussian_variance_lr_test(rnorm(50), 1, 1), "Argument alternative should be a character.")
  expect_error(gaussian_variance_lr_test(rnorm(50), 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})
