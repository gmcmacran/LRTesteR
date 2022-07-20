###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  test <- binomial_p_lr_test(25, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 25, n = 50, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .06)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), binomial_p_lr_test(25, 50, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), binomial_p_lr_test(25, 50, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- binomial_p_lr_test(75, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 75, n = 100, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), binomial_p_lr_test(75, 100, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), binomial_p_lr_test(75, 100, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  test <- binomial_p_lr_test(25, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 25, n = 100, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), binomial_p_lr_test(25, 100, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), binomial_p_lr_test(25, 100, CI2, alt)$p.value, .05)
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
  expect_error(binomial_p_lr_test("foo"), "First argument should be numeric.")
  expect_error(binomial_p_lr_test(c(5, 4)), "First argument should have length 1.")
  expect_error(binomial_p_lr_test(2.5), "First argument should be an integer.")
  expect_error(binomial_p_lr_test(-1), "First argument should be 0 or above.")
})

test_that("n input checking works", {
  expect_error(binomial_p_lr_test(1, "foo"), "Second argument should be numeric.")
  expect_error(binomial_p_lr_test(1, c(5, 4)), "Second argument should have length 1.")
  expect_error(binomial_p_lr_test(1, 2.5), "Second argument should be an integer.")
  expect_error(binomial_p_lr_test(1, -1), "Second argument should be 0 or above.")
})

test_that("p input checking works", {
  expect_error(binomial_p_lr_test(1, 1, "foo"), "Argument p should be numeric.")
  expect_error(binomial_p_lr_test(1, 1, c(.5, .6)), "Argument p should have length one.")
  expect_error(binomial_p_lr_test(1, 1, -.1), "Argument p should be between 0 and 1.")
  expect_error(binomial_p_lr_test(1, 1, 1.01), "Argument p should be between 0 and 1.")
})

test_that("alternative input checking works", {
  expect_error(binomial_p_lr_test(5, 10, .5, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(binomial_p_lr_test(5, 10, .5, 1), "Argument alternative should be a character.")
  expect_error(binomial_p_lr_test(5, 10, .5, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater'")
})
