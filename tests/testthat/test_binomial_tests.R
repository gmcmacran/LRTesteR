###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  test <- binomial_p_lr_test(25, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 25, n = 50, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .06)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- binomial_p_lr_test(75, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 75, n = 100, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  test <- binomial_p_lr_test(25, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 25, n = 100, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Input checking
###############################################

test_that("x input checking works", {
  expect_error(binomial_p_lr_test("foo", 10), "Argument x should be numeric.")
  expect_error(binomial_p_lr_test(c(5, 4), 10), "Argument x should have length 1.")
  expect_error(binomial_p_lr_test(-1, 10), "Argument x should be 0 or positive.")
  expect_error(binomial_p_lr_test(12, 10), "Argument x should be less than or equal to n.")
})

test_that("n input checking works", {
  expect_error(binomial_p_lr_test(5, "foo"), "Argument n should be numeric.")
  expect_error(binomial_p_lr_test(5, c(10, 11)), "Argument n should have length 1.")
  expect_error(binomial_p_lr_test(0, 0), "Argument n should be positive.")
})

set.seed(1)
test_that("p input checking works", {
  expect_error(binomial_p_lr_test(5, 10, "foo"), "Argument p should be numeric.")
  expect_error(binomial_p_lr_test(5, 10, c(.5, .6)), "Argument p should have length one.")
  expect_error(binomial_p_lr_test(5, 10, -.1), "Argument p should be between 0 and 1.")
  expect_error(binomial_p_lr_test(5, 10, 1.01), "Argument p should be between 0 and 1.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(binomial_p_lr_test(5, 10, .5, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(binomial_p_lr_test(5, 10, .5, 1), "Argument alternative should be a character.")
  expect_error(binomial_p_lr_test(5, 10, .5, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater'")
})
