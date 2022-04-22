###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  test <- binomial_p_lr_test(2500, 5000, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with exact test
  test_02 <- stats::binom.test(x = 2500, n = 5000, p = .50, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- binomial_p_lr_test(75, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
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
    expect_true(class(test) == "mltest")
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
  expect_error(binomial_p_lr_test("foo", 10), NULL)
  expect_error(binomial_p_lr_test(c(5, 4), 10), NULL)
  expect_error(binomial_p_lr_test(-1, 10), NULL)
  expect_error(binomial_p_lr_test(12, 10), NULL)
})

test_that("n input checking works", {
  expect_error(binomial_p_lr_test(5, "foo"), NULL)
  expect_error(binomial_p_lr_test(5, c(10, 11)), NULL)
  expect_error(binomial_p_lr_test(0, -1), NULL)
})

set.seed(1)
test_that("p input checking works", {
  expect_error(binomial_p_lr_test(5, 10, "foo"), NULL)
  expect_error(binomial_p_lr_test(5, 10, c(.5, .6)), NULL)
  expect_error(binomial_p_lr_test(5, 10, -1), NULL)
  expect_error(binomial_p_lr_test(5, 10, 1.1), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(binomial_p_lr_test(5, 10, .5, c("two.sided", "less")), NULL)
  expect_error(binomial_p_lr_test(5, 10, .5, 1), NULL)
  expect_error(binomial_p_lr_test(5, 10, .5, "lesss"), NULL)
})
