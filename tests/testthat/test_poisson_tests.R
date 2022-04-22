###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rpois(200, 1)
  test <- poisson_lambda_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CLT
  test_02 <- stats::t.test(x = x, alternative = alt, mu = 1)
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
  x <- rpois(200, 3)
  test <- poisson_lambda_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CLT
  test_02 <- stats::t.test(x = x, alternative = alt, mu = 1)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rpois(200, 1)
  test <- poisson_lambda_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CLT
  test_02 <- stats::t.test(x = x, alternative = alt, mu = 3)
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
  expect_error(poisson_lambda_lr_test(c()), NULL)
  expect_error(poisson_lambda_lr_test(rep("foo", 50)), NULL)
  expect_error(poisson_lambda_lr_test(rpois(49, lambda = 1)), NULL)
})

set.seed(1)
test_that("lambda input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), c(1, 2)), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), "foo"), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 0), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, c("two.sided", "less")), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, 1), NULL)
  expect_error(poisson_lambda_lr_test(rpois(50, lambda = 1), 1, "lesss"), NULL)
})
