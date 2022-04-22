library(MLTesteR)

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rexp(200, 1)
  test <- exponentail_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CLT
  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- stats::t.test(x = x, alternative = alt2, mu = 1)
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
  x <- rexp(200, 3)
  test <- exponentail_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CLT
  alt2 <- ifelse(alt == "greater", "less", "two.sided")
  test_02 <- stats::t.test(x = x, alternative = alt2, mu = 1)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rexp(200, 1)
  test <- exponentail_rate_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  # Compare with test about mean using CL
  alt2 <- ifelse(alt == "less", "greater", "two.sided")
  test_02 <- stats::t.test(x = x, alternative = alt2, mu = 1 / 3)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}
