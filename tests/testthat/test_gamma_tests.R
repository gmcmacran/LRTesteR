library(MLTesteR)

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rgamma(n = 100, shape = 10, scale = .5)
  test <- gamma_shape_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 1)
  test <- gamma_shape_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(2)
  x <- rgamma(n = 100, shape = 8, scale = 1)
  test <- gamma_shape_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(5)
  x <- rgamma(n = 100, shape = 10, scale = 1)
  test <- gamma_scale_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 3)
  test <- gamma_scale_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 1)
  test <- gamma_scale_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(5)
  x <- rgamma(n = 100, shape = 10, rate = 1)
  test <- gamma_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, rate = 3)
  test <- gamma_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, rate = 1)
  test <- gamma_rate_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}
