library(MLTesteR)

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 1, shape2 = 2)
  test <- beta_shape1_lr_test(x, 1, alt)

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
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape1_lr_test(x, 1, alt)

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
  x <- rbeta(100, shape1 = 1, shape2 = 2)
  test <- beta_shape1_lr_test(x, 2, alt)

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
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_lr_test(x, 2, alt)

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
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_lr_test(x, 1, alt)

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
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape2_lr_test(x, 2, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}
