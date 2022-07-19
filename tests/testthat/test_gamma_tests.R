###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rgamma(n = 100, shape = 10, scale = .5)
  test <- gamma_shape_lr_test(x, 10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(gamma_shape_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(gamma_shape_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("shape input checking works", {
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_shape_lr_test(rgamma(50, shape = 1), 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(5)
  x <- rgamma(n = 100, shape = 10, scale = 1)
  test <- gamma_scale_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(gamma_scale_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(gamma_scale_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("scale input checking works", {
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_scale_lr_test(rgamma(50, shape = 1), 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(5)
  x <- rgamma(n = 100, shape = 10, rate = 1)
  test <- gamma_rate_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
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
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(gamma_rate_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(gamma_rate_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
test_that("rate input checking works", {
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_rate_lr_test(rgamma(50, shape = 1), 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})
