# When X ~ Beta(alpha, 1), then  sum(-ln(X)) ~ gamma(N, 1/alpha)
# (shape and scale)
# Function requires and assumes beta (the parameter) equals 1
# Not programmatically checked.
exact_test_shape1 <- function(x, shape1, alternative) {
  calc_two_sided_p_value <- function(x) {
    2 * pmin(
      stats::pgamma(q = sum(-log(x)), shape = length(x), scale = 1 / shape1, lower.tail = TRUE),
      stats::pgamma(q = sum(-log(x)), shape = length(x), scale = 1 / shape1, lower.tail = FALSE)
    )
  }

  calc_left_p_value <- function(x) {
    stats::pgamma(q = sum(-log(x)), shape = length(x), scale = 1 / shape1, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x) {
    stats::pgamma(q = sum(-log(x)), shape = length(x), scale = 1 / shape1, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(x)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(x)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(x)
  }
  return(list(p.value = p.value))
}


###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 3, shape2 = 1)
  test <- beta_shape1_lr_test(x, 3, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 3, alt)
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
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape1_lr_test(x, 1, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 1, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 1, shape2 = 1)
  test <- beta_shape1_lr_test(x, 2, alt)

  test_02 <- exact_test_shape1(x, 2, alt)
  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

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
  expect_error(beta_shape1_lr_test(c()), NULL)
  expect_error(beta_shape1_lr_test(rep("foo", 50)), NULL)
  expect_error(beta_shape1_lr_test(rbeta(49, shape1 = 1, shape2 = 1)), NULL)
})

set.seed(1)
test_that("shape1 input checking works", {
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), c(1, 2)), NULL)
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), "foo"), NULL)
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 0), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 1, c("two.sided", "less")), NULL)
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 1, 1), NULL)
  expect_error(beta_shape1_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 1, "lesss"), NULL)
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_lr_test(x, 2, alt)

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
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_lr_test(x, 1, alt)

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
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape2_lr_test(x, 2, alt)

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
  expect_error(beta_shape2_lr_test(c()), NULL)
  expect_error(beta_shape2_lr_test(rep("foo", 50)), NULL)
  expect_error(beta_shape2_lr_test(rbeta(49, shape1 = 1, shape2 = 1)), NULL)
})

set.seed(1)
test_that("shape2 input checking works", {
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), c(1, 2)), NULL)
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), "foo"), NULL)
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 0), NULL)
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 2, c("two.sided", "less")), NULL)
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 2, 1), NULL)
  expect_error(beta_shape2_lr_test(rbeta(50, shape1 = 1, shape2 = 1), 2, "lesss"), NULL)
})
