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
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 3, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), beta_shape1_lr_test(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), beta_shape1_lr_test(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
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
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 1, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape1_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape1_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 1, shape2 = 1)
  test <- beta_shape1_lr_test(x, 2, alt)

  test_02 <- exact_test_shape1(x, 2, alt)
  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape1_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape1_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(beta_shape1_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(beta_shape1_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rbeta(50, shape1 = 1, shape2 = 1)
test_that("shape1 input checking works", {
  expect_error(beta_shape1_lr_test(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(beta_shape1_lr_test(x, "foo"), "The tested parameter should be numeric.")
  expect_error(beta_shape1_lr_test(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(beta_shape1_lr_test(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(beta_shape1_lr_test(x, 1, 1), "Argument alternative should be a character.")
  expect_error(beta_shape1_lr_test(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("alternative input checking works", {
  expect_error(beta_shape1_lr_test(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape1_lr_test(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(beta_shape1_lr_test(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(beta_shape1_lr_test(x, 1, "less", 1), "conf.level should between zero and one.")
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
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), beta_shape2_lr_test(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), beta_shape2_lr_test(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
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
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape2_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape2_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape2_lr_test(x, 2, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "lrtest")
    expect_true(length(test) == 4)
    expect_true(all(names(test) == c("statistic", "p.value", "CI", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  CI1 <- test$CI[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$CI[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape2_lr_test(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape2_lr_test(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Input checking
###############################################
set.seed(1)
test_that("x input checking works", {
  expect_error(beta_shape2_lr_test(c()), "Argument x should have at least 50 data points.")
  expect_error(beta_shape2_lr_test(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rbeta(50, shape1 = 1, shape2 = 1)
test_that("shape2 input checking works", {
  expect_error(beta_shape2_lr_test(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(beta_shape2_lr_test(x, "foo"), "The tested parameter should be numeric.")
  expect_error(beta_shape2_lr_test(x, 0), "The tested parameter should be above 0.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(beta_shape2_lr_test(x, 2, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(beta_shape2_lr_test(x, 2, 1), "Argument alternative should be a character.")
  expect_error(beta_shape2_lr_test(x, 2, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

set.seed(1)
test_that("alternative input checking works", {
  expect_error(beta_shape2_lr_test(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape2_lr_test(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(beta_shape2_lr_test(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(beta_shape2_lr_test(x, 1, "less", 1), "conf.level should between zero and one.")
})
