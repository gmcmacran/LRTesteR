###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, shape = 2)
  test <- inverse_gaussian_mu_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt, mu = 1)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), inverse_gaussian_mu_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), inverse_gaussian_mu_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 3, shape = 1)
  test <- inverse_gaussian_mu_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt, mu = 1)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_mu_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, shape = 1)
  test <- inverse_gaussian_mu_one_sample(x, 3, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt, mu = 3)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_mu_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(inverse_gaussian_mu_one_sample(c()), "Argument x should have at least 50 data points.")
  expect_error(inverse_gaussian_mu_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- statmod::rinvgauss(n = 50)
test_that("mu input checking works", {
  expect_error(inverse_gaussian_mu_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(inverse_gaussian_mu_one_sample(x, "foo"), "The tested parameter should be numeric.")
})

test_that("alternative input checking works", {
  expect_error(inverse_gaussian_mu_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(inverse_gaussian_mu_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(inverse_gaussian_mu_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(inverse_gaussian_mu_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(inverse_gaussian_mu_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(inverse_gaussian_mu_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(inverse_gaussian_mu_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, shape = 2)
  test <- inverse_gaussian_shape_one_sample(x, 2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), inverse_gaussian_shape_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), inverse_gaussian_shape_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 3, shape = 3)
  test <- inverse_gaussian_shape_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_shape_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_shape_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, shape = 1)
  test <- inverse_gaussian_shape_one_sample(x, 3, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt, mu = 3)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_shape_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_shape_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(inverse_gaussian_shape_one_sample(c()), "Argument x should have at least 50 data points.")
  expect_error(inverse_gaussian_shape_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- statmod::rinvgauss(n = 50)
test_that("mu input checking works", {
  expect_error(inverse_gaussian_shape_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(inverse_gaussian_shape_one_sample(x, "foo"), "The tested parameter should be numeric.")
})

test_that("alternative input checking works", {
  expect_error(inverse_gaussian_shape_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(inverse_gaussian_shape_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(inverse_gaussian_shape_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(inverse_gaussian_shape_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(inverse_gaussian_shape_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(inverse_gaussian_shape_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(inverse_gaussian_shape_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, dispersion = 2)
  test <- inverse_gaussian_dispersion_one_sample(x, 2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- inverse_gaussian_shape_one_sample(x, 1 / 2, alt2)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), inverse_gaussian_dispersion_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), inverse_gaussian_dispersion_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 3, dispersion = 3)
  test <- inverse_gaussian_dispersion_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- inverse_gaussian_shape_one_sample(x, 1 / 1, alt2)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_dispersion_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_dispersion_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- statmod::rinvgauss(n = 200, mean = 1, dispersion = 1)
  test <- inverse_gaussian_dispersion_one_sample(x, 3, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- inverse_gaussian_shape_one_sample(x, 1 / 3, alt2)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), inverse_gaussian_dispersion_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), inverse_gaussian_dispersion_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(inverse_gaussian_dispersion_one_sample(c()), "Argument x should have at least 50 data points.")
  expect_error(inverse_gaussian_dispersion_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- statmod::rinvgauss(n = 50)
test_that("mu input checking works", {
  expect_error(inverse_gaussian_dispersion_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, "foo"), "The tested parameter should be numeric.")
})

test_that("alternative input checking works", {
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(inverse_gaussian_dispersion_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})
