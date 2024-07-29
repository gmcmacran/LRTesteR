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
  test <- beta_shape1_one_sample(x, 3, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 3, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), beta_shape1_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), beta_shape1_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape1_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_02 <- exact_test_shape1(x, 1, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape1_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape1_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 1, shape2 = 1)
  test <- beta_shape1_one_sample(x, 2, alt)

  test_02 <- exact_test_shape1(x, 2, alt)
  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
  rm(test_02)

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape1_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape1_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(beta_shape1_one_sample(c()), "Argument x should have at least 40 data points.")
  expect_error(beta_shape1_one_sample(rep("foo", 40)), "Argument x should be numeric.")
})

set.seed(1)
x <- rbeta(50, shape1 = 1, shape2 = 1)
test_that("shape1 input checking works", {
  expect_error(beta_shape1_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(beta_shape1_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(beta_shape1_one_sample(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(beta_shape1_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(beta_shape1_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(beta_shape1_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(beta_shape1_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape1_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(beta_shape1_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(beta_shape1_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_one_sample(x, 2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), beta_shape2_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), beta_shape2_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 2)
  test <- beta_shape2_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape2_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape2_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rbeta(100, shape1 = 2, shape2 = 1)
  test <- beta_shape2_one_sample(x, 2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), beta_shape2_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), beta_shape2_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(beta_shape2_one_sample(c()), "Argument x should have at least 40 data points.")
  expect_error(beta_shape2_one_sample(rep("foo", 40)), "Argument x should be numeric.")
})

set.seed(1)
x <- rbeta(50, shape1 = 1, shape2 = 1)
test_that("shape2 input checking works", {
  expect_error(beta_shape2_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(beta_shape2_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(beta_shape2_one_sample(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(beta_shape2_one_sample(x, 2, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(beta_shape2_one_sample(x, 2, 1), "Argument alternative should be a character.")
  expect_error(beta_shape2_one_sample(x, 2, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(beta_shape2_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape2_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(beta_shape2_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(beta_shape2_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rbeta(150, 2, 2)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- beta_shape1_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_one", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- beta_shape1_one_sample(x[which(fctr == 1)], 5, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rbeta(50, 1, 2), rbeta(50, 2, 2), rbeta(50, 3, 2))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- beta_shape1_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_one", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value <= .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- beta_shape1_one_sample(x[which(fctr == 1)], 5, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(beta_shape1_one_way(c()), "Argument x should have at least 80 data points.")
  expect_error(beta_shape1_one_way(rep("foo", 80)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)), levels = c("1", "2", "3"))
fctr3 <- factor(c(rep(1, 60), rep(2, 39), 3), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(beta_shape1_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(beta_shape1_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(beta_shape1_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(beta_shape1_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
  expect_error(beta_shape1_one_way(x, fctr2), "Each level in fctr needs to be present.")
  expect_error(beta_shape1_one_way(x, fctr3), "Each groups needs to contain at least 40 data points for CIs to be accurate.")
})
rm(fctr1, fctr2, fctr3)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(beta_shape1_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape1_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(beta_shape1_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(beta_shape1_one_way(x, fctr, 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rbeta(150, 2, 2)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- beta_shape2_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_one", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- beta_shape2_one_sample(x[which(fctr == 1)], 5, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rbeta(50, 2, 1), rbeta(50, 2, 2), rbeta(50, 2, 3))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- beta_shape2_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_one", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- beta_shape2_one_sample(x[which(fctr == 1)], 5, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(beta_shape2_one_way(c()), "Argument x should have at least 80 data points.")
  expect_error(beta_shape2_one_way(rep("foo", 80)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)), levels = c("1", "2", "3"))
fctr3 <- factor(c(rep(1, 60), rep(2, 39), 3), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(beta_shape2_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(beta_shape2_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(beta_shape2_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(beta_shape2_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
  expect_error(beta_shape2_one_way(x, fctr2), "Each level in fctr needs to be present.")
  expect_error(beta_shape2_one_way(x, fctr3), "Each groups needs to contain at least 40 data points for CIs to be accurate.")
})
rm(fctr1, fctr2, fctr3)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(beta_shape2_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(beta_shape2_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(beta_shape2_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(beta_shape2_one_way(x, fctr, 1), "conf.level should between zero and one.")
})
