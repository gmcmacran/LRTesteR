###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(2)
  x <- rgamma(n = 100, shape = 10, scale = .5)
  test <- gamma_shape_one_sample(x, 10, alt)

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
    expect_true(ifelse(is.finite(CI1), gamma_shape_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), gamma_shape_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 1)
  test <- gamma_shape_one_sample(x, 10, alt)

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
    ifelse(is.finite(CI1), gamma_shape_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_shape_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(2)
  x <- rgamma(n = 100, shape = 8, scale = 1)
  test <- gamma_shape_one_sample(x, 10, alt)

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
    ifelse(is.finite(CI1), gamma_shape_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_shape_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(gamma_shape_one_sample(c()), "Argument x should have at least 45 data points.")
  expect_error(gamma_shape_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(50, shape = 1)
test_that("shape input checking works", {
  expect_error(gamma_shape_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_shape_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_shape_one_sample(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(gamma_shape_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_shape_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_shape_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(gamma_shape_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_shape_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(gamma_shape_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(gamma_shape_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 1, scale = 1)
  test <- gamma_scale_one_sample(x, 1, alt)

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
    expect_true(ifelse(is.finite(CI1), gamma_scale_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), gamma_scale_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 3)
  test <- gamma_scale_one_sample(x, 1, alt)

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
    ifelse(is.finite(CI1), gamma_scale_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_scale_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, scale = 1)
  test <- gamma_scale_one_sample(x, 3, alt)

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
    ifelse(is.finite(CI1), gamma_scale_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_scale_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(gamma_scale_one_sample(c()), "Argument x should have at least 45 data points.")
  expect_error(gamma_scale_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(50, shape = 1)
test_that("scale input checking works", {
  expect_error(gamma_scale_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_scale_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_scale_one_sample(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(gamma_scale_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_scale_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_scale_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(gamma_scale_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_scale_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(gamma_scale_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(gamma_scale_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(5)
  x <- rgamma(n = 100, shape = 10, rate = 1)
  test <- gamma_rate_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- gamma_scale_one_sample(x, 1 / 1, alt2)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), gamma_rate_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), gamma_rate_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, rate = 3)
  test <- gamma_rate_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- gamma_scale_one_sample(x, 1 / 1, alt2)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), gamma_rate_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_rate_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rgamma(n = 100, shape = 12, rate = 1)
  test <- gamma_rate_one_sample(x, 3, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_one", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  alt2 <- ifelse(alt == "greater", "less", ifelse(alt == "less", "greater", "two.sided"))
  test_02 <- gamma_scale_one_sample(x, 1 / 3, alt2)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_equal(test$p.value, test_02$p.value)
  })
  rm(alt2, test_02)

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), gamma_rate_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), gamma_rate_one_sample(x, CI2, alt)$p.value, .05)
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
  expect_error(gamma_rate_one_sample(c()), "Argument x should have at least 45 data points.")
  expect_error(gamma_rate_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(50, shape = 1)
test_that("rate input checking works", {
  expect_error(gamma_rate_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(gamma_rate_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(gamma_rate_one_sample(x, 0), "The tested parameter should be above 0.")
})

test_that("alternative input checking works", {
  expect_error(gamma_rate_one_sample(x, 1, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(gamma_rate_one_sample(x, 1, 1), "Argument alternative should be a character.")
  expect_error(gamma_rate_one_sample(x, 1, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(gamma_rate_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_rate_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(gamma_rate_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(gamma_rate_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rgamma(150, 2, 2)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_shape_one_way(x, fctr, .95)

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
CI2 <- gamma_shape_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rgamma(50, 1, 2), rgamma(50, 2, 2), rgamma(50, 3, 2))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_shape_one_way(x, fctr, .95)

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
CI2 <- gamma_shape_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(gamma_shape_one_way(c()), "Argument x should have at least 90 data points.")
  expect_error(gamma_shape_one_way(rep("foo", 100)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(100, 2, 1)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)), levels = c("1", "2", "3"))
fctr3 <- factor(c(rep(1, 60), rep(2, 39), 3), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(gamma_shape_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(gamma_shape_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(gamma_shape_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(gamma_shape_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
  expect_error(gamma_shape_one_way(x, fctr2), "Each level in fctr needs to be present.")
  expect_error(gamma_shape_one_way(x, fctr3), "Each groups needs to contain at least 45 data points for CIs to be accurate.")
})
rm(fctr1, fctr2, fctr3)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(gamma_shape_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_shape_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(gamma_shape_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(gamma_shape_one_way(x, fctr, 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rgamma(150, 1, scale = 2)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_scale_one_way(x, fctr, .95)

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
CI2 <- gamma_scale_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rgamma(50, 2, scale = 1), rgamma(50, 2, scale = 2), rgamma(50, 2, scale = 3))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_scale_one_way(x, fctr, .95)

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
CI2 <- gamma_scale_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(gamma_scale_one_way(c()), "Argument x should have at least 90 data points.")
  expect_error(gamma_scale_one_way(rep("foo", 100)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(100, 2, scale = 1)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)), levels = c("1", "2", "3"))
fctr3 <- factor(c(rep(1, 60), rep(2, 39), 3), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(gamma_scale_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(gamma_scale_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(gamma_scale_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(gamma_scale_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
  expect_error(gamma_scale_one_way(x, fctr2), "Each level in fctr needs to be present.")
  expect_error(gamma_scale_one_way(x, fctr3), "Each groups needs to contain at least 45 data points for CIs to be accurate.")
})
rm(fctr1, fctr2, fctr3)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(gamma_scale_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_scale_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(gamma_scale_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(gamma_scale_one_way(x, fctr, 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rgamma(150, 1, 2)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_rate_one_way(x, fctr, .95)

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
CI2 <- gamma_rate_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rgamma(50, 2, 1), rgamma(50, 2, 2), rgamma(50, 2, 3))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- gamma_rate_one_way(x, fctr, .95)

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
CI2 <- gamma_rate_one_sample(x[which(fctr == 1)], 2, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(gamma_rate_one_way(c()), "Argument x should have at least 90 data points.")
  expect_error(gamma_rate_one_way(rep("foo", 100)), "Argument x should be numeric.")
})

set.seed(1)
x <- rgamma(100, 2, 1)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
fctr2 <- factor(c(rep(1, 60), rep(2, 40)), levels = c("1", "2", "3"))
fctr3 <- factor(c(rep(1, 60), rep(2, 39), 3), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(gamma_rate_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(gamma_rate_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(gamma_rate_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(gamma_rate_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
  expect_error(gamma_rate_one_way(x, fctr2), "Each level in fctr needs to be present.")
  expect_error(gamma_rate_one_way(x, fctr3), "Each groups needs to contain at least 45 data points for CIs to be accurate.")
})
rm(fctr1, fctr2, fctr3)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(gamma_rate_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(gamma_rate_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(gamma_rate_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(gamma_rate_one_way(x, fctr, 1), "conf.level should between zero and one.")
})
