library(MLTesteR)

exact_test <- function(num_failures, p, alternative) {
  calc_two_sided_p_value <- function(x, prob) {
    if (prob == 0) {
      (as.numeric(x > 0))
    } else if (prob == 1) {
      (as.numeric(x == 0))
    } else {
      relErr <- 1 + 1e-05
      d <- dgeom(x, prob)
      m <- (1 - prob) / prob
      if (x == m) {
        1
      } else if (x < m) {
        nearInf <- ceiling(m * 20)
        i <- seq.int(from = ceiling(m), to = nearInf)
        i <- setdiff(i, x)
        y <- sum(dgeom(i, prob) < d * relErr)
        pgeom(x, prob) + pgeom(pmax(nearInf - y, 0), prob, lower.tail = FALSE)
      } else {
        i <- seq.int(from = 0, to = floor(m))
        i <- setdiff(i, x)
        y <- sum(dgeom(i, prob) < d * relErr)
        pgeom(y - 1, prob) + pgeom(x - 1, prob, lower.tail = FALSE)
      }
    }
  }

  calc_left_p_value <- function(x, prob) {
    pgeom(q = x, prob = p, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, prob) {
    pgeom(q = x - 1, prob = p, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(num_failures, p)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(num_failures, p)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(num_failures, p)
  }
  return(list(p.value = p.value))
}

###############################################
# Null True
###############################################
for (alt in c("two.sided")) {
  test <- geometric_p_lr_test(1, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(1, .50, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("less")) {
  test <- geometric_p_lr_test(0, .10, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(0, .10, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .02)
  })
}

for (alt in c("greater")) {
  test <- geometric_p_lr_test(0, .26, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(0, .26, alt)
  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .30)
  })
}

###############################################
# Null False
###############################################

for (alt in c("two.sided", "less")) {
  test <- geometric_p_lr_test(10, .925, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(10, .925, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })
}

for (alt in c("greater")) {
  test <- geometric_p_lr_test(0, .05, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_02 <- exact_test(0, .05, alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .05)
  })
}
