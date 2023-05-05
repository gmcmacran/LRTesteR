#' Test any quantile of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param Q The quantile. A single numeric number. (.50 is median.)
#' @param value A single numeric value that is the Q quantile.
#' @inherit gaussian_mu_one_sample return
#' @source \itemize{
#' \item Yudi Pawitan. In All Likelihood. Oxford University Press.
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' empirical_quantile_one_sample(x, .5, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(25, 2, 1)
#' empirical_mu_one_sample(x, .5, 1, "greater")
#' @export
empirical_quantile_one_sample <- function(x, Q, value, alternative = "two.sided", conf.level = .95) {
  if (length(x) < 1) {
    stop("Argument x should have positive length.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(Q) != 1) {
    stop("The Q parameter should have length one.")
  }
  if (!is.numeric(Q)) {
    stop("The Q parameter should be numeric.")
  }
  if (Q <= 0 || Q >= 1) {
    stop("Q should between zero and one.")
  }
  if (length(value) != 1) {
    stop("The value parameter should have length one.")
  }
  if (!is.numeric(value)) {
    stop("The value parameter should be numeric.")
  }
  if (value <= min(x)) {
    stop("The value parameter must be greater than the min of x.")
  }
  if (value >= max(x)) {
    stop("The value parameter must be less than the max of x.")
  }
  if (length(alternative) != 1) {
    stop("Argument alternative should have length one.")
  }
  if (!is.character(alternative)) {
    stop("Argument alternative should be a character.")
  }
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Argument alternative should be 'two.sided', 'less', or 'greater.'")
  }
  if (length(conf.level) != 1) {
    stop("conf.level should have length one.")
  }
  if (!is.numeric(conf.level)) {
    stop("conf.level should be numeric.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_test_stat <- function(x, Q, value, alternative) {
    x <- ifelse(x > value, 1, 0)

    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    calc_null_p <- function(x, Q) {
      calc_lambda <- function(x, Q) {
        g <- function(lambda) {
          numerator <- x - Q
          denominator <- n - lambda * (x - Q)
          denominator[is.nan(denominator)] <- -Inf
          out <- sum(numerator / denominator)
          return(out)
        }
        n <- length(x)
        if (Q < mean(x)) {
          LB <- n / (min(x) - Q)
          UB <- pmin(n / (max(x) - Q), 0)

          if (g(LB) == 0) {
            lambda <- LB
          } else {
            lambda <- stats::uniroot(g, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root
          }
        } else if (Q > mean(x)) {
          LB <- pmax(n / (min(x) - Q), 0)
          UB <- n / (max(x) - Q)

          if (g(UB) == 0) {
            lambda <- UB
          } else {
            lambda <- stats::uniroot(g, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root
          }
        } else {
          lambda <- 0
        } # null's Q is xbar

        return(lambda)
      }

      lambda <- calc_lambda(x, Q)
      phi <- -length(x) - lambda * Q

      p <- -1 / (phi + lambda * x)

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p(x, Q)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0) # underflow
    if (alternative != "two.sided") {
      W <- sign(mean(x) - Q) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, Q, alternative, conf.level) {
    alpha <- 1 - conf.level

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, Q, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- min(x) + .01
      UB <- median(x)

      out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")
      out <- out$root - out$estim.prec
      out <- max(x[which(x <= out)])


      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, Q, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- median(x)
      UB <- max(x) - .01

      out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")
      out <- out$root + out$estim.prec
      out <- max(x[which(x <= out)])


      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
    } else if (alternative == "less") {
      CI <- c(NA_real_, calc_right_side_CI(alpha))
    } else {
      CI <- c(calc_left_side_CI(alpha), NA_real_)
    }

    return(CI)
  }

  W <- calc_test_stat(x, Q, value, alternative)

  # calculate p value
  if (alternative == "two.sided") {
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  } else if (alternative == "less") {
    p.value <- stats::pnorm(q = W, lower.tail = TRUE)
  } else {
    p.value <- stats::pnorm(q = W, lower.tail = FALSE)
  }

  CI <- calc_CI(x, Q, alternative, conf.level)

  out <- list(statistic = W, p.value = p.value, conf.int = CI, conf.level = conf.level, alternative = alternative)
  class(out) <- c("one_sample_case_three", "lrtest")

  return(out)
}

#' Test the equality of means of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_way
#' @param x a numeric vector.
#' @inherit gaussian_mu_one_way return
#' @inherit empirical_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(75, 1, 1)
#' fctr <- c(rep(1, 25), rep(2, 25), rep(3, 25))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' empirical_quantile_one_way(x, .50, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(25, 1, 1), rnorm(25, 2, 1), rnorm(25, 3, 1))
#' fctr <- c(rep(1, 25), rep(2, 25), rep(3, 25))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' empirical_quantile_one_way(x, .50, fctr, .95)
#' @export
empirical_quantile_one_way <- function(x, Q, fctr, conf.level = 0.95) {
  if (length(x) < 1) {
    stop("Argument x should have positive length.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(Q) != 1) {
    stop("The Q parameter should have length one.")
  }
  if (!is.numeric(Q)) {
    stop("The Q parameter should be numeric.")
  }
  if (Q <= 0 || Q >= 1) {
    stop("Q should between zero and one.")
  }
  if (length(fctr) != length(x)) {
    stop("Argument fctr should have same length as x.")
  }
  if (!is.factor(fctr)) {
    stop("Argument fctr should be a factor.")
  }
  if (length(base::unique(fctr)) < 2) {
    stop("Argument fctr should have at least two unique values.")
  }
  if (length(conf.level) != 1) {
    stop("conf.level should have length one.")
  }
  if (!is.numeric(conf.level)) {
    stop("conf.level should be numeric.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_test_stat <- function(x, Q, fctr) {
    value <- as.numeric(quantile(x, Q))
    x <- ifelse(x <= value, 1, 0)

    calc_null_p <- function(x, fctr) {
      calc_lambdas <- function(x) {
        g <- function(lambda, level) {
          grandMean <- mean(x)
          n <- length(x)

          tempX <- x[fctr == level]

          numerator <- tempX - grandMean
          denominator <- length(tempX) * lambda * (tempX - grandMean) + n
          denominator[is.nan(denominator)] <- -Inf
          out <- sum(numerator / denominator)
          return(out)
        }
        n <- length(x)
        lambdas <- vector(mode = "numeric", length = length(levels(fctr)))
        for (i in seq(lambdas)) {
          level <- levels(fctr)[i]
          tempX <- x[fctr == level]
          ni <- length(tempX)
          LB <- (1 - n) / (ni * (max(tempX) - mean(x)))
          UB <- (1 - n) / (ni * (min(tempX) - mean(x)))
          lambdas[i] <- stats::uniroot(g, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes", level = level)$root
        }

        return(lambdas)
      }
      lambdas <- calc_lambdas(x)

      p <- vector(mode = "numeric", length = length(x))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)

        tempX <- x[index]
        p[index] <- 1 / (length(tempX) * lambdas[i] * (tempX - mean(x)) + length(x))
      }

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    calc_obs_p <- function(x, fctr) {
      p <- vector(mode = "numeric", length = length(x))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)

        tempX <- x[index]
        p[index] <- rep(1 / length(tempX), length(tempX))
      }
      p <- p / sum(p)

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)
    }

    null_p <- calc_null_p(x, fctr)
    obs_p <- calc_obs_p(x, fctr)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0)

    return(W)
  }

  W <- calc_test_stat(x, Q, fctr)

  # Under null, 1 parameter (overall value) is allowed to vary
  # Under alternative, parameter for each group is allowed to vary
  df <- length(levels(fctr)) - 1

  p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

  # Bonferroni correction and convert back to confidence
  alpha <- 1 - conf.level
  alpha <- alpha / length(levels(fctr))
  individual.conf.level <- 1 - alpha

  CI <- list()
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempCI <- empirical_quantile_one_sample(tempX, Q, mean(tempX), "two.sided", individual.conf.level)
    tempCI <- tempCI$conf.int
    CI[[l]] <- tempCI
  }

  out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
  class(out) <- c("one_way_case_three", "lrtest")
  return(out)
}

###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- empirical_quantile_one_sample(x, .50, 0, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
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
    expect_true(ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .5, CI1, alt)$p.value, .05) >= .04)
    expect_true(ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .5, CI2, alt)$p.value, .05) >= .04)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- empirical_quantile_one_sample(x, .50, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
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
    ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .50, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- empirical_quantile_one_sample(x, .50, -2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
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
    ifelse(is.finite(CI1), empirical_quantile_one_sample(x, .50, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_quantile_one_sample(x, .50, CI2, alt)$p.value, .05)
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
  expect_error(empirical_quantile_one_sample(c()), "Argument x should have positive length.")
  expect_error(empirical_quantile_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("Q input checking works", {
  expect_error(empirical_quantile_one_sample(x, c(1, 2)), "The Q parameter should have length one.")
  expect_error(empirical_quantile_one_sample(x, "foo"), "The Q parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, 0), "Q should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, 1), "Q should between zero and one.")
})

test_that("value input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, c(1, 2)), "The value parameter should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, "foo"), "he value parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, .50, min(x)), "The value parameter must be greater than the min of x.")
  expect_error(empirical_quantile_one_sample(x, .50, max(x)), "The value parameter must be less than the max of x.")
})


test_that("alternative input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, .50, 0, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_quantile_one_way(x, .50, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_quantile_one_sample(x[which(fctr == 1)], .50, 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Null False
###############################################

set.seed(2)
x <- c(rnorm(50, 1.5, 1), rnorm(50, 2, 1), rnorm(50, 2.25, 1))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_quantile_one_way(x, .50, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_quantile_one_sample(x[which(fctr == 1)], .50, 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2)

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_quantile_one_way(c()), "Argument x should have positive length.")
  expect_error(empirical_quantile_one_way("foo"), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("Q input checking works", {
  expect_error(empirical_quantile_one_way(x, c(1, 2)), "The Q parameter should have length one.")
  expect_error(empirical_quantile_one_way(x, "foo"), "The Q parameter should be numeric.")
  expect_error(empirical_quantile_one_sample(x, 0), "Q should between zero and one.")
  expect_error(empirical_quantile_one_sample(x, 1), "Q should between zero and one.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, "foo"), "Argument fctr should have same length as x.")
  expect_error(empirical_quantile_one_way(x, .50, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(empirical_quantile_one_way(x, .50, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(empirical_quantile_one_way(x, .50, fctr1), "Argument fctr should have at least two unique values.")
})
rm(fctr1)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_quantile_one_way(x, .50, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, "foo"), "conf.level should be numeric.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, 0), "conf.level should between zero and one.")
  expect_error(empirical_quantile_one_way(x, .50, fctr, 1), "conf.level should between zero and one.")
})
