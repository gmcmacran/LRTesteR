#' Test a quantile of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x a numeric vector.
#' @param Q The quantile. A single numeric number. (.50 is median.)
#' @param value A single numeric value that is the hypothesized Q quantile.
#' @inherit gaussian_mu_one_sample return
#' @source \itemize{
#' \item Yudi Pawitan. In All Likelihood. Oxford University Press.
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' }
#' @details
#' For confidence intervals, an endpoint may be outside the observed range of x.
#' In this case, NA is returned. Reducing confidence or collecting more data
#' will make the CI computable.
#'
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
#' empirical_quantile_one_sample(x, .5, 1, "greater")
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
    Q <- 1 - Q
    x <- ifelse(x > value, 1, 0)

    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    calc_null_p <- function(x, Q) {
      # Lambda is calculated two ways.
      # The first way works well most of the time. Sometimes, the range is
      # expanded too much in uniroot's extendInt = "yes". If this happens,
      # a secondary problem is optimized.
      # 1 comes from In All Likelihood book.
      # 2 comes from Empirical Likelihood book.
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
      calc_lambda_two <- function(x, Q) {
        g <- function(lambda) {
          numerator <- x - Q
          denominator <- 1 + lambda * (x - Q)
          # dropped 1 divided by n
          # Should reduce floating point issues when x is large.
          # Searching for zero, so same zero will be found.
          # Just scales the objective function up.
          out <- sum(numerator / denominator)
          return(out)
        }
        n <- length(x)
        if (Q != mean(x)) {
          LB <- (1 - 1 / n) / (Q - max(x))
          UB <- (1 - 1 / n) / (Q - min(x))

          if (g(LB) == 0) {
            lambda <- LB
          } else if (g(UB) == 0) {
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

      if (min(p) < 0 || max(p) > 1 || sum(p) != 1) {
        lambda <- calc_lambda_two(x, Q)
        p <- 1 / (1 + lambda * (x - Q)) * (1 / length(x))
      }
      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p(x, Q)

    check_empirical_optimization(obs_p)
    check_empirical_optimization(null_p)

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
      UB <- max(x) - .01

      if (sign(helper(LB)) != sign(helper(UB))) {
        out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")
        out <- out$root - out$estim.prec
        out <- max(x[which(x <= out)])
      } else {
        out <- NA_real_
      }


      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, Q, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- min(x) + .01
      UB <- max(x) - .01

      if (sign(helper(LB)) != sign(helper(UB))) {
        out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")
        out <- out$root + out$estim.prec
        out <- max(x[which(x <= out)])
      } else {
        out <- NA_real_
      }


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

#' Test the equality of a quantile from an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_way
#' @param x a numeric vector.
#' @param Q The quantile. A single numeric number. (.50 is median.)
#' @inherit gaussian_mu_one_way return
#' @inherit empirical_mu_one_sample source
#' @details
#' \itemize{
#' \item Null: Quantiles are equal. (Q1 = Q2 ... Qk).
#' \item Alternative: At least one quantile is not equal.
#' }
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
  # Confirm optimization problem is solvable
  if (any(as.vector(by(x, fctr, min)) >= as.numeric(stats::quantile(x, Q)))) {
    stop("Every group in x must have at least one data point less than the tested quantile.")
  }
  if (any(as.vector(by(x, fctr, max)) <= as.numeric(stats::quantile(x, Q)))) {
    stop("Every group in x must have at least one data point greater than the tested quantile.")
  }

  calc_test_stat <- function(x, Q, fctr) {
    value <- as.numeric(stats::quantile(x, Q))
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

          LB <- pmax(
            (1 - n) / (ni * (max(tempX) - mean(x))),
            -n / (ni * (max(tempX) - mean(x)))
          )
          LB <- LB + 10 * .Machine$double.eps # greater than, not greater than or equal to.

          UB <- pmin(
            (1 - n) / (ni * (min(tempX) - mean(x))),
            -n / (ni * (min(tempX) - mean(x)))
          )
          UB <- UB - 10 * .Machine$double.eps # less than, not less than or equal to.

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

    check_empirical_optimization(null_p)
    check_empirical_optimization(obs_p)

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
    tempCI <- empirical_quantile_one_sample(tempX, Q, as.numeric(stats::quantile(tempX, Q)), "two.sided", individual.conf.level)
    tempCI <- tempCI$conf.int
    CI[[l]] <- tempCI
  }

  out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
  class(out) <- c("one_way_case_three", "lrtest")
  return(out)
}
