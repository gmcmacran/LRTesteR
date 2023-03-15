#' Test the mean parameter of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x a numeric vector.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' empirical_mean_one_sample(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(25, 2, 1)
#' empirical_mean_one_sample(x, 1, "greater")
#' @export
empirical_mean_one_sample <- function(x, mu, alternative = "two.sided", conf.level = .95) {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(mu) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(mu)) {
    stop("The tested parameter should be numeric.")
  }
  if (mu <= min(x)) {
    stop("The tested parameter must be greater than the min of x.")
  }
  if (mu >= max(x)) {
    stop("The tested parameter must be less than the max of x.")
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
  if (conf.level <= 0 | conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_test_stat <- function(x, mu, alternative) {
    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    calc_null_p <- function(x, mu) {
      calc_lambda <- function(x, mu) {
        g <- function(lambda) {
          numerator <- x - mu
          denominator <- n - lambda * (x - mu)
          denominator[is.nan(denominator)] <- -Inf
          out <- sum(numerator / denominator)
          return(out)
        }
        n <- length(x)
        if (mu < mean(x)) {
          LB <- n / (min(x) - mu)
          UB <- pmin(n / (max(x) - mu), 0)

          if (g(LB) == 0) {
            lambda <- LB
          } else {
            while (g(LB) > 0) {
              # underflow. Lowering LB
              LB <- LB * 1.01
            }

            lambda <- stats::uniroot(g, lower = LB, upper = UB, tol = .Machine$double.eps^.50)$root
          }
        } else if (mu > mean(x)) {
          LB <- pmax(n / (min(x) - mu), 0)
          UB <- n / (max(x) - mu)

          if (g(UB) == 0) {
            lambda <- UB
          } else {
            while (g(UB) < 0) {
              # underflow. lowering UB
              UB <- UB * .99
            }

            lambda <- stats::uniroot(g, lower = LB, upper = UB)$root
          }
        } else {
          lambda <- 0
        } # null's mu is xbar

        return(lambda)
      }

      lambda <- calc_lambda(x, mu)
      phi <- -length(x) - lambda * mu

      p <- -1 / (phi + lambda * x)

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p(x, mu)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    if (alternative != "two.sided") {
      W <- sign(mean(x) - mu) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level

    calc_left_side_CI <- function(alpha) {
      param <- 0
      helper <- function(param) {
        param <<- param
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- min(x) + 10 * .Machine$double.eps
      UB <- max(x) - 10 * .Machine$double.eps

      out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50)$root
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      holder <- -1000
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        if (is.na(out) | !is.finite(out)) {
          holder <<- param
        }
        return(out)
      }
      LB <- min(x) + 10 * .Machine$double.eps
      UB <- max(x) - 10 * .Machine$double.eps

      helper(LB)
      helper(UB)

      out <- stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50)$root

      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
    } else if (alternative == "less") {
      CI <- c(-Inf, calc_right_side_CI(alpha))
    } else {
      CI <- c(calc_left_side_CI(alpha), Inf)
    }

    return(CI)
  }

  W <- calc_test_stat(x, mu, alternative)

  # calculate p value
  if (alternative == "two.sided") {
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  } else if (alternative == "less") {
    p.value <- stats::pnorm(q = W, lower.tail = TRUE)
  } else {
    p.value <- stats::pnorm(q = W, lower.tail = FALSE)
  }

  CI <- calc_CI(x, alternative, conf.level)

  out <- list(statistic = W, p.value = p.value, conf.int = CI, conf.level = conf.level, alternative = alternative)
  class(out) <- c("one_sample_case_one", "lrtest")

  return(out)
}
