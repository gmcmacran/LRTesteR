#' Test the skewness of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x a numeric vector.
#' @param skewness a number indicating the tested value of skewness.
#' @inherit gaussian_mu_one_sample return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item \url{https://github.com/statsmodels/statsmodels/blob/main/statsmodels/emplike/descriptive.py}
#' }
#' @details
#' The mean and variance are nuisance parameters and are profiled out of the
#' likelihood.
#'
#' For confidence intervals, an endpoint may not be computable. In this case,
#' NA is returned. Reducing confidence or collecting more data
#' will make the CI computable.
#'
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' empirical_skewness_one_sample(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rexp(25, 1)
#' empirical_skewness_one_sample(x, 0, "greater")
#' @export
empirical_skewness_one_sample <- function(x, skewness, alternative = "two.sided", conf.level = .95) {
  if (length(x) < 4) {
    stop("Argument x should have at least four observations.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (max(x) == min(x)) {
    stop("Argument x should have at least two unique values.")
  }
  if (length(skewness) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(skewness)) {
    stop("The tested parameter should be numeric.")
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

  calc_test_stat <- function(x, skewness, alternative) {
    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    calc_null_p <- function(x, skewness) {
      # Three constraints. One for the mean (nuisance), one for the
      # variance (nuisance), and one for skewness.
      # Matches _opt_skew in statsmodels' emplike module.
      # Variance is optimized on the log scale to keep it positive.
      build_est_vect <- function(nuisance) {
        mu <- nuisance[1]
        sigma.squared <- exp(nuisance[2])
        est_vect <- cbind(
          x - mu,
          (x - mu)^2 - sigma.squared,
          (x - mu)^3 / sigma.squared^1.5 - skewness
        )
        return(est_vect)
      }
      profile_helper <- function(nuisance) {
        out <- calc_el_solution(build_est_vect(nuisance))$W
        return(out)
      }
      # The feasible region may not contain the sample moments, so a coarse
      # grid search over the variance picks the starting point. Nelder-Mead
      # is run twice to polish the solution.
      log_s2_grid <- log(mean((x - mean(x))^2)) + seq(-3, 2, length.out = 11)
      Ws <- vapply(log_s2_grid, function(ls2) profile_helper(c(mean(x), ls2)), numeric(1))
      start <- c(mean(x), log_s2_grid[which.min(Ws)])
      opt <- stats::optim(par = start, fn = profile_helper, method = "Nelder-Mead")
      opt <- stats::optim(par = opt$par, fn = profile_helper, method = "Nelder-Mead")

      p <- calc_el_solution(build_est_vect(opt$par))$p

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p(x, skewness)

    check_empirical_optimization(obs_p)
    check_empirical_optimization(null_p)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0) # underflow
    if (alternative != "two.sided") {
      obs_skewness <- mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
      W <- sign(obs_skewness - skewness) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level
    obs_skewness <- mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- obs_skewness - 1
      UB <- obs_skewness

      out <- tryCatch(
        stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root,
        error = function(e) NA_real_
      )
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- obs_skewness
      UB <- obs_skewness + 1

      out <- tryCatch(
        stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root,
        error = function(e) NA_real_
      )
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

  W <- calc_test_stat(x, skewness, alternative)

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
  class(out) <- c("one_sample_case_three", "lrtest")

  return(out)
}
