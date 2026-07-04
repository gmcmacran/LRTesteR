#' Test the variance of an unknown distribution.
#'
#' @inheritParams gaussian_variance_one_sample
#' @param x a numeric vector.
#' @inherit gaussian_variance_one_sample return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item \url{https://github.com/statsmodels/statsmodels/blob/main/statsmodels/emplike/descriptive.py}
#' }
#' @details
#' The mean is a nuisance parameter and is profiled out of the likelihood.
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
#' empirical_variance_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(25, 0, 2)
#' empirical_variance_one_sample(x, 1, "greater")
#' @export
empirical_variance_one_sample <- function(x, sigma.squared, alternative = "two.sided", conf.level = .95) {
  if (length(x) < 3) {
    stop("Argument x should have at least three observations.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (max(x) == min(x)) {
    stop("Argument x should have at least two unique values.")
  }
  if (length(sigma.squared) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(sigma.squared)) {
    stop("The tested parameter should be numeric.")
  }
  if (sigma.squared <= 0) {
    stop("The tested parameter must be greater than zero.")
  }
  if (sigma.squared >= (max(x) - min(x))^2) {
    stop("The tested parameter must be less than the squared range of x.")
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

  calc_test_stat <- function(x, sigma.squared, alternative) {
    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    calc_null_p <- function(x, sigma.squared) {
      # Two constraints. One for the mean (nuisance) and one for the variance.
      # Matches _opt_var in statsmodels' emplike module.
      build_est_vect <- function(mu) {
        est_vect <- cbind(x - mu, (x - mu)^2 - sigma.squared)
        return(est_vect)
      }
      # The nuisance mean is profiled out. It must stay strictly inside the
      # range of x for the mean constraint to be solvable.
      profile_helper <- function(mu) {
        out <- calc_el_solution(build_est_vect(mu))$W
        return(out)
      }
      # The profile can have disjoint feasible regions in mu, so a coarse
      # grid search brackets the minimum before Brent polishes it.
      buffer <- (max(x) - min(x)) * .Machine$double.eps^.5
      mus <- seq(min(x) + buffer, max(x) - buffer, length.out = 21)
      Ws <- vapply(mus, profile_helper, numeric(1))
      i <- which.min(Ws)
      opt <- stats::optim(
        par = mus[i], fn = profile_helper, method = "Brent",
        lower = mus[max(i - 1, 1)], upper = mus[min(i + 1, length(mus))]
      )

      p <- calc_el_solution(build_est_vect(opt$par))$p

      # division by near zero numbers can cause -Inf and Inf
      # underflow
      p <- pmax(p, .Machine$double.eps)
      p <- pmin(p, 1 - .Machine$double.eps)

      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p(x, sigma.squared)

    check_empirical_optimization(obs_p)
    check_empirical_optimization(null_p)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0) # underflow
    if (alternative != "two.sided") {
      obs_variance <- mean((x - mean(x))^2)
      W <- sign(obs_variance - sigma.squared) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level
    obs_variance <- mean((x - mean(x))^2)

    # Roots are found on the log scale so candidate variances stay positive.
    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, exp(param), "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- log(obs_variance) - 1
      UB <- log(obs_variance)

      out <- tryCatch(
        exp(stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root),
        error = function(e) NA_real_
      )
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, exp(param), "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- log(obs_variance)
      UB <- log(obs_variance) + 1

      out <- tryCatch(
        exp(stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root),
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

  W <- calc_test_stat(x, sigma.squared, alternative)

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
