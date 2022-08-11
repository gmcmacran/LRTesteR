#' @keywords internal
calc_test_stat_cauchy_location <- function(x, location, alternative) {
  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_location <- MLEs[1]
      est_scale <- MLEs[2]
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(est_scale * pi) - sum(log(1 + ((x - est_location) / est_scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    start_location <- stats::median(x)
    start_scale <- stats::median(base::abs(x - start_location))
    MLEstart <- c(start_location, start_scale)

    searchLB <- c(-999999, base::min(base::abs(x - start_location)))
    searchUB <- c(999999, base::max(base::abs(x - start_location)))

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)

  obs_location <- MLEs[1]
  obs_scale <- MLEs[2]
  rm(MLEs)

  get_profile_scale <- function(x, location) {
    # negative log likelihood
    profile_helper <- function(est_scale) {
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(est_scale * pi) - sum(log(1 + ((x - location) / est_scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    start_scale <- stats::median(base::abs(x - location))

    searchLB <- base::min(base::abs(x - location))
    searchUB <- base::max(base::abs(x - location))

    profile_scale <- stats::optim(start_scale, profile_helper, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_scale)
  }
  profile_scale <- get_profile_scale(x, location)

  W <- 2 * (sum(stats::dcauchy(x = x, location = obs_location, scale = obs_scale, log = TRUE)) -
    sum(stats::dcauchy(x = x, location = location, scale = profile_scale, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_location - location) * W^.5
  }

  return(W)
}

#' Test the location parameter of a cauchy distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param location a number indicating the tested value of the location parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 1, scale = 2)
#' cauchy_location_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 3, scale = 2)
#' cauchy_location_one_sample(x, 1, "greater")
#' @export
cauchy_location_one_sample <- LRTesteR:::create_test_function_one(LRTesteR:::calc_test_stat_cauchy_location, location)

#' @keywords internal
calc_test_stat_cauchy_scale <- function(x, scale, alternative) {
  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_location <- MLEs[1]
      est_scale <- MLEs[2]
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(est_scale * pi) - sum(log(1 + ((x - est_location) / est_scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    start_location <- stats::median(x)
    start_scale <- stats::median(base::abs(x - start_location))
    MLEstart <- c(start_location, start_scale)

    searchLB <- c(-999999, base::min(base::abs(x - start_location)))
    searchUB <- c(999999, base::max(base::abs(x - start_location)))

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)

  obs_location <- MLEs[1]
  obs_scale <- MLEs[2]
  rm(MLEs)

  get_profile_location <- function(x, scale) {
    # negative log likelihood
    profile_helper <- function(est_location) {
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(scale * pi) - sum(log(1 + ((x - est_location) / scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    start_location <- stats::median(x)

    searchLB <- -999999
    searchUB <- 999999

    profile_location <- stats::optim(start_location, profile_helper, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_location)
  }
  profile_location <- get_profile_location(x, scale)

  W <- 2 * (sum(stats::dcauchy(x = x, location = obs_location, scale = obs_scale, log = TRUE)) -
    sum(stats::dcauchy(x = x, location = profile_location, scale = scale, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_scale - scale) * W^.5
  }

  return(W)
}

#' Test the scale parameter of a cauchy distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param scale a number indicating the tested value of the scale parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 1, scale = 2)
#' cauchy_scale_one_sample(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 3, scale = 2)
#' cauchy_scale_one_sample(x, 1, "greater")
#' @export
cauchy_scale_one_sample <- LRTesteR:::create_test_function_one(LRTesteR:::calc_test_stat_cauchy_scale, scale, 0)
