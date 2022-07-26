#' @keywords internal
calc_test_stat_gamma_shape <- function(x, shape, alternative) {
  get_MLEs <- function(x) {
    # Based on wiki page for gamma distribution.
    # starting points
    s <- log(mean(x)) - mean(log(x))
    shape <- (3 - s + ((s - 3)^2 + 24 * s)^.5) / (12 * s)

    # newton updates
    for (i in 1:10) {
      shape <- shape - (log(shape) - base::digamma(shape) - s) / ((1 / shape) - base::psigamma(shape, deriv = 1))
    }

    scale <- sum(x) / (shape * length(x))
    rate <- 1 / scale
    MLEs <- c(shape, rate)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_shape <- MLEs[1]
  obs_rate <- MLEs[2]
  obs_scale <- 1 / obs_rate

  # Profile scale/rate based on null hypothesis shape
  profile_scale <- mean(x) / shape
  profile_rate <- 1 / profile_scale

  W <- 2 * (sum(stats::dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
    sum(stats::dgamma(x = x, shape = shape, rate = profile_rate, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_shape - shape) * W^.5
  }

  return(W)
}

#' Test the shape parameter of a gamma distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param shape a number indicating the tested value of the shape parameter.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_shape_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 3, scale = 2)
#' gamma_shape_lr_test(x, 1, "greater")
#' @export
gamma_shape_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_gamma_shape, shape, 0)

#' @keywords internal
calc_test_stat_gamma_scale <- function(x, scale, alternative) {
  get_MLEs <- function(x) {
    # Based on wiki page for gamma distribution.
    # starting points
    s <- log(mean(x)) - mean(log(x))
    shape <- (3 - s + ((s - 3)^2 + 24 * s)^.5) / (12 * s)

    # newton updates
    for (i in 1:10) {
      shape <- shape - (log(shape) - base::digamma(shape) - s) / ((1 / shape) - base::psigamma(shape, deriv = 1))
    }

    scale <- sum(x) / (shape * length(x))
    rate <- 1 / scale
    MLEs <- c(shape, rate)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_shape <- MLEs[1]
  obs_rate <- MLEs[2]
  obs_scale <- 1 / obs_rate

  get_profile_shape <- function(x, scale) {
    scale <- pmax(scale, .0000001) # Avoid underflow in bounds of search
    geo_mean <- function(x) {
      return(exp(mean(log(x))))
    }

    profile_helper <- function(shape) {
      return(base::digamma(shape) - log(geo_mean(x) / scale))
    }

    profile_shape <- stats::uniroot(profile_helper, lower = geo_mean(x) / scale, upper = geo_mean(x) / scale + 1)$root

    return(profile_shape)
  }

  profile_shape <- get_profile_shape(x, scale)

  W <- 2 * (sum(stats::dgamma(x = x, shape = obs_shape, scale = obs_scale, log = TRUE)) -
    sum(stats::dgamma(x = x, shape = profile_shape, scale = scale, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_scale - scale) * W^.5
  }

  return(W)
}

#' Test the scale parameter of a gamma distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param scale a number indicating the tested value of the scale parameter.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_lr_test(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_lr_test(x, 1, "greater")
#' @export
gamma_scale_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_gamma_scale, scale, 0)

#' @keywords internal
calc_test_stat_gamma_rate <- function(x, rate, alternative) {
  get_MLEs <- function(x) {
    # Based on wiki page for gamma distribution.
    # starting points
    s <- log(mean(x)) - mean(log(x))
    shape <- (3 - s + ((s - 3)^2 + 24 * s)^.5) / (12 * s)

    # newton updates
    for (i in 1:10) {
      shape <- shape - (log(shape) - base::digamma(shape) - s) / ((1 / shape) - base::psigamma(shape, deriv = 1))
    }

    scale <- sum(x) / (shape * length(x))
    rate <- 1 / scale
    MLEs <- c(shape, rate)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_shape <- MLEs[1]
  obs_rate <- MLEs[2]
  obs_scale <- 1 / obs_rate

  get_profile_shape <- function(x, rate) {
    scale <- 1 / rate
    scale <- pmax(scale, .0000001) # Avoid underflow in bounds of search
    geo_mean <- function(x) {
      return(exp(mean(log(x))))
    }

    profile_helper <- function(shape) {
      return(base::digamma(shape) - log(geo_mean(x) / scale))
    }

    profile_shape <- stats::uniroot(profile_helper, lower = geo_mean(x) / scale, upper = geo_mean(x) / scale + 1)$root

    return(profile_shape)
  }

  profile_shape <- get_profile_shape(x, rate)

  W <- 2 * (sum(stats::dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
    sum(stats::dgamma(x = x, shape = profile_shape, rate = rate, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_rate - rate) * W^.5
  }

  return(W)
}

#' Test the rate parameter of a gamma distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param rate a number indicating the tested value of the rate parameter.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 1)
#' gamma_rate_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 2)
#' gamma_rate_lr_test(x, 1, "greater")
#' @export
gamma_rate_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_gamma_rate, rate, 0)
