#' @keywords internal
calc_test_stat_beta_shape1 <- function(x, shape1, alternative) {
  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_shape1 <- MLEs[1]
      est_shape2 <- MLEs[2]

      objective <- -1 * sum(stats::dbeta(x = x, shape1 = est_shape1, shape2 = est_shape2, log = TRUE))

      return(objective)
    }

    # starting points (method of moments)
    xbar <- base::mean(x)
    vbar <- stats::var(x)
    C <- (xbar * (1 - xbar)) / vbar - 1
    shape1_start <- xbar * C
    shape2_start <- (1 - xbar) * C
    MLEstart <- c(shape1_start, shape2_start)
    rm(xbar, vbar, C, shape1_start, shape2_start)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]
  rm(MLEs)

  get_profile_shape2 <- function(x, shape1) {
    # negative log likelihood
    profile_helper <- function(shape2) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(shape1, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_shape2)
  }
  profile_shape2 <- get_profile_shape2(x, shape1)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = shape1, shape2 = profile_shape2, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_shape1 - shape1) * W^.5
  }

  return(W)
}

#' Test the shape1 parameter of a beta distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param shape1 a number indicating the tested value of the shape1 parameter.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 2)
#' beta_shape1_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 3, shape2 = 2)
#' beta_shape1_lr_test(x, 1, "greater")
#' @export
beta_shape1_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_beta_shape1, shape1, 0)

#' @keywords internal
calc_test_stat_beta_shape2 <- function(x, shape2, alternative) {
  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_shape1 <- MLEs[1]
      est_shape2 <- MLEs[2]

      objective <- -1 * sum(stats::dbeta(x = x, shape1 = est_shape1, shape2 = est_shape2, log = TRUE))

      return(objective)
    }

    # starting points (method of moments)
    xbar <- base::mean(x)
    vbar <- stats::var(x)
    C <- (xbar * (1 - xbar)) / vbar - 1
    shape1_start <- xbar * C
    shape2_start <- (1 - xbar) * C
    MLEstart <- c(shape1_start, shape2_start)
    rm(xbar, vbar, C, shape1_start, shape2_start)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]

  get_profile_shape2 <- function(x, shape2) {
    # negative log likelihood
    profile_helper <- function(shape1) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(shape2, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_shape2)
  }
  profile_shape1 <- get_profile_shape2(x, shape2)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = profile_shape1, shape2 = shape2, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_shape2 - shape2) * W^.5
  }

  return(W)
}

#' Test the shape2 parameter of a beta distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param shape2 a number indicating the tested value of the shape2 parameter.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 1)
#' beta_shape2_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 3)
#' beta_shape2_lr_test(x, 1, "greater")
#' @export
beta_shape2_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_beta_shape2, shape2, 0)
