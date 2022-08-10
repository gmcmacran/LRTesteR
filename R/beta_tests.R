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

  get_profile_shape2 <- function(x, MLE) {
    # negative log likelihood
    profile_helper <- function(shape2) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(MLE, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_shape2)
  }
  profile_shape2 <- get_profile_shape2(x, obs_shape2)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = shape1, shape2 = profile_shape2, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_shape1 - shape1) * W^.5
  }

  return(W)
}

#' Test the shape1 parameter of a beta distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param shape1 a number indicating the tested value of the shape1 parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 2)
#' beta_shape1_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 3, shape2 = 2)
#' beta_shape1_one_sample(x, 1, "greater")
#' @export
beta_shape1_one_sample <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_beta_shape1, shape1, 0)

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

  get_profile_shape1 <- function(x, MLE) {
    # negative log likelihood
    profile_helper <- function(shape1) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape1 <- stats::optim(MLE, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(profile_shape1)
  }
  profile_shape1 <- get_profile_shape1(x, obs_shape1)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = profile_shape1, shape2 = shape2, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_shape2 - shape2) * W^.5
  }

  return(W)
}

#' Test the shape2 parameter of a beta distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param shape2 a number indicating the tested value of the shape2 parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 1)
#' beta_shape2_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 3)
#' beta_shape2_one_sample(x, 1, "greater")
#' @export
beta_shape2_one_sample <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_beta_shape2, shape2, 0)

#' @keywords internal
calc_test_stat_beta_shape1_one_way <- function(x, fctr) {
  # null
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

  W1 <- sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape2 <- estimates[1] # pooled shape2
      est_shape1s <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in 1:length(likelihoods)) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = est_shape1s[i], shape2 = est_shape2, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # MOMs
    shape1s <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in 1:length(shape1s)) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      xbar <- base::mean(tempX)
      vbar <- stats::var(tempX)
      C <- (xbar * (1 - xbar)) / vbar - 1
      shape1s[i] <- xbar * C
    }

    # starting points (MLE for pooled estimate and group wise MOMs)
    start <- c(obs_shape2, shape1s)
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_shape2_HA <- group_MLEs[1]
  group_shape1 <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(likelihoods)) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = group_shape1[i], shape2 = profile_shape2_HA, log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)

  return(W)
}

#' Test equality of shape 1 of beta distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' Null: All shape1s are equal. (shape1_1 = shape1_2 ... shape1_k).
#' Alternative: At least one shape1 is not equal.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(150, 1, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape1_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rbeta(50, 1, 2), rbeta(50, 2, 2), rbeta(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape1_one_way(x, fctr, .95)
#' @export
beta_shape1_one_way <- create_test_function_continuous_one_way(LRTesteR:::calc_test_stat_beta_shape1_one_way, beta_shape1_one_sample)

calc_test_stat_beta_shape2_one_way <- function(x, fctr) {
  # null
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

  W1 <- sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape1 <- estimates[1] # pooled shape1
      est_shape2s <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in 1:length(likelihoods)) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = est_shape1, shape2 = est_shape2s[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # MOMs
    shape2s <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in 1:length(shape2s)) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      xbar <- base::mean(tempX)
      vbar <- stats::var(tempX)
      C <- (xbar * (1 - xbar)) / vbar - 1
      shape2s[i] <- (1 - xbar) * C
    }
    # starting points (MLEs from above)
    start <- c(obs_shape1, shape2s)
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_shape1_HA <- group_MLEs[1]
  group_shape2 <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(likelihoods)) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = profile_shape1_HA, shape2 = group_shape2[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)

  return(W)
}

#' Test equality of shape 2 of beta distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' Null: All shape2s are equal. (shape2_1 = shape2_2 ... shape2_k).
#' Alternative: At least one shape2 is not equal.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(150, 2, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape2_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rbeta(50, 2, 1), rbeta(50, 2, 2), rbeta(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape2_one_way(x, fctr, .95)
#' @export
beta_shape2_one_way <- create_test_function_continuous_one_way(LRTesteR:::calc_test_stat_beta_shape2_one_way, beta_shape2_one_sample)
