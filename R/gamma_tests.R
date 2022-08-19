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
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_shape - shape) * W^.5
  }

  return(W)
}

#' Test the shape parameter of a gamma distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param shape a number indicating the tested value of the shape parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_shape_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 3, scale = 2)
#' gamma_shape_one_sample(x, 1, "greater")
#' @export
gamma_shape_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_shape, shape, 0)

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
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_scale - scale) * W^.5
  }

  return(W)
}

#' Test the scale parameter of a gamma distribution.
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
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_one_sample(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_one_sample(x, 1, "greater")
#' @export
gamma_scale_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_scale, scale, 0)

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
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_rate - rate) * W^.5
  }

  return(W)
}

#' Test the rate parameter of a gamma distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param rate a number indicating the tested value of the rate parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 1)
#' gamma_rate_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 2)
#' gamma_rate_one_sample(x, 1, "greater")
#' @export
gamma_rate_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_rate, rate, 0)

#' @keywords internal
calc_test_stat_gamma_shape_one_way <- function(x, fctr) {
  # null
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

  W1 <- sum(stats::dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_rate <- estimates[1] # pooled rate
      est_shapes <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in 1:length(likelihoods)) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shapes[i], rate = est_rate, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # starting points
    shapes <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in 1:length(shapes)) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]

      s <- log(mean(tempX)) - mean(log(tempX))
      shape <- (3 - s + ((s - 3)^2 + 24 * s)^.5) / (12 * s)
      # newton updates
      for (j in 1:10) {
        shape <- shape - (log(shape) - base::digamma(shape) - s) / ((1 / shape) - base::psigamma(shape, deriv = 1))
      }
      shapes[i] <- shape
      rm(j, shape)
    }

    start <- c(obs_rate, shapes)
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_rate_HA <- group_MLEs[1]
  group_shapes <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(likelihoods)) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = group_shapes[i], rate = profile_rate_HA, log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test equality of shapes of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All shapes are equal. (shape_1 = shape_2 ... shape_k).
#' \item Alternative: At least one shape is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 2, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_shape_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rgamma(50, 1, 2), rgamma(50, 2, 2), rgamma(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_shape_one_way(x, fctr, .95)
#' @export
gamma_shape_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_shape_one_way, gamma_shape_one_sample)

#' @keywords internal
calc_test_stat_gamma_scale_one_way <- function(x, fctr) {
  # null
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

  W1 <- sum(stats::dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape <- estimates[1] # pooled shape
      est_scales <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in 1:length(likelihoods)) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shape, scale = est_scales[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # starting points (MLEs from above)
    start <- c(obs_shape, rep(obs_scale, length(levels(fctr))))
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_shape_HA <- group_MLEs[1]
  group_scales <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(likelihoods)) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = profile_shape_HA, scale = group_scales[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test equality of scales of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: Null: All scales are equal. (scale_1 = scale_2 ... scale_k).
#' \item Alternative: At least one scale is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 1, scale = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_scale_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rgamma(50, 2, scale = 1), rgamma(50, 2, scale = 2), rgamma(50, 2, scale = 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_scale_one_way(x, fctr, .95)
#' @export
gamma_scale_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_scale_one_way, gamma_scale_one_sample)

#' @keywords internal
calc_test_stat_gamma_rate_one_way <- function(x, fctr) {
  # null
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

  W1 <- sum(stats::dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape <- estimates[1] # pooled shape
      est_rates <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in 1:length(likelihoods)) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shape, rate = est_rates[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # starting points (MLEs from above)
    start <- c(obs_shape, rep(obs_rate, length(levels(fctr))))
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_shape_HA <- group_MLEs[1]
  group_rates <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(likelihoods)) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = profile_shape_HA, rate = group_rates[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test equality of rates of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All rates are equal. (rate_1 = rate_2 ... rate_k).
#' \item Alternative: At least one rate is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 1, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_rate_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rgamma(50, 2, 1), rgamma(50, 2, 2), rgamma(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_rate_one_way(x, fctr, .95)
#' @export
gamma_rate_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_rate_one_way, gamma_rate_one_sample)
