#' @keywords internal
calc_test_stat_gamma_shape <- function(x, shape, alternative) {
  MLEs <- unname(EnvStats::egamma(x, method = "mle")$parameters)
  MLEs[2] <- 1 / MLEs[2] # convert to rate
  obs_shape <- MLEs[1]
  obs_rate <- MLEs[2]

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
#' @inheritParams gaussian_mu_test
#' @param shape a number indicating the tested value of the shape parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_shape_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 3, scale = 2)
#' gamma_shape_test(x, 1, "greater")
#' @export
gamma_shape_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_shape, shape, 45, 0)

#' @keywords internal
calc_test_stat_gamma_scale <- function(x, scale, alternative) {
  MLEs <- unname(EnvStats::egamma(x, method = "mle")$parameters)
  MLEs[2] <- 1 / MLEs[2] # convert to rate

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
#' @inheritParams gaussian_mu_test
#' @param scale a number indicating the tested value of the scale parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_test(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_test(x, 1, "greater")
#' @export
gamma_scale_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_scale, scale, 45, 0)

#' @keywords internal
calc_test_stat_gamma_rate <- function(x, rate, alternative) {
  MLEs <- unname(EnvStats::egamma(x, method = "mle")$parameters)
  MLEs[2] <- 1 / MLEs[2] # convert to rate
  obs_shape <- MLEs[1]
  obs_rate <- MLEs[2]

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
#' @inheritParams gaussian_mu_test
#' @param rate a number indicating the tested value of the rate parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 1)
#' gamma_rate_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 2)
#' gamma_rate_test(x, 1, "greater")
#' @export
gamma_rate_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_gamma_rate, rate, 45, 0)

#' @keywords internal
calc_gamma_group_MLEs <- function(x, fctr) {
  # Unrestricted per group MLEs. These are the alternative hypothesis of every
  # gamma one way test. No parameter is shared there, so the likelihood
  # separates and each group is fit on its own instead of searching 2k
  # dimensions at once. They also start the restricted fits.

  group_shapes <- vector(mode = "numeric", length = length(levels(fctr)))
  group_scales <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    MLEs <- unname(EnvStats::egamma(tempX, method = "mle")$parameters)
    group_shapes[i] <- MLEs[1]
    group_scales[i] <- MLEs[2]
  }

  group_MLEs <- list(
    shape = pmax(group_shapes, .Machine$double.eps),
    scale = pmax(group_scales, .Machine$double.eps),
    rate = pmax(1 / group_scales, .Machine$double.eps)
  )
  return(group_MLEs)
}

#' @keywords internal
calc_test_stat_gamma_shape_one_way <- function(x, fctr) {
  # Rates are nuisance parameters under both hypotheses. Only the shape is
  # restricted by the null, so the groups are not required to share a rate.
  # The null estimates one shape and k rates. The alternative estimates k shapes
  # and k rates.

  # alt
  group_MLEs <- calc_gamma_group_MLEs(x, fctr)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = group_MLEs$shape[i], rate = group_MLEs$rate[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape <- pmax(estimates[1], .Machine$double.eps) # pooled shape
      est_rates <- pmax(estimates[2:length(estimates)], .Machine$double.eps)

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shape, rate = est_rates[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # starting points (group wise MLEs from the alternative)
    # shape first b/c of how neg_log_likelihood splits arguments
    start <- c(base::mean(group_MLEs$shape), group_MLEs$rate)
    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    null_MLEs <- pmax(null_MLEs, .Machine$double.eps) # Per pdf definition, parameter must be positive.
    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_shape_H0 <- null_MLEs[1]
  null_rates <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = profile_shape_H0, rate = null_rates[i], log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of shape parameters of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All shapes are equal. (shape_1 = shape_2 ... shape_k).
#' \item Alternative: At least one shape is not equal.
#' }
#' The rates are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 2, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_shape_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rgamma(50, 1, 2), rgamma(50, 2, 2), rgamma(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_shape_one_way_test(x, fctr, .95)
#' @export
gamma_shape_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_shape_one_way, gamma_shape_test, 90)

#' @keywords internal
calc_test_stat_gamma_scale_one_way <- function(x, fctr) {
  # Shapes are nuisance parameters under both hypotheses. Only the scale is
  # restricted by the null, so the groups are not required to share a shape.
  # The null estimates k shapes and one scale. The alternative estimates k
  # shapes and k scales.

  # alt
  group_MLEs <- calc_gamma_group_MLEs(x, fctr)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = group_MLEs$shape[i], scale = group_MLEs$scale[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_scale <- pmax(estimates[1], .Machine$double.eps) # pooled scale
      est_shapes <- pmax(estimates[2:length(estimates)], .Machine$double.eps)

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shapes[i], scale = est_scale, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # starting points (group wise MLEs from the alternative)
    # scale first b/c of how neg_log_likelihood splits arguments
    start <- c(base::mean(group_MLEs$scale), group_MLEs$shape)
    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    null_MLEs <- pmax(null_MLEs, .Machine$double.eps) # Per pdf definition, parameter must be positive.
    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_scale_H0 <- null_MLEs[1]
  null_shapes <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = null_shapes[i], scale = profile_scale_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of scale parameters of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All scales are equal. (scale_1 = scale_2 ... scale_k).
#' \item Alternative: At least one scale is not equal.
#' }
#' The shapes are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 1, scale = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_scale_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rgamma(50, 2, scale = 1), rgamma(50, 2, scale = 2), rgamma(50, 2, scale = 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_scale_one_way_test(x, fctr, .95)
#' @export
gamma_scale_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_scale_one_way, gamma_scale_test, 90)

#' @keywords internal
calc_test_stat_gamma_rate_one_way <- function(x, fctr) {
  # Shapes are nuisance parameters under both hypotheses. Only the rate is
  # restricted by the null, so the groups are not required to share a shape.
  # The null estimates k shapes and one rate. The alternative estimates k shapes
  # and k rates.

  # alt
  group_MLEs <- calc_gamma_group_MLEs(x, fctr)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = group_MLEs$shape[i], rate = group_MLEs$rate[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_rate <- pmax(estimates[1], .Machine$double.eps) # pooled rate
      est_shapes <- pmax(estimates[2:length(estimates)], .Machine$double.eps)

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = est_shapes[i], rate = est_rate, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # starting points (group wise MLEs from the alternative)
    # rate first b/c of how neg_log_likelihood splits arguments
    start <- c(base::mean(group_MLEs$rate), group_MLEs$shape)
    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    null_MLEs <- pmax(null_MLEs, .Machine$double.eps) # Per pdf definition, parameter must be positive.
    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_rate_H0 <- null_MLEs[1]
  null_shapes <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dgamma(x = tempX, shape = null_shapes[i], rate = profile_rate_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of rate parameters of gamma distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All rates are equal. (rate_1 = rate_2 ... rate_k).
#' \item Alternative: At least one rate is not equal.
#' }
#' The shapes are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rgamma(150, 1, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_rate_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(5)
#' x <- c(rgamma(50, 2, 1), rgamma(50, 2, 2), rgamma(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gamma_rate_one_way_test(x, fctr, .95)
#' @export
gamma_rate_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_gamma_rate_one_way, gamma_rate_test, 90)
