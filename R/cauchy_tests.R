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

    start_location <- base::mean(x, trim = .38)
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
  W <- pmax(W, 0)

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
cauchy_location_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_cauchy_location, location, 15)

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

    start_location <- base::mean(x, trim = .38)
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
  W <- pmax(W, 0)

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
cauchy_scale_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_cauchy_scale, scale, 35, 0)

#' @keywords internal
calc_test_stat_cauchy_location_one_way <- function(x, fctr) {
  # null
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

    start_location <- base::mean(x, trim = .38)
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

  W1 <- sum(stats::dcauchy(x = x, location = obs_location, scale = obs_scale, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_scale <- estimates[1] # pooled scale
      est_locations <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = est_locations[i], scale = est_scale, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # starting points and bounds on location
    locations <- vector(mode = "numeric", length = length(levels(fctr)))
    searchLB <- vector(mode = "numeric", length = length(levels(fctr)))
    searchUB <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in seq_along(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      locations[i] <- base::mean(tempX, trim = .38)
      searchLB[i] <- -999999
      searchUB[i] <- 999999
    }

    # bounding scale by widest range possible range
    scaleLB <- base::min(base::abs(x - base::mean(x, trim = .38)))
    scaleUB <- base::max(base::abs(x - base::mean(x, trim = .38)))
    for (i in seq_along(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      scaleLB <- pmin(base::min(base::abs(tempX - locations[i])), scaleLB)
      scaleUB <- pmax(base::max(base::abs(tempX - locations[i])), scaleUB)
    }

    # combine bounds.
    # scale first b/c of how neg_log_likelihood splits arguments
    searchLB <- c(scaleLB, searchLB)
    searchUB <- c(scaleUB, searchUB)
    rm(scaleLB, scaleUB)

    start <- c(obs_scale, locations)

    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_scale_HA <- group_MLEs[1]
  group_locations <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = group_locations[i], scale = profile_scale_HA, log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of location parameters of cauchy distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item All locations are equal. (location_1 = location_2 ... location_k).
#' \item Alternative: At least one location is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 150, location = 1, scale = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_location_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rcauchy(50, 1, 2), rcauchy(50, 2, 2), rcauchy(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_location_one_way(x, fctr, .95)
#' @export
cauchy_location_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_cauchy_location_one_way, cauchy_location_one_sample, 30)

#' @keywords internal
calc_test_stat_cauchy_scale_one_way <- function(x, fctr) {
  # null
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

    start_location <- base::mean(x, trim = .38)
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

  W1 <- sum(stats::dcauchy(x = x, location = obs_location, scale = obs_scale, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      location <- estimates[1] # pooled location
      est_scales <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = location, scale = est_scales[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }
    # starting points and bounds on scales
    scales <- vector(mode = "numeric", length = length(levels(fctr)))
    searchLB <- vector(mode = "numeric", length = length(levels(fctr)))
    searchUB <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in seq_along(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      tempLocation <- base::mean(tempX, trim = .38)
      scales[i] <- stats::median(base::abs(tempX - tempLocation))
      searchLB[i] <- base::min(base::abs(tempX - tempLocation))
      searchUB[i] <- base::max(base::abs(tempX - tempLocation))
    }

    # Add bound on pooled location
    # location first b/c of how neg_log_likelihood splits arguments
    searchLB <- c(-999999, searchLB)
    searchUB <- c(999999, searchUB)

    start <- c(obs_location, scales)
    group_MLEs <- stats::optim(start, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    neg_log_likelihood(start)
    neg_log_likelihood(group_MLEs)

    start
    group_MLEs

    return(group_MLEs)
  }

  group_MLEs <- get_group_MLEs(x, fctr)
  profile_location_HA <- group_MLEs[1]
  group_scales <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = profile_location_HA, scale = group_scales[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of scale parameters of cauchy distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All scales are equal. (scale_1 = scale_2 ... scale_k).
#' \item Alternative: At least one scale is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 150, 1, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_scale_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rcauchy(50, 2, 1), rcauchy(50, 2, 2), rcauchy(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_scale_one_way(x, fctr, .95)
#' @export
cauchy_scale_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_cauchy_scale_one_way, cauchy_scale_one_sample, 70)
