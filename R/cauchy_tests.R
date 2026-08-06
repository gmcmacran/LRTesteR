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
#' @inheritParams gaussian_mu_test
#' @param location a number indicating the tested value of the location parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 1, scale = 2)
#' cauchy_location_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 3, scale = 2)
#' cauchy_location_test(x, 1, "greater")
#' @export
cauchy_location_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_cauchy_location, location, 15)

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
#' @inheritParams gaussian_mu_test
#' @param scale a number indicating the tested value of the scale parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 1, scale = 2)
#' cauchy_scale_test(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rcauchy(n = 100, location = 3, scale = 2)
#' cauchy_scale_test(x, 1, "greater")
#' @export
cauchy_scale_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_cauchy_scale, scale, 35, 0)

#' @keywords internal
calc_test_stat_cauchy_location_one_way <- function(x, fctr) {
  # Scales are nuisance parameters under both hypotheses. Only the location is
  # restricted by the null, so the groups are not required to share a scale.
  # The null estimates one location and k scales. The alternative estimates k
  # locations and k scales.

  # Starting points for every location.
  start_locations <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    start_locations[i] <- base::mean(tempX, trim = .38)
  }

  # Deviations about each group's location bound every scale in both models.
  # Sharing one set of bounds keeps the null's scales inside the alternative's
  # search region. Bounding each group by its own deviations would let the null
  # sit outside the alternative and produce a negative test statistic.
  deviations <- base::abs(x - start_locations[base::as.integer(fctr)])
  scaleLB <- base::min(deviations)
  scaleUB <- base::max(deviations)
  start_scales <- rep(stats::median(deviations), length(levels(fctr)))
  rm(deviations)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_location <- estimates[1] # pooled location
      est_scales <- estimates[2:length(estimates)]

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = est_location, scale = est_scales[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # combine bounds.
    # location first b/c of how neg_log_likelihood splits arguments
    searchLB <- c(-999999, rep(scaleLB, length(levels(fctr))))
    searchUB <- c(999999, rep(scaleUB, length(levels(fctr))))

    start <- c(base::mean(x, trim = .38), start_scales)

    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_location_H0 <- null_MLEs[1]
  null_scales <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = profile_location_H0, scale = null_scales[i], log = TRUE))
  }
  W1 <- sum(likelihoods)

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own instead of searching 2k dimensions at once.
  get_MLEs <- function(x, start_location) {
    neg_log_likelihood <- function(MLEs) {
      est_location <- MLEs[1]
      est_scale <- MLEs[2]
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(est_scale * pi) - sum(log(1 + ((x - est_location) / est_scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    MLEstart <- c(start_location, stats::median(base::abs(x - start_location)))

    searchLB <- c(-999999, scaleLB)
    searchUB <- c(999999, scaleUB)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    MLEs <- get_MLEs(tempX, start_locations[i])
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = MLEs[1], scale = MLEs[2], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of location parameters of cauchy distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All locations are equal. (location_1 = location_2 ... location_k).
#' \item Alternative: At least one location is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(2)
#' x <- rcauchy(n = 150, location = 1, scale = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_location_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(2)
#' x <- c(rcauchy(50, 1, 2), rcauchy(50, 2, 2), rcauchy(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_location_one_way_test(x, fctr, .95)
#' @export
cauchy_location_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_cauchy_location_one_way, cauchy_location_test, 30)

#' @keywords internal
calc_test_stat_cauchy_scale_one_way <- function(x, fctr) {
  # Locations are nuisance parameters under both hypotheses. Only the scale is
  # restricted by the null, so the groups are not required to share a location.
  # The null estimates k locations and one scale. The alternative estimates k
  # locations and k scales.

  # Starting points for every location.
  start_locations <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    start_locations[i] <- base::mean(tempX, trim = .38)
  }

  # Deviations about each group's location bound every scale in both models.
  # Sharing one set of bounds keeps the null's scale inside the alternative's
  # search region. Bounding each group by its own deviations would let the null
  # sit outside the alternative and produce a negative test statistic.
  deviations <- base::abs(x - start_locations[base::as.integer(fctr)])
  scaleLB <- base::min(deviations)
  scaleUB <- base::max(deviations)
  start_scale <- stats::median(deviations)
  rm(deviations)

  # null
  get_null_MLEs <- function(x, fctr) {
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

    # combine bounds.
    # scale first b/c of how neg_log_likelihood splits arguments
    searchLB <- c(scaleLB, rep(-999999, length(levels(fctr))))
    searchUB <- c(scaleUB, rep(999999, length(levels(fctr))))

    start <- c(start_scale, start_locations)

    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par

    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_scale_H0 <- null_MLEs[1]
  null_locations <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = null_locations[i], scale = profile_scale_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own instead of searching 2k dimensions at once.
  get_MLEs <- function(x, start_location) {
    neg_log_likelihood <- function(MLEs) {
      est_location <- MLEs[1]
      est_scale <- MLEs[2]
      n <- length(x)

      # negative log likelihood
      objective <- -1 * n * log(est_scale * pi) - sum(log(1 + ((x - est_location) / est_scale)^2))
      objective <- -1 * objective # minimize function

      return(objective)
    }

    MLEstart <- c(start_location, stats::median(base::abs(x - start_location)))

    searchLB <- c(-999999, scaleLB)
    searchUB <- c(999999, scaleUB)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = searchLB, upper = searchUB, method = "L-BFGS-B", control = list(factr = 1e4))$par
    return(MLEs)
  }

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    MLEs <- get_MLEs(tempX, start_locations[i])
    likelihoods[i] <- sum(stats::dcauchy(x = tempX, location = MLEs[1], scale = MLEs[2], log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of scale parameters of cauchy distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
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
#' cauchy_scale_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rcauchy(50, 2, 1), rcauchy(50, 2, 2), rcauchy(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' cauchy_scale_one_way_test(x, fctr, .95)
#' @export
cauchy_scale_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_cauchy_scale_one_way, cauchy_scale_test, 70)
