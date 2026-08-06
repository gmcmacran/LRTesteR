#' @keywords internal
calc_test_stat_beta_shape1 <- function(x, shape1, alternative) {
  MLEs <- unname(EnvStats::ebeta(x, method = "mle")$parameters)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]
  rm(MLEs)

  get_profile_shape2 <- function(x, MLE) {
    # negative log likelihood
    profile_helper <- function(shape2) {
      shape2 <- pmax(shape2, .0001)
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(MLE, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    profile_shape2 <- pmax(profile_shape2, .Machine$double.eps) # Per pdf definition, parameter must be positive.

    return(profile_shape2)
  }
  profile_shape2 <- get_profile_shape2(x, obs_shape2)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = shape1, shape2 = profile_shape2, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_shape1 - shape1) * W^.5
  }

  return(W)
}

#' Test the shape1 parameter of a beta distribution.
#'
#' @inheritParams gaussian_mu_test
#' @param shape1 a number indicating the tested value of the shape1 parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 2)
#' beta_shape1_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 3, shape2 = 2)
#' beta_shape1_test(x, 1, "greater")
#' @export
beta_shape1_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_beta_shape1, shape1, 40, 0)

#' @keywords internal
calc_test_stat_beta_shape2 <- function(x, shape2, alternative) {
  MLEs <- unname(EnvStats::ebeta(x, method = "mle")$parameters)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]

  get_profile_shape1 <- function(x, MLE) {
    # negative log likelihood
    profile_helper <- function(shape1) {
      shape1 <- pmax(shape1, .0001)
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape1 <- stats::optim(MLE, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    profile_shape1 <- pmax(profile_shape1, .Machine$double.eps) # Per pdf definition, parameter must be positive.

    return(profile_shape1)
  }
  profile_shape1 <- get_profile_shape1(x, obs_shape1)

  W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
    sum(stats::dbeta(x = x, shape1 = profile_shape1, shape2 = shape2, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_shape2 - shape2) * W^.5
  }

  return(W)
}

#' Test the shape2 parameter of a beta distribution.
#'
#' @inheritParams gaussian_mu_test
#' @param shape2 a number indicating the tested value of the shape2 parameter.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 1)
#' beta_shape2_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 3)
#' beta_shape2_test(x, 1, "greater")
#' @export
beta_shape2_test <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_beta_shape2, shape2, 40, 0)

#' @keywords internal
calc_test_stat_beta_shape1_one_way <- function(x, fctr) {
  # Shape2s are nuisance parameters under both hypotheses. Only shape1 is
  # restricted by the null, so the groups are not required to share a shape2.
  # The null estimates one shape1 and k shape2s. The alternative estimates k
  # shape1s and k shape2s.

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own instead of searching 2k dimensions at once.
  group_shape1s <- vector(mode = "numeric", length = length(levels(fctr)))
  group_shape2s <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    MLEs <- unname(EnvStats::ebeta(tempX, method = "mle")$parameters)
    group_shape1s[i] <- MLEs[1]
    group_shape2s[i] <- MLEs[2]
  }
  rm(MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = group_shape1s[i], shape2 = group_shape2s[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape1 <- pmax(estimates[1], .Machine$double.eps) # pooled shape1
      est_shape2s <- pmax(estimates[2:length(estimates)], .Machine$double.eps)

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = est_shape1, shape2 = est_shape2s[i], log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # starting points (group wise MLEs from the alternative)
    # shape1 first b/c of how neg_log_likelihood splits arguments
    start <- c(base::mean(group_shape1s), group_shape2s)
    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    null_MLEs <- pmax(null_MLEs, .Machine$double.eps) # Per pdf definition, parameter must be positive.
    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_shape1_H0 <- null_MLEs[1]
  null_shape2s <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = profile_shape1_H0, shape2 = null_shape2s[i], log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of shape 1 parameters of beta distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All shape1s are equal. (shape1_1 = shape1_2 ... shape1_k).
#' \item Alternative: At least one shape1 is not equal.
#' }
#' The shape2s are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(150, 1, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape1_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rbeta(50, 1, 2), rbeta(50, 2, 2), rbeta(50, 3, 2))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape1_one_way_test(x, fctr, .95)
#' @export
beta_shape1_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_beta_shape1_one_way, beta_shape1_test, 80)

#' @keywords internal
calc_test_stat_beta_shape2_one_way <- function(x, fctr) {
  # Shape1s are nuisance parameters under both hypotheses. Only shape2 is
  # restricted by the null, so the groups are not required to share a shape1.
  # The null estimates one shape2 and k shape1s. The alternative estimates k
  # shape1s and k shape2s.

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own instead of searching 2k dimensions at once.
  group_shape1s <- vector(mode = "numeric", length = length(levels(fctr)))
  group_shape2s <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    MLEs <- unname(EnvStats::ebeta(tempX, method = "mle")$parameters)
    group_shape1s[i] <- MLEs[1]
    group_shape2s[i] <- MLEs[2]
  }
  rm(MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = group_shape1s[i], shape2 = group_shape2s[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  get_null_MLEs <- function(x, fctr) {
    neg_log_likelihood <- function(estimates) {
      est_shape2 <- pmax(estimates[1], .Machine$double.eps) # pooled shape2
      est_shape1s <- pmax(estimates[2:length(estimates)], .Machine$double.eps)

      likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
      for (i in seq_along(levels(fctr))) {
        l <- levels(fctr)[i]
        index <- which(fctr == l)
        tempX <- x[index]
        likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = est_shape1s[i], shape2 = est_shape2, log = TRUE))
      }
      likelihoods <- -1 * sum(likelihoods)
      return(likelihoods)
    }

    # starting points (group wise MLEs from the alternative)
    # shape2 first b/c of how neg_log_likelihood splits arguments
    start <- c(base::mean(group_shape2s), group_shape1s)
    null_MLEs <- stats::optim(start, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B", control = list(factr = 1e4))$par
    null_MLEs <- pmax(null_MLEs, .Machine$double.eps) # Per pdf definition, parameter must be positive.
    return(null_MLEs)
  }

  null_MLEs <- get_null_MLEs(x, fctr)
  profile_shape2_H0 <- null_MLEs[1]
  null_shape1s <- null_MLEs[2:length(null_MLEs)]
  rm(null_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(stats::dbeta(x = tempX, shape1 = null_shape1s[i], shape2 = profile_shape2_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of shape 2 parameters of beta distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All shape2s are equal. (shape2_1 = shape2_2 ... shape2_k).
#' \item Alternative: At least one shape2 is not equal.
#' }
#' The shape1s are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rbeta(150, 2, 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape2_one_way_test(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rbeta(50, 2, 1), rbeta(50, 2, 2), rbeta(50, 2, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' beta_shape2_one_way_test(x, fctr, .95)
#' @export
beta_shape2_one_way_test <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_beta_shape2_one_way, beta_shape2_test, 80)
