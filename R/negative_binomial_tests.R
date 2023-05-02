#' @keywords internal
calc_MLE_negative_binomial_p <- function(arg1, arg2) {
  ops_p <- arg2 / (arg2 + arg1)
  return(ops_p)
}

#' @keywords internal
calc_test_stat_negative_binomial_p <- function(arg1, arg2, p, alternative) {
  obs_p <- calc_MLE_negative_binomial_p(arg1, arg2)
  W <- 2 * (sum(stats::dnbinom(x = arg1, size = arg2, prob = obs_p, log = TRUE)) -
    sum(stats::dnbinom(x = arg1, size = arg2, prob = p, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * (W^.5)
  }

  return(W)
}

#' Test the p parameter of a negative binomial distribution.
#'
#' @param num_failures Number of failures.
#' @param num_successes Number of successes.
#' @inheritParams binomial_p_one_sample
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true. 48 failures before 52 successes.
#' negative_binomial_p_one_sample(48, 52, .50, "two.sided")
#'
#' # Null is false. 25 failures before 75 successes.
#' negative_binomial_p_one_sample(25, 75, .50, "two.sided")
#' @export
negative_binomial_p_one_sample <- LRTesteR:::create_test_function_one_sample_case_two(LRTesteR:::calc_MLE_negative_binomial_p, LRTesteR:::calc_test_stat_negative_binomial_p, num_failures, num_successes)

#' @keywords internal
calc_test_stat_negative_binomial_p_one_way <- function(num_failures, num_successes, fctr) {
  # Null
  obs_p <- sum(num_successes) / (sum(num_successes) + sum(num_failures))

  W1 <- sum(stats::dnbinom(x = num_failures, size = num_successes, prob = obs_p, log = TRUE))

  # alt
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempFailure <- num_failures[index]
    tempSuccess <- num_successes[index]
    tempP <- tempSuccess / (tempSuccess + tempFailure)
    likelihoods[i] <- sum(stats::dnbinom(x = tempFailure, size = tempSuccess, prob = tempP, log = TRUE))
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of p parameters of negative binomial distributions.
#' @param num_failures a numeric vector indicating number of failures per group.
#' @param num_successes a numeric vector indicating number of successes per group.
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true.
#' set.seed(1)
#' num_failures <- rnbinom(3, 50, .5)
#' num_successes <- rep(50, length(num_failures))
#' fctr <- factor(1:length(num_failures))
#' negative_binomial_p_one_way(num_failures, num_successes, fctr, .95)
#'
#'
#' # Null is false
#' set.seed(1)
#' num_failures <- rnbinom(3, 50, c(.25, .50, .75))
#' num_successes <- rep(50, length(num_failures))
#' fctr <- factor(1:length(num_failures))
#' negative_binomial_p_one_way(num_failures, num_successes, fctr, .95)
#' @export
negative_binomial_p_one_way <- LRTesteR:::create_test_function_one_way_case_two(LRTesteR:::calc_test_stat_negative_binomial_p_one_way, negative_binomial_p_one_sample)
