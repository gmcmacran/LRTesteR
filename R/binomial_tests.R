#' @keywords internal
calc_MLE_binomial_p <- function(arg1, arg2) {
  ops_p <- arg1 / arg2
  return(ops_p)
}

#' @keywords internal
calc_test_stat_binomial_p <- function(arg1, arg2, p, alternative) {
  obs_p <- calc_MLE_binomial_p(arg1, arg2)
  W <- 2 * (sum(stats::dbinom(x = arg1, size = arg2, p = obs_p, log = TRUE)) -
    sum(stats::dbinom(x = arg1, size = arg2, p = p, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * W^.5
  }

  return(W)
}

#' Test the p parameter of a binomial distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x Number of successes.
#' @param n Number of trials.
#' @param p Hypothesized probability of success.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true. 52 successes. 100 trials
#' binomial_p_one_sample(52, 100, .50, "two.sided")
#'
#' # Null is false. 75 successes. 100 trials
#' binomial_p_one_sample(75, 100, .50, "two.sided")
#' @export
binomial_p_one_sample <- LRTesteR:::create_test_function_one_sample_case_two(LRTesteR:::calc_MLE_binomial_p, LRTesteR:::calc_test_stat_binomial_p, x, n)

#' @keywords internal
calc_test_stat_p_one_way <- function(x, n, fctr) {
  # Null
  obs_p <- base::sum(x) / sum(n)

  W1 <- sum(stats::dbinom(x = x, size = n, prob = obs_p, log = TRUE))

  # alt
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempN <- n[index]
    tempP <- tempX / tempN
    likelihoods[i] <- sum(stats::dbinom(x = tempX, size = tempN, prob = tempP, log = TRUE))
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of p parameters of binomial distributions.
#' @inheritParams gaussian_mu_one_way
#' @param x a numeric vector indicating number of successes per group.
#' @param n a numeric vector indicating number of attempts per group.
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true.
#' set.seed(1)
#' x <- rbinom(3, 50, .5)
#' n <- rep(50, length(x))
#' fctr <- factor(1:length(x))
#' binomial_p_one_way(x, n, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- rbinom(3, 50, c(.25, .50, .75))
#' n <- rep(50, length(x))
#' fctr <- factor(1:length(x))
#' binomial_p_one_way(x, n, fctr, .95)
#' @export
binomial_p_one_way <- LRTesteR:::create_test_function_one_way_case_two(LRTesteR:::calc_test_stat_p_one_way, binomial_p_one_sample)
