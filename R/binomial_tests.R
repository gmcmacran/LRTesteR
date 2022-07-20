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

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * W^.5
  }

  return(W)
}

#' Test p of a binomial distribution using the likelihood ratio test.
#'
#' @inheritParams gaussian_mu_lr_test
#' @param x Number of successes.
#' @param n Number of trials.
#' @param p Hypothesized probability of success.
#' @inherit gaussian_mu_lr_test return
#' @inherit gaussian_mu_lr_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true. 52 successes. 100 trials
#' binomial_p_lr_test(52, 100, .50, "two.sided")
#'
#' # Null is false. 75 successes. 100 trials
#' binomial_p_lr_test(75, 100, .50, "two.sided")
#' @export
binomial_p_lr_test <- LRTesteR:::create_test_function_discrete(LRTesteR:::calc_MLE_binomial_p, LRTesteR:::calc_test_stat_binomial_p, x, n)
