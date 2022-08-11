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

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * (W^.5)
  }

  return(W)
}

#' Test p of a negative binomial distribution.
#'
#' @inheritParams binomial_p_one_sample
#' @param num_failures Number of failures.
#' @param num_success Number of successes.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true. 48 failures before 52 successes.
#' negative_binomial_p_lr_test(48, 52, .50, "two.sided")
#'
#' # Null is false. 25 failures before 75 successes.
#' negative_binomial_p_lr_test(25, 75, .50, "two.sided")
#' @export
negative_binomial_p_lr_test <- LRTesteR:::create_test_function_one_sample_case_two(LRTesteR:::calc_MLE_negative_binomial_p, LRTesteR:::calc_test_stat_negative_binomial_p, num_failures, num_success)
