#' @keywords internal
calc_test_stat_exponentail_rate <- function(x, rate, alternative) {
  obs_rate <- 1 / mean(x)

  W <- 2 * (sum(stats::dexp(x = x, rate = obs_rate, log = TRUE)) -
    sum(stats::dexp(x = x, rate = rate, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_rate - rate) * W^.5
  }

  return(W)
}

#' Test the rate of a exponential distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param rate a number indicating the tested value of rate.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rexp(100, 1)
#' exponentail_rate_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rexp(100, 3)
#' exponentail_rate_lr_test(x, 1, "greater")
#' @export
exponentail_rate_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_exponentail_rate, rate, 0)
