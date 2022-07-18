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

#' Test the rate of a exponential distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param rate a number indicating the tested value of rate.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
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
