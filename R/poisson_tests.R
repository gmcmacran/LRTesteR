#' @keywords internal
calc_test_stat_poisson_lambda <- function(x, lambda, alternative) {
  obs_lambda <- mean(x)

  W <- 2 * (sum(stats::dpois(x = x, lambda = obs_lambda, log = TRUE)) -
    sum(stats::dpois(x = x, lambda = lambda, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_lambda - lambda) * W^.5
  }

  return(W)
}

#' Test lambda of a poisson distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param lambda a number indicating the tested value of lambda
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rpois(100, 1)
#' poisson_lambda_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rpois(100, 2)
#' poisson_lambda_lr_test(x, 1, "greater")
#' @export
poisson_lambda_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_poisson_lambda, lambda, 0)
