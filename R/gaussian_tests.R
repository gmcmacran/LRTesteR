#' @keywords internal
calc_test_stat_normal_mu <- function(x, mu, alternative) {
  obs_mean <- base::mean(x)
  obs_sd <- (sum((x - obs_mean)^2) / length(x))^.5 # Need n denominator. Not n-1.

  profile_sd <- (sum((x - mu)^2) / length(x))^.5

  W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
    sum(stats::dnorm(x = x, mean = mu, sd = profile_sd, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_mean - mu) * W^.5
  }

  return(W)
}

#' Test the mean of a gaussian distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param mu a number indicating the tested value of mu.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' gaussian_mu_lr_test(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 3, 1)
#' gaussian_mu_lr_test(x, 0, "greater")
#' @export
gaussian_mu_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_mu, mu)

#' @keywords internal
calc_test_stat_normal_sigma.squared <- function(x, sigma.squared, alternative) {
  obs_mean <- base::mean(x)
  obs_sd <- (sum((x - obs_mean)^2) / length(x))^.5 # Need n denominator. Not n-1.

  W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
    sum(stats::dnorm(x = x, mean = obs_mean, sd = sigma.squared^.5, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_sd^2 - sigma.squared) * W^.5
  }

  return(W)
}

#' Test the variance of a gaussian distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param sigma.squared a number indicating the tested value of sigma squared.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' gaussian_variance_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 0, 2)
#' gaussian_variance_lr_test(x, 1, "greater")
#' @export
gaussian_variance_lr_test <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, 0)
