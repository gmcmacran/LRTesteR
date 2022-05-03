#' Test lambda of a poisson distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param lambda a number indicating the tested value of lambda
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @details
#' This test requires a large sample size for approximation to be accurate.
#'
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
poisson_lambda_lr_test <- function(x, lambda = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(lambda) != 1) {
    stop("Argument lambda should have length one.")
  }
  if (!is.numeric(lambda)) {
    stop("Argument lambda should be numeric.")
  }
  if (lambda <= 0) {
    stop("Argument lambda should be positive.")
  }
  if (length(alternative) != 1) {
    stop("Argument alternative should have length one.")
  }
  if (!is.character(alternative)) {
    stop("Argument alternative should be a character.")
  }
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Argument alternative should be 'two.sided', 'less', or 'greater'")
  }

  obs_lambda <- mean(x)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dpois(x = x, lambda = obs_lambda, log = TRUE)) -
      sum(stats::dpois(x = x, lambda = lambda, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dpois(x = x, lambda = obs_lambda, log = TRUE)) -
      sum(stats::dpois(x = x, lambda = lambda, log = TRUE)))
    W <- sign(obs_lambda - lambda) * W^.5
    if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    }
    if (alternative == "greater") {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }
  }

  out <- list(statistic = W, p.value = p.value, alternative = alternative)
  class(out) <- "lrtest"
  return(out)
}
