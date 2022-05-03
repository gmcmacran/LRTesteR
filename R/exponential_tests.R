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
exponentail_rate_lr_test <- function(x, rate = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(rate) != 1) {
    stop("Argument rate should have length one.")
  }
  if (!is.numeric(rate)) {
    stop("Argument rate should be numeric.")
  }
  if (rate <= 0) {
    stop("Argument rate should be positive.")
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

  obs_rate <- 1 / mean(x)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dexp(x = x, rate = obs_rate, log = TRUE)) -
      sum(stats::dexp(x = x, rate = rate, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dexp(x = x, rate = obs_rate, log = TRUE)) -
      sum(stats::dexp(x = x, rate = rate, log = TRUE)))
    W <- sign(obs_rate - rate) * W^.5
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
