#' Test p of a negative binomial distribution's p using likelihood ratio test.
#'
#' @param num_failures Number of failures.
#' @param num_success Number of successes.
#' @param p Hypothesized probability of success.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @details
#' This test requires a large sample size for approximation to be accurate.
#'
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(MLTesteR)
#'
#' # Null is true. 52 successes. 100 trials
#' negative_binomial_p_lr_test(48, 52, .50, "two.sided")
#'
#' # Null is false. 75 successes. 100 trials
#' negative_binomial_p_lr_test(25, 75, .50, "two.sided")
#' @export
negative_binomial_p_lr_test <- function(num_failures, num_success, p = .50, alternative = "two.sided") {
  if (!is.numeric(num_failures)) {
    stop("Argument num_failures should be numeric.")
  }
  if (length(num_failures) != 1) {
    stop("Argument num_failures should have length 1.")
  }
  if (num_failures < 0) {
    stop("Argument num_failures should be 0 or positive.")
  }
  if (!is.numeric(num_success)) {
    stop("Argument num_success should be numeric.")
  }
  if (length(num_success) != 1) {
    stop("Argument num_success should have length 1.")
  }
  if (num_success < 0) {
    stop("Argument num_success should be 0 or positive.")
  }
  if (!is.numeric(p)) {
    stop("Argument p should be numeric.")
  }
  if (length(p) != 1) {
    stop("Argument p should have length one.")
  }
  if (p < 0 | p > 1) {
    stop("Argument p should be between 0 and 1.")
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

  obs_p <- num_success / (num_success + num_failures)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dnbinom(x = num_failures, size = num_success, prob = obs_p, log = TRUE)) -
      sum(stats::dnbinom(x = num_failures, size = num_success, prob = p, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dnbinom(x = num_failures, size = num_success, prob = obs_p, log = TRUE)) -
      sum(stats::dnbinom(x = num_failures, size = num_success, prob = p, log = TRUE)))
    W <- sign(obs_p - p) * (W^.5)
    if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    }
    if (alternative == "greater") {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }
  }

  out <- list(statistic = W, p.value = p.value, alternative = alternative)
  class(out) <- "mltest"
  return(out)
}
