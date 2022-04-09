#' Test p of a binomial distribution using likelihood ratio test.
#'
#' @param x Number of successes.
#' @param n Number of trials.
#' @param p Hypothesized probability of success..
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
#' binomial_p_lr_test(52, 100, .50, "two.sided")
#'
#' # Null is false. 75 successes. 100 trials
#' binomial_p_lr_test(75, 100, .50, "two.sided")
#' @export
binomial_p_lr_test <- function(x, n, p = .50, alternative = "two.sided") {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(x) != 1) {
    stop("Argument x should have length 1.")
  }
  if (x < 0) {
    stop("Argument x should be 0 or positive.")
  }
  if (x > n) {
    stop("Argument x should be less than or equal to n.")
  }
  if (!is.numeric(n)) {
    stop("Argument n should be numeric.")
  }
  if (length(n) != 1) {
    stop("Argument n should have length 1.")
  }
  if (n < 0) {
    stop("Argument n should be positive.")
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

  obs_p <- x / n

  if (alternative == "two.sided") {
    W <- 2 * (sum(dbinom(x = x, size = n, p = obs_p, log = TRUE)) -
      sum(dbinom(x = x, size = n, p = p, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dbinom(x = x, size = n, p = obs_p, log = TRUE)) -
      sum(dbinom(x = x, size = n, p = p, log = TRUE)))
    W <- sign(obs_p - p) * W^.5
    if (alternative == "less") {
      p.value <- pnorm(q = W, lower.tail = TRUE)
    }
    if (alternative == "greater") {
      p.value <- pnorm(q = W, lower.tail = FALSE)
    }
  }

  out <- list(statistic = W, p.value = p.value, alternative = alternative)
  class(out) <- "mltest"
  return(out)
}
