#' Test p of a geometric distribution using likelihood ratio test.
#'
#' @param num_failures Number of failures.
#' @param p Hypothesized probability of success.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @details
#' Simulation shows this test needs the hypothesized p at or above .35 for a
#' reasonable type I error.
#'
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(MLTesteR)
#'
#' # Null is true. Two failures before the 1st success.
#' geometric_p_lr_test(2, .50, "two.sided")
#'
#' # Null is false. 15 failures before the 1st success.
#' geometric_p_lr_test(15, .50, "two.sided")
#' @export
geometric_p_lr_test <- function(num_failures, p = .50, alternative = "two.sided") {
  if (!is.numeric(num_failures)) {
    stop("Argument num_failures should be numeric.")
  }
  if (length(num_failures) != 1) {
    stop("Argument num_failures should have length 1.")
  }
  if (num_failures < 0) {
    stop("Argument num_failures should be 0 or positive.")
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

  # Geometric distribution is a negative binomial with 1 success
  obs_p <- 1 / (1 + num_failures)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dgeom(x = num_failures, prob = obs_p, log = TRUE)) -
      sum(stats::dgeom(x = num_failures, prob = p, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dgeom(x = num_failures, prob = obs_p, log = TRUE)) -
      sum(stats::dgeom(x = num_failures, prob = p, log = TRUE)))
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
