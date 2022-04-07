#' Test p of a binomial distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param p a number indicating the tested value of p.
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
#' # Null is true
#' set.seed(1)
#' x <- rbinom(100, 1, .5)
#' binomial_p_lr_test(x, .50, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbinom(100, 1, .75)
#' binomial_p_lr_test(x, .50, "greater")
#' @export
binomial_p_lr_test <- function(x, p = .50, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (!any(unique(x) %in% c(0, 1))) {
    stop("Argument x should be only contains 0s and 1s.")
  }
  if (length(p) != 1) {
    stop("Argument p should have length one.")
  }
  if (!is.numeric(p)) {
    stop("Argument p should be numeric.")
  }
  if (p<0 | p > 1) {
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

  obs_X <- sum(x)
  obs_N <- length(x)
  obs_p <- mean(x)

  if (alternative == "two.sided") {
    W <- 2 * (sum(dbinom(x = obs_X, size = obs_N, p =  obs_p, log = TRUE)) -
                sum(dbinom(x = obs_X, size = obs_N,p = p, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dbinom(x = obs_X, size = obs_N, p =  obs_p, log = TRUE)) -
                sum(dbinom(x = obs_X, size = obs_N,p = p, log = TRUE)))
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
