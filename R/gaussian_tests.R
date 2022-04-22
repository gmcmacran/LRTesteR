#' Test the mean of a gaussian distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param mu a number indicating the tested value of mu.
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
#' x <- rnorm(100, 0, 1)
#' gaussian_mu_lr_test(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 3, 1)
#' gaussian_mu_lr_test(x, 0, "greater")
#' @export
gaussian_mu_lr_test <- function(x, mu = 0, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(mu) != 1) {
    stop("Argument mu should have length one.")
  }
  if (!is.numeric(mu)) {
    stop("Argument mu should be numeric.")
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

  obs_sd <- stats::sd(x)
  obs_mean <- base::mean(x)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
      sum(stats::dnorm(x = x, mean = mu, sd = obs_sd, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
      sum(stats::dnorm(x = x, mean = mu, sd = obs_sd, log = TRUE)))
    W <- sign(obs_mean - mu) * W^.5
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

#' Test the variance of a gaussian distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param sigma.squared a number indicating the tested value of sigma squared.
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
#' x <- rnorm(100, 0, 1)
#' gaussian_variance_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 0, 2)
#' gaussian_variance_lr_test(x, 1, "greater")
#' @export
gaussian_variance_lr_test <- function(x, sigma.squared = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(sigma.squared) != 1) {
    stop("Argument sigma.squared should have length one.")
  }
  if (!is.numeric(sigma.squared)) {
    stop("Argument sigma.squared should be numeric.")
  }
  if (sigma.squared <= 0) {
    stop("Argument sigma.squared should be positive.")
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

  obs_sd <- stats::sd(x)
  obs_mean <- base::mean(x)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
      sum(stats::dnorm(x = x, mean = obs_mean, sd = sigma.squared^.5, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
      sum(stats::dnorm(x = x, mean = obs_mean, sd = sigma.squared^.5, log = TRUE)))
    W <- sign(obs_sd^2 - sigma.squared) * W^.5
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
