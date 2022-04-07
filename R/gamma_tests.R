#' Test the mean of a gaussian distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param shape a number indicating the tested value of the shape parameter.
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
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_shape_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 3, scale = 2)
#' gamma_shape_lr_test(x, 1, "greater")
#' @export
gamma_shape_lr_test <- function(x, shape = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(shape) != 1) {
    stop("Argument shape should have length one.")
  }
  if (!is.numeric(shape)) {
    stop("Argument shape should be numeric.")
  }
  if (shape < 0) {
    stop("Argument shape should be positive.")
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

  est <- fitdistrplus::fitdist(data = x, distr = "gamma", method = "mle")
  obs_shape <- unname(est$estimate["shape"])
  obs_rate <- unname(est$estimate["rate"])
  obs_scale <- 1 / obs_rate

  if (alternative == "two.sided") {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
      sum(dgamma(x = x, shape = shape, rate = obs_rate, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
                sum(dgamma(x = x, shape = shape, rate = obs_rate, log = TRUE)))
    W <- sign(obs_shape - shape) * W^.5
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

#' Test the variance of a gaussian distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param scale a number indicating the tested value of the scale parameter.
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
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_lr_test(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, scale = 2)
#' gamma_scale_lr_test(x, 1, "greater")
#' @export
gamma_scale_lr_test <- function(x, scale = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(scale) != 1) {
    stop("Argument scale should have length one.")
  }
  if (!is.numeric(scale)) {
    stop("Argument scale should be numeric.")
  }
  if (scale < 0) {
    stop("Argument scale should be positive.")
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

  est <- fitdistrplus::fitdist(data = x, distr = "gamma", method = "mle")
  obs_shape <- unname(est$estimate["shape"])
  obs_rate <- unname(est$estimate["rate"])
  obs_scale <- 1 / obs_rate

  if (alternative == "two.sided") {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, scale = obs_scale, log = TRUE)) -
      sum(dgamma(x = x, shape = obs_shape, scale = scale, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, scale = obs_scale, log = TRUE)) -
                sum(dgamma(x = x, shape = obs_shape, scale = scale, log = TRUE)))
    W <- sign(obs_scale - scale) * W^.5
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

#' Test the variance of a gaussian distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param rate a number indicating the tested value of the rate parameter.
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
#' x <- rgamma(100, shape = 1, rate = 1)
#' gamma_rate_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rgamma(100, shape = 1, rate = 2)
#' gamma_rate_lr_test(x, 1, "greater")
#' @export
gamma_rate_lr_test <- function(x, rate = 1, alternative = "two.sided") {
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
  if (rate < 0) {
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

  est <- fitdistrplus::fitdist(data = x, distr = "gamma", method = "mle")
  obs_shape <- unname(est$estimate["shape"])
  obs_rate <- unname(est$estimate["rate"])
  obs_scale <- 1 / obs_rate

  if (alternative == "two.sided") {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
      sum(dgamma(x = x, shape = obs_shape, rate = rate, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dgamma(x = x, shape = obs_shape, rate = obs_rate, log = TRUE)) -
                sum(dgamma(x = x, shape = obs_shape, rate = rate, log = TRUE)))
    W <- sign(obs_rate - rate) * W^.5
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
