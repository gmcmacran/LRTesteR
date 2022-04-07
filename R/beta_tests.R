#' Test the shape1 of a beta distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param shape1 a number indicating the tested value of the shape1 parameter.
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
#' x <- rbeta(100, shape1 = 1, shape2 = 2)
#' beta_shape1_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 3, shape2 = 2)
#' beta_shape1_lr_test(x, 1, "greater")
#' @export
beta_shape1_lr_test <- function(x, shape1 = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(shape1) != 1) {
    stop("Argument shape1 should have length one.")
  }
  if (!is.numeric(shape1)) {
    stop("Argument shape1 should be numeric.")
  }
  if (shape1 < 0) {
    stop("Argument shape1 should be positive.")
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

  est <- fitdistrplus::fitdist(data = x, distr = "beta", method = "mle")
  obs_shape1 <- unname(est$estimate["shape1"])
  obs_shape2 <- unname(est$estimate["shape2"])

  if (alternative == "two.sided") {
    W <- 2 * (sum(dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
                sum(dbeta(x = x, shape1 = shape1, shape2 = obs_shape2, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
                sum(dbeta(x = x, shape1 = shape1, shape2 = obs_shape2, log = TRUE)))
    W <- sign(obs_shape1 - shape1) * W^.5
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

#' Test the shape2 of a beta distribution using likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param shape2 a number indicating the tested value of the shape2 parameter.
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
#' x <- rbeta(100, shape1 = 1, shape2 = 1)
#' beta_shape2_lr_test(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rbeta(100, shape1 = 1, shape2 = 3)
#' beta_shape2_lr_test(x, 1, "greater")
#' @export
beta_shape2_lr_test <- function(x, shape2 = 1, alternative = "two.sided") {
  if (length(x) < 50) {
    stop("Argument x should have at least 50 data points.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(shape2) != 1) {
    stop("Argument shape2 should have length one.")
  }
  if (!is.numeric(shape2)) {
    stop("Argument shape2 should be numeric.")
  }
  if (shape2 < 0) {
    stop("Argument shape2 should be positive.")
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

  est <- fitdistrplus::fitdist(data = x, distr = "beta", method = "mle")
  obs_shape1 <- unname(est$estimate["shape1"])
  obs_shape2 <- unname(est$estimate["shape2"])

  if (alternative == "two.sided") {
    W <- 2 * (sum(dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
                sum(dbeta(x = x, shape1 = obs_shape1, shape2 = shape2, log = TRUE)))
    p.value <- pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
                sum(dbeta(x = x, shape1 = obs_shape1, shape2 = shape2, log = TRUE)))
    W <- sign(obs_shape2 - shape2) * W^.5
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
