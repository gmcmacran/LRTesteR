#' Test the shape1 parameter of a beta distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param shape1 a number indicating the tested value of the shape1 parameter.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
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
  if (shape1 <= 0) {
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

  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_shape1 <- MLEs[1]
      est_shape2 <- MLEs[2]

      objective <- -1 * sum(stats::dbeta(x = x, shape1 = est_shape1, shape2 = est_shape2, log = TRUE))

      return(objective)
    }

    # starting points (method of moments)
    xbar <- base::mean(x)
    vbar <- stats::var(x)
    C <- (xbar * (1 - xbar)) / vbar - 1
    shape1_start <- xbar * C
    shape2_start <- (1 - xbar) * C
    MLEstart <- c(shape1_start, shape2_start)
    rm(xbar, vbar, C, shape1_start, shape2_start)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B")$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]
  rm(MLEs)

  get_profile_shape2 <- function(x, shape1) {
    # negative log likelihood
    profile_helper <- function(shape2) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(shape1, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B")$par

    return(profile_shape2)
  }
  profile_shape2 <- get_profile_shape2(x, shape1)


  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
      sum(stats::dbeta(x = x, shape1 = shape1, shape2 = profile_shape2, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
      sum(stats::dbeta(x = x, shape1 = shape1, shape2 = profile_shape2, log = TRUE)))
    W <- sign(obs_shape1 - shape1) * W^.5
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

#' Test the shape2 parameter of a beta distribution using the likelihood ratio test.
#'
#' @param x a (non-empty) numeric vector of data values.
#' @param shape2 a number indicating the tested value of the shape2 parameter.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @return An S3 class containing the test statistic, p value and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
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
  if (shape2 <= 0) {
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

  get_MLEs <- function(x) {
    neg_log_likelihood <- function(MLEs) {
      est_shape1 <- MLEs[1]
      est_shape2 <- MLEs[2]

      objective <- -1 * sum(stats::dbeta(x = x, shape1 = est_shape1, shape2 = est_shape2, log = TRUE))

      return(objective)
    }

    # starting points (method of moments)
    xbar <- base::mean(x)
    vbar <- stats::var(x)
    C <- (xbar * (1 - xbar)) / vbar - 1
    shape1_start <- xbar * C
    shape2_start <- (1 - xbar) * C
    MLEstart <- c(shape1_start, shape2_start)
    rm(xbar, vbar, C, shape1_start, shape2_start)

    MLEs <- stats::optim(MLEstart, neg_log_likelihood, lower = .Machine$double.eps, method = "L-BFGS-B")$par
    return(MLEs)
  }
  MLEs <- get_MLEs(x)
  obs_shape1 <- MLEs[1]
  obs_shape2 <- MLEs[2]

  get_profile_shape2 <- function(x, shape2) {
    # negative log likelihood
    profile_helper <- function(shape1) {
      return(-1 * sum(stats::dbeta(x = x, shape1 = shape1, shape2 = shape2, log = TRUE)))
    }

    profile_shape2 <- stats::optim(shape2, profile_helper, lower = .Machine$double.eps, method = "L-BFGS-B")$par

    return(profile_shape2)
  }
  profile_shape1 <- get_profile_shape2(x, shape2)

  if (alternative == "two.sided") {
    W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
      sum(stats::dbeta(x = x, shape1 = profile_shape1, shape2 = shape2, log = TRUE)))
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  }
  else {
    W <- 2 * (sum(stats::dbeta(x = x, shape1 = obs_shape1, shape2 = obs_shape2, log = TRUE)) -
      sum(stats::dbeta(x = x, shape1 = profile_shape1, shape2 = shape2, log = TRUE)))
    W <- sign(obs_shape2 - shape2) * W^.5
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
