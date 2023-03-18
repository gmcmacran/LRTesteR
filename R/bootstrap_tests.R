#' Test any parameter of an unknown distribution via bias corrected accelerated bootstrapping
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x a numeric vector.
#' @param param a number indicating the tested value of the parameter.
#' @param stat.fun a function to compute observed parameter.
#' @inherit gaussian_mu_one_sample return
#' @source \itemize{
#' \item \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' \item Yudi Pawitan. In All Likelihood. Oxford University Press.
#' \item Hodd, McKean, and Craig. Introduction to Mathematical Statistics. Pearson.
#' \item Efron, Tibshirani. An Introduction to the Bootstrap. Chapman & Hall/CRC.
#' }
#' @examples
#' library(LRTesteR)
#'
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' # mean
#' calc_stat <- function(x) {
#'   return(mean(x))
#' }
#' bootstrap_one_sample(x, 0, calc_stat, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(25, 0, 2)
#' # variance
#' calc_stat <- function(x) {
#'   return(sum((x - mean(x))^2) / length(x))
#' }
#' bootstrap_one_sample(x, 1, calc_stat, "greater")
#' @export
bootstrap_one_sample <- function(x, param, stat.fun, alternative = "two.sided", conf.level = .95) {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(param) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(param)) {
    stop("The tested parameter should be numeric.")
  }
  if (!inherits(stat.fun, "function")) {
    stop("Argument stat.fun must be a function.")
  }
  args <- names(formals(stat.fun))
  if (args[1] != "x") {
    stop("stat.fun's first argument is not x.")
  }
  if (length(args) != 1) {
    stop("stat.fun has too many arguments.")
  }
  rm(args)

  if (length(alternative) != 1) {
    stop("Argument alternative should have length one.")
  }
  if (!is.character(alternative)) {
    stop("Argument alternative should be a character.")
  }
  if (!(alternative %in% c("two.sided", "less", "greater"))) {
    stop("Argument alternative should be 'two.sided', 'less', or 'greater.'")
  }
  if (length(conf.level) != 1) {
    stop("conf.level should have length one.")
  }
  if (!is.numeric(conf.level)) {
    stop("conf.level should be numeric.")
  }
  if (conf.level <= 0 | conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_jacknife_acceleration <- function(x) {
    paramJack <- vector(mode = "numeric", length = length(x))
    for (i in seq(x)) {
      xJack <- x[-i]
      paramJack[i] <- stat.fun(xJack)
    }
    numerator <- sum((mean(paramJack) - paramJack)^3)
    denominator <- 6 * (sum((mean(paramJack) - paramJack)^2))^(3 / 2)
    a <- numerator / denominator
    return(a)
  }

  convert_theta_phi <- function(theta) {
    p <- mean(paramStar < theta)
    p <- pmax(p, .Machine$double.eps)
    p <- pmin(p, 1 - .Machine$double.eps)
    phi <- stats::qnorm(p = p, mean = 0, sd = 1)
    return(phi)
  }

  calc_bias_correction <- function(x) {
    obsTheta <- stat.fun(x)
    p <- mean(paramStar < obsTheta)
    p <- pmax(p, .Machine$double.eps)
    p <- pmin(p, 1 - .Machine$double.eps)
    z0 <- stats::qnorm(p = p, mean = 0, sd = 1)
    return(z0)
  }

  B <- 20000
  generate_param_distribution <- function(B) {
    # seed setting does not affect user's RNG
    old <- .Random.seed
    on.exit({
      .Random.seed <<- old
    })
    set.seed(42)

    paramStar <- vector(mode = "numeric", length = B)
    for (i in seq(1, B, 1)) {
      xStar <- sample(x = x, size = length(x), replace = TRUE)
      paramStar[i] <- stat.fun(xStar)
    }
    return(paramStar)
  }
  paramStar <- generate_param_distribution(B)

  calc_test_stat <- function(x, param, stat.fun, alternative) {

    # Page 417 of In All Likelihood
    log_likelihood <- function(theta) {
      phi <- convert_theta_phi(theta)
      obsTheta <- stat.fun(x)
      phiHat <- convert_theta_phi(obsTheta)

      a <- calc_jacknife_acceleration(x)

      # normalization parameters
      z0 <- calc_bias_correction(x)
      sdPhi <- 1 + a * phi

      out <- -log(sdPhi) - (phiHat - phi + z0 * sdPhi)^2 / (2 * sdPhi^2)
      return(out)
    }

    obsParam <- stat.fun(x)

    W <- 2 * (log_likelihood(obsParam) - log_likelihood(param))
    W <- pmax(W, 0)

    if (alternative != "two.sided") {
      W <- sign(obsParam - param) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level
    z0 <- calc_bias_correction(x)
    a <- calc_jacknife_acceleration(x)

    ps <- vector(mode = "numeric", length = B)
    for (i in seq(1, B, 1)) {
      ps[i] <- mean(paramStar < paramStar[i])
    }

    calc_left_side_CI <- function(alpha) {
      zAlpha <- stats::qnorm(p = alpha, mean = 0, sd = 1)

      zFinal <- (z0 + zAlpha) / (1 - a * (z0 + zAlpha))
      zFinal <- z0 + zFinal
      prob <- stats::pnorm(q = zFinal)

      out <- min(paramStar[ps >= prob])
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      zAlpha <- stats::qnorm(p = 1 - alpha, mean = 0, sd = 1)

      zFinal <- (z0 + zAlpha) / (1 - a * (z0 + zAlpha))
      zFinal <- z0 + zFinal
      prob <- stats::pnorm(q = zFinal)

      out <- max(paramStar[ps <= prob])
      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
    } else if (alternative == "less") {
      CI <- c(NA_real_, calc_right_side_CI(alpha))
    } else {
      CI <- c(calc_left_side_CI(alpha), NA_real_)
    }

    return(CI)
  }

  W <- calc_test_stat(x, param, stat.fun, alternative)

  # calculate p value
  if (alternative == "two.sided") {
    p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
  } else if (alternative == "less") {
    p.value <- stats::pnorm(q = W, lower.tail = TRUE)
  } else {
    p.value <- stats::pnorm(q = W, lower.tail = FALSE)
  }

  CI <- calc_CI(x, alternative, conf.level)

  out <- list(statistic = W, p.value = p.value, conf.int = CI, conf.level = conf.level, alternative = alternative)
  class(out) <- c("one_sample_case_four", "lrtest")

  return(out)
}

#' Test the equality of any parameter of unknown distributions via bias corrected accelerated bootstrapping
#'
#' @inheritParams gaussian_mu_one_way
#' @param x a numeric vector.
#' @param stat.fun a function to compute observed statistic.
#' @param ci.fun a function to compute confidence intervel.
#' @inherit gaussian_mu_one_way return
#' @source \itemize{
#' \item \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' \item Yudi Pawitan. In All Likelihood. Oxford University Press.
#' \item Hodd, McKean, and Craig. Introduction to Mathematical Statistics. Pearson.
#' \item Efron, Tibshirani. An introduction to the Bootstrap. Chapman & Hall/CRC.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' stat.fun <- function(x, fctr) {
#'   groupMeans <- by(x, fctr, mean)
#'   stat <- max(groupMeans) - min(groupMeans)
#'
#'   return(stat)
#' }
#'
#' ci.fun <- function(x) {
#'   out <- mean(x)
#'   return(out)
#' }
#'
#' set.seed(1)
#' x <- rnorm(30, 1, 1)
#' fctr <- c(rep(1, 10), rep(2, 10), rep(3, 10))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' bootstrap_one_way(x, fctr, stat.fun, ci.fun)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(10, 1, 1), rnorm(10, 2, 1), rnorm(10))
#' fctr <- c(rep(1, 10), rep(2, 10), rep(3, 10))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' bootstrap_one_way(x, fctr, stat.fun, ci.fun)
#'
#' @export
bootstrap_one_way <- function(x, fctr, stat.fun, ci.fun, conf.level = .95) {
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (length(fctr) != length(x)) {
    stop("Argument fctr should have same length as x.")
  }
  if (!is.factor(fctr)) {
    stop("Argument fctr should be a factor.")
  }
  if (length(base::unique(fctr)) < 2) {
    stop("Argument fctr should have at least two unique values.")
  }
  if (!inherits(stat.fun, "function")) {
    stop("Argument stat.fun must be a function.")
  }
  args <- names(formals(stat.fun))
  if (args[1] != "x") {
    stop("stat.fun's first argument is not x.")
  }
  if (args[2] != "fctr") {
    stop("stat.fun's second argument is not fctr.")
  }
  if (length(args) != 2) {
    stop("stat.fun has too many arguments.")
  }
  rm(args)
  if (!inherits(ci.fun, "function")) {
    stop("Argument ci.fun must be a function.")
  }
  args <- names(formals(ci.fun))
  if (args[1] != "x") {
    stop("ci.fun's first argument is not x")
  }
  if (length(args) != 1) {
    stop("ci.fun has too many arguments.")
  }
  rm(args)
  if (length(conf.level) != 1) {
    stop("conf.level should have length one.")
  }
  if (!is.numeric(conf.level)) {
    stop("conf.level should be numeric.")
  }
  if (conf.level <= 0 | conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_jacknife_acceleration <- function(x, fctr) {
    paramJack <- vector(mode = "numeric", length = length(x))
    for (i in seq(x)) {
      xJack <- x[-i]
      fctrJack <- fctr[-i]
      paramJack[i] <- stat.fun(xJack, fctrJack)
    }
    numerator <- sum((mean(paramJack) - paramJack)^3)
    denominator <- 6 * (sum((mean(paramJack) - paramJack)^2))^(3 / 2)
    a <- numerator / denominator
    return(a)
  }

  convert_theta_phi <- function(theta) {
    p <- mean(paramStar < theta)
    p <- pmax(p, .Machine$double.eps)
    p <- pmin(p, 1 - .Machine$double.eps)
    phi <- stats::qnorm(p = p, mean = 0, sd = 1)
    return(phi)
  }

  calc_bias_correction <- function(x, fctr) {
    obsTheta <- stat.fun(x, fctr)
    p <- mean(paramStar < obsTheta)
    p <- pmax(p, .Machine$double.eps)
    p <- pmin(p, 1 - .Machine$double.eps)
    z0 <- stats::qnorm(p = p, mean = 0, sd = 1)
    return(z0)
  }

  B <- 20000
  generate_param_distribution <- function(B) {
    # seed setting does not affect user's RNG
    old <- .Random.seed
    on.exit({
      .Random.seed <<- old
    })
    set.seed(42)

    paramStar <- vector(mode = "numeric", length = B)
    for (i in seq(1, B, 1)) {
      xStar <- vector(mode = "numeric", length = length(x))
      for (j in 1:length(levels(fctr))) {
        l <- levels(fctr)[j]
        index <- which(fctr == l)
        indexStar <- sample(x = index, size = length(index), replace = TRUE)
        xStar[index] <- x[indexStar]
      }
      paramStar[i] <- stat.fun(xStar, fctr)
    }
    return(paramStar)
  }
  paramStar <- generate_param_distribution(B)

  calc_test_stat <- function(x, fctr, stat.fun) {

    # Page 417 of In All Likelihood
    log_likelihood <- function(theta) {
      phi <- convert_theta_phi(theta)
      obsTheta <- stat.fun(x, fctr)
      phiHat <- convert_theta_phi(obsTheta)

      a <- calc_jacknife_acceleration(x, fctr)

      # normalization parameters
      z0 <- calc_bias_correction(x, fctr)
      sdPhi <- 1 + a * phi

      out <- -log(sdPhi) - (phiHat - phi + z0 * sdPhi)^2 / (2 * sdPhi^2)
      return(out)
    }

    helperFctr <- rep(levels(fctr)[1], length(fctr))
    helperFctr <- factor(x = helperFctr, levels = levels(fctr)[1])
    param <- stat.fun(x, helperFctr)
    obsParam <- stat.fun(x, fctr)

    W <- 2 * (log_likelihood(obsParam) - log_likelihood(param))
    W <- pmax(W, 0)

    return(W)
  }

  W <- calc_test_stat(x, fctr, stat.fun)

  # Under null, 1 parameter (overall value) is allowed to vary
  # Under alternative, parameter for each group is allowed to vary
  df <- length(levels(fctr)) - 1

  # calculate p value
  p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

  # Bonferroni correction and convert back to confidence
  alpha <- 1 - conf.level
  alpha <- alpha / length(levels(fctr))
  individual.conf.level <- 1 - alpha

  CI <- list()
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempParam <- ci.fun(x)
    tempCI <- bootstrap_one_sample(tempX, tempParam, ci.fun, "two.sided", individual.conf.level)
    tempCI <- tempCI$conf.int
    CI[[l]] <- tempCI
  }

  out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
  class(out) <- c("one_way_case_four", "lrtest")

  return(out)
}
