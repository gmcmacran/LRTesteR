#' Test the skewness of an unknown distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param x a numeric vector.
#' @param skewness a number indicating the tested value of skewness.
#' @inherit gaussian_mu_one_sample return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item \url{https://github.com/statsmodels/statsmodels/blob/main/statsmodels/emplike/descriptive.py}
#' }
#' @details
#' The mean and variance are nuisance parameters and are profiled out of the
#' likelihood.
#'
#' For confidence intervals, an endpoint may not be computable. In this case,
#' NA is returned. Reducing confidence or collecting more data
#' will make the CI computable.
#'
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' empirical_skewness_one_sample(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rexp(25, 1)
#' empirical_skewness_one_sample(x, 0, "greater")
#' @export
empirical_skewness_one_sample <- function(x, skewness, alternative = "two.sided", conf.level = .95) {
  if (length(x) < 4) {
    stop("Argument x should have at least four observations.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (max(x) == min(x)) {
    stop("Argument x should have at least two unique values.")
  }
  if (length(skewness) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(skewness)) {
    stop("The tested parameter should be numeric.")
  }
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
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_test_stat <- function(x, skewness, alternative) {
    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p_skewness(x, skewness)

    check_empirical_optimization(obs_p)
    check_empirical_optimization(null_p)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0) # underflow
    if (alternative != "two.sided") {
      obs_skewness <- mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5
      W <- sign(obs_skewness - skewness) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level
    obs_skewness <- mean((x - mean(x))^3) / mean((x - mean(x))^2)^1.5

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- obs_skewness - 1
      UB <- obs_skewness

      out <- tryCatch(
        stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root,
        error = function(e) NA_real_
      )
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- obs_skewness
      UB <- obs_skewness + 1

      out <- tryCatch(
        stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root,
        error = function(e) NA_real_
      )
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

  W <- calc_test_stat(x, skewness, alternative)

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
  class(out) <- c("one_sample_case_three", "lrtest")

  return(out)
}

#' Test the equality of skewness of unknown distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @param x a numeric vector.
#' @inherit gaussian_mu_one_way return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item Owen. (1991). Empirical Likelihood for Linear Models. The Annals of Statistics, 19(4).
#' \item Qin and Lawless. (1994). Empirical Likelihood and General Estimating Equations. The Annals of Statistics, 22(1).
#' \item \url{https://github.com/statsmodels/statsmodels/blob/main/statsmodels/emplike/descriptive.py}
#' }
#' @details
#' \itemize{
#' \item Null: All skewness are equal. (skewness 1 = skewness 2 ... skewness k).
#' \item Alternative: At least one skewness is not equal.
#' }
#'
#' Because groups are independent, the -2 log likelihood ratios of the groups
#' add. The common skewness under the null is profiled out by minimizing the
#' summed statistic.
#'
#' The asymptotic approximation converges slowly for third moments. In
#' simulations with three normal groups of 50 to 250, type I error was
#' roughly .05 to .13 at the nominal .05. Large groups are recommended.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(2)
#' x <- rnorm(75, 0, 1)
#' fctr <- c(rep(1, 25), rep(2, 25), rep(3, 25))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' empirical_skewness_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(25, 0, 1), rnorm(25, 0, 1), rnorm(25, 1, 1))
#' fctr <- c(rep(1, 25), rep(2, 25), rep(3, 25))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' empirical_skewness_one_way(x, fctr, .95)
#' @export
empirical_skewness_one_way <- function(x, fctr, conf.level = 0.95) {
  if (length(x) < 1) {
    stop("Argument x should have positive length.")
  }
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
  if (any(as.vector(by(x, fctr, length)) < 4)) {
    stop("Each group in x should have at least four observations.")
  }
  if (any(as.vector(by(x, fctr, function(z) max(z) == min(z))))) {
    stop("Each group in x should have at least two unique values.")
  }
  if (length(conf.level) != 1) {
    stop("conf.level should have length one.")
  }
  if (!is.numeric(conf.level)) {
    stop("conf.level should be numeric.")
  }
  if (conf.level <= 0 || conf.level >= 1) {
    stop("conf.level should between zero and one.")
  }

  calc_test_stat <- function(x, fctr) {
    ests <- as.vector(by(x, fctr, function(z) mean((z - mean(z))^3) / mean((z - mean(z))^2)^1.5))

    # The common skewness is profiled out. The minimizer lies near the hull
    # of the group estimates, so the search interval is the range of group
    # estimates with a margin.
    total_W <- function(theta) {
      Ws <- vapply(
        levels(fctr),
        function(l) calc_group_W(x[fctr == l], theta, calc_null_p_skewness),
        numeric(1)
      )
      return(sum(Ws))
    }
    LB <- min(ests) - .5
    UB <- max(ests) + .5
    opt <- stats::optim(
      par = stats::median(ests), fn = total_W, method = "Brent",
      lower = LB, upper = UB
    )

    for (l in levels(fctr)) {
      check_empirical_optimization(calc_null_p_skewness(x[fctr == l], opt$par))
    }

    W <- pmax(opt$value, 0)
    return(W)
  }

  W <- calc_test_stat(x, fctr)

  # Under null, 1 parameter (overall value) is allowed to vary
  # Under alternative, parameter for each group is allowed to vary
  df <- length(levels(fctr)) - 1

  p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

  # Bonferroni correction and convert back to confidence
  alpha <- 1 - conf.level
  alpha <- alpha / length(levels(fctr))
  individual.conf.level <- 1 - alpha

  CI <- list()
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    obs_skewness <- mean((tempX - mean(tempX))^3) / mean((tempX - mean(tempX))^2)^1.5
    tempCI <- LRTesteR::empirical_skewness_one_sample(tempX, obs_skewness, "two.sided", individual.conf.level)
    tempCI <- tempCI$conf.int
    CI[[l]] <- tempCI
  }

  out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
  class(out) <- c("one_way_case_three", "lrtest")
  return(out)
}
