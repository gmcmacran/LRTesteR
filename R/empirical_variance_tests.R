#' Nonparametric test for the variance of an unknown distribution.
#'
#' @inheritParams gaussian_variance_test
#' @param x a numeric vector.
#' @inherit gaussian_variance_test return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item \url{https://www.statsmodels.org/stable/emplike.html}
#' }
#' @details
#' The mean is a nuisance parameter and is profiled out of the likelihood.
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
#' empirical_variance_test(x, 1, "two.sided") # Testing variance, not standard deviation
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(25, 0, 1)
#' empirical_variance_test(x, 2, "less")
#' @export
empirical_variance_test <- function(x, sigma.squared, alternative = "two.sided", conf.level = .95) {
  if (length(x) < 3) {
    stop("Argument x should have at least three observations.")
  }
  if (!is.numeric(x)) {
    stop("Argument x should be numeric.")
  }
  if (max(x) == min(x)) {
    stop("Argument x should have at least two unique values.")
  }
  if (length(sigma.squared) != 1) {
    stop("The tested parameter should have length one.")
  }
  if (!is.numeric(sigma.squared)) {
    stop("The tested parameter should be numeric.")
  }
  if (sigma.squared <= 0) {
    stop("The tested parameter must be greater than zero.")
  }
  # No reweighting of x can push the variance above one fourth the squared
  # range. That bound needs half the weight on the min and half on the max,
  # so it is only reachable with positive weights everywhere when no
  # observation lies strictly between the two. Anything past that has no
  # solution and is rejected here rather than failing in the optimizer.
  max_sigma.squared <- (max(x) - min(x))^2 / 4
  interior <- base::any(x > min(x) & x < max(x))
  if (sigma.squared > max_sigma.squared || (sigma.squared == max_sigma.squared && interior)) {
    stop("The tested parameter must not be greater than one fourth the squared range of x.")
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

  calc_test_stat <- function(x, sigma.squared, alternative) {
    calc_obs_p <- function(x) {
      p <- rep(1 / length(x), length(x))
      return(p)
    }
    obs_p <- calc_obs_p(x)
    null_p <- calc_null_p_variance(x, sigma.squared)

    check_empirical_optimization(obs_p)
    check_empirical_optimization(null_p)

    W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
    W <- pmax(W, 0) # underflow
    if (alternative != "two.sided") {
      obs_variance <- mean((x - mean(x))^2)
      W <- sign(obs_variance - sigma.squared) * W^.5
    }
    return(W)
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level
    obs_variance <- mean((x - mean(x))^2)

    # Roots are found on the log scale so candidate variances stay positive.
    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, exp(param), "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      LB <- log(obs_variance) - 1
      UB <- log(obs_variance)

      out <- tryCatch(
        exp(stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root),
        error = function(e) NA_real_
      )
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, exp(param), "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      LB <- log(obs_variance)
      UB <- log(obs_variance) + 1

      out <- tryCatch(
        exp(stats::uniroot(helper, lower = LB, upper = UB, tol = .Machine$double.eps^.50, extendInt = "yes")$root),
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

  W <- calc_test_stat(x, sigma.squared, alternative)

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

#' Nonparametric test for the equality of variances of unknown distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @param x a numeric vector.
#' @inherit gaussian_mu_one_way_test return
#' @source \itemize{
#' \item Owen. Empirical Likelihood. Chapman & Hall/CRC.
#' \item Owen. (1991). Empirical Likelihood for Linear Models. The Annals of Statistics, 19(4).
#' \item Qin and Lawless. (1994). Empirical Likelihood and General Estimating Equations. The Annals of Statistics, 22(1).
#' \item \url{https://www.statsmodels.org/stable/emplike.html}
#' }
#' @details
#' \itemize{
#' \item Null: All variances are equal. (sigma squared 1 = sigma squared 2 ... sigma squared k).
#' \item Alternative: At least one variance is not equal.
#' }
#'
#' Because groups are independent, the -2 log likelihood ratios of the groups
#' add. The common variance under the null is profiled out by minimizing the
#' summed statistic.
#'
#' The asymptotic approximation requires moderately large groups. In
#' simulations with three normal groups, type I error was near the nominal
#' .05 for groups of 100 or more and mildly inflated for smaller groups.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(30, 0, 1)
#' fctr <- c(rep(1, 15), rep(2, 15))
#' fctr <- factor(fctr, levels = c("1", "2"))
#' empirical_variance_one_way_test(x, fctr, .95) # Testing variance, not standard deviation
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(15, 0, 1), rnorm(15, 0, 3))
#' fctr <- c(rep(1, 15), rep(2, 15))
#' fctr <- factor(fctr, levels = c("1", "2"))
#' empirical_variance_one_way_test(x, fctr, .95)
#' @export
empirical_variance_one_way_test <- function(x, fctr, conf.level = 0.95) {
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
  if (any(as.vector(by(x, fctr, length)) < 3)) {
    stop("Each group in x should have at least three observations.")
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
    ests <- as.vector(by(x, fctr, function(z) mean((z - mean(z))^2)))

    # The common variance is profiled out on the log scale. The minimizer
    # lies near the hull of the group estimates, so the search interval is
    # the range of group estimates with a margin.
    total_W <- function(log_theta) {
      Ws <- vapply(
        levels(fctr),
        function(l) calc_group_W(x[fctr == l], exp(log_theta), calc_null_p_variance),
        numeric(1)
      )
      return(sum(Ws))
    }
    LB <- log(min(ests)) - .5
    UB <- log(max(ests)) + .5
    opt <- stats::optim(
      par = log(stats::median(ests)), fn = total_W, method = "Brent",
      lower = LB, upper = UB
    )

    for (l in levels(fctr)) {
      check_empirical_optimization(calc_null_p_variance(x[fctr == l], exp(opt$par)))
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
    obs_variance <- mean((tempX - mean(tempX))^2)
    tempCI <- LRTesteR::empirical_variance_test(tempX, obs_variance, "two.sided", individual.conf.level)
    tempCI <- tempCI$conf.int
    CI[[l]] <- tempCI
  }

  out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
  class(out) <- c("one_way_case_three", "lrtest")
  return(out)
}
