#' @keywords internal
calc_test_stat_normal_mu <- function(x, mu, alternative) {
  obs_mean <- base::mean(x)
  obs_sd <- (sum((x - obs_mean)^2) / length(x))^.5 # Need n denominator. Not n-1.

  profile_sd <- (sum((x - mu)^2) / length(x))^.5

  W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
    sum(stats::dnorm(x = x, mean = mu, sd = profile_sd, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_mean - mu) * W^.5
  }

  return(W)
}

#' Test the mean of a gaussian distribution.
#'
#' @param x a numeric vector of at least 50 data values.
#' @param mu a number indicating the tested value of mu.
#' @param alternative a character string specifying the alternative hypothesis, must be one of "two.sided" (default), "greater" or "less".
#' @param conf.level confidence level of the likelihood interval.
#' @return An S3 class containing the test statistic, p value, likelihood based confidence interval, and alternative
#' hypothesis.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' gaussian_mu_one_sample(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 3, 1)
#' gaussian_mu_one_sample(x, 0, "greater")
#' @export
gaussian_mu_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_mu, mu)

#' @keywords internal
calc_test_stat_normal_sigma.squared <- function(x, sigma.squared, alternative) {
  obs_mean <- base::mean(x)
  obs_sd <- (sum((x - obs_mean)^2) / length(x))^.5 # Need n denominator. Not n-1.

  W <- 2 * (sum(stats::dnorm(x = x, mean = obs_mean, sd = obs_sd, log = TRUE)) -
    sum(stats::dnorm(x = x, mean = obs_mean, sd = sigma.squared^.5, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_sd^2 - sigma.squared) * W^.5
  }

  return(W)
}

#' Test the variance of a gaussian distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param sigma.squared a number indicating the tested value of sigma squared.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' gaussian_variance_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rnorm(100, 0, 2)
#' gaussian_variance_one_sample(x, 1, "greater")
#' @export
gaussian_variance_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, 0)

#' @keywords internal
calc_test_stat_normal_mu_one_way <- function(x, fctr) {
  # Null
  obs_mean <- base::mean(x)

  profile_sd <- (sum((x - obs_mean)^2) / length(x))^.5

  W1 <- sum(stats::dnorm(x = x, mean = obs_mean, sd = profile_sd, log = TRUE))

  # alt
  group_means <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- base::mean(tempX)
    group_means[i] <- temp
  }

  SS <- 0
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempSS <- sum((tempX - group_means[i])^2)
    SS <- SS + tempSS
  }
  profile_sd_ha <- (SS / length(x))^.5

  likelihoods <- vector(mode = "numeric", length = length(group_means))
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- sum(stats::dnorm(x = tempX, mean = group_means[i], sd = profile_sd_ha, log = TRUE))
    likelihoods[i] <- temp
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)

  return(W)
}

#' Test equality of means of gaussian distributions.
#'
#' @param x a numeric vector of at least 50 data values per group.
#' @param fctr a factor vector indicating groups.
#' @param conf.level overall confidence level of the likelihood intervals. Uses Bonferroni correction.
#' @return An S3 class containing the test statistic, p value, list of likelihood based confidence intervals,
#' overall confidence level, individual confidence level of each interval and alternative
#' hypothesis.
#' @details
#' Null: All mus are equal. (mu1 = mu2 ... muk).
#' Alternative: At least one mu is not equal.
#' @source \url{https://en.wikipedia.org/wiki/Likelihood-ratio_test}
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(150, 1, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gaussian_mu_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(50, 1, 1), rnorm(50, 2, 1), rnorm(50, 3, 1))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gaussian_mu_one_way(x, fctr, .95)
#' @export
gaussian_mu_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_normal_mu_one_way, gaussian_mu_one_sample)

#' @keywords internal
calc_test_stat_normal_sigma.squared_one_way <- function(x, fctr) {
  # null
  profile_mean <- base::mean(x)

  obs_sd <- (sum((x - profile_mean)^2) / length(x))^.5

  W1 <- sum(stats::dnorm(x = x, mean = profile_mean, sd = obs_sd, log = TRUE))

  # alt
  profile_mean_HA <- base::mean(x)
  group_sds <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    group_sds[i] <- (sum((tempX - profile_mean_HA)^2) / length(tempX))^.5
  }

  likelihoods <- vector(mode = "numeric", length = length(group_sds))
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- sum(stats::dnorm(x = tempX, mean = profile_mean_HA, sd = group_sds[i], log = TRUE))
    likelihoods[i] <- temp
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)

  return(W)
}

#' Test equality of variances of gaussian distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' Null: All variances are equal. (o^2_1 = o^2_2 ... o^2_k).
#' Alternative: At least one variance is not equal.
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rnorm(150, 1, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gaussian_variance_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rnorm(50, 1, 1), rnorm(50, 1, 2), rnorm(50, 1, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' gaussian_variance_one_way(x, fctr, .95)
#' @export
gaussian_variance_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared_one_way, gaussian_variance_one_sample)
