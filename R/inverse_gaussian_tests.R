#' @keywords internal
calc_test_stat_inv_gauss_mu <- function(x, mu, alternative) {
  get_MLEs <- function(x) {
    xbar <- mean(x)
    xbar <- pmax(xbar, .Machine$double.eps)

    harmonic <- 1 / mean(1 / x)
    shape <- (1 / harmonic) - (1 / xbar)
    shape <- 1 / shape
    shape <- pmax(shape, .Machine$double.eps)

    MLEs <- c(xbar, shape)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_mean <- MLEs[1]
  obs_shape <- MLEs[2]

  # Profile shape
  get_profile_shape <- function(x, mu) {
    C <- sum((x - mu)^2 / x)
    C <- (1 / mu^2) * C
    profile_shape <- length(x) / C
    profile_shape <- pmax(profile_shape, .Machine$double.eps)

    return(profile_shape)
  }
  profile_shape <- get_profile_shape(x, mu)

  W <- 2 * (sum(statmod::dinvgauss(x = x, mean = obs_mean, shape = obs_shape, log = TRUE)) -
    sum(statmod::dinvgauss(x = x, mean = mu, shape = profile_shape, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_mean - mu) * W^.5
  }

  return(W)
}

#' Test the mean of an inverse gaussian distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 1, shape = 2)
#' inverse_gaussian_mu_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 3, shape = 2)
#' inverse_gaussian_mu_one_sample(x, 1, "greater")
#' @export
inverse_gaussian_mu_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_inv_gauss_mu, mu, 35, 0)

#' @keywords internal
calc_test_inv_gauss_shape <- function(x, shape, alternative) {
  get_MLEs <- function(x) {
    xbar <- mean(x)
    xbar <- pmax(xbar, .Machine$double.eps)

    harmonic <- 1 / mean(1 / x)
    shape <- (1 / harmonic) - (1 / xbar)
    shape <- 1 / shape
    shape <- pmax(shape, .Machine$double.eps)

    MLEs <- c(xbar, shape)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_mean <- MLEs[1]
  obs_shape <- MLEs[2]

  profile_mean <- pmax(mean(x), .Machine$double.eps)

  W <- 2 * (sum(statmod::dinvgauss(x = x, mean = obs_mean, shape = obs_shape, log = TRUE)) -
    sum(statmod::dinvgauss(x = x, mean = profile_mean, shape = shape, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_shape - shape) * W^.5
  }

  return(W)
}

#' Test the shape parameter of an inverse gaussian distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param shape a number indicating the tested value of the shape parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 1, shape = 2)
#' inverse_gaussian_shape_one_sample(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 1, shape = 2)
#' inverse_gaussian_shape_one_sample(x, 1, "greater")
#' @export
inverse_gaussian_shape_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_inv_gauss_shape, shape, 35, 0)

#' @keywords internal
calc_test_inv_gauss_dispersion <- function(x, dispersion, alternative) {
  get_MLEs <- function(x) {
    xbar <- mean(x)
    xbar <- pmax(xbar, .Machine$double.eps)

    harmonic <- 1 / mean(1 / x)
    shape <- (1 / harmonic) - (1 / xbar)
    shape <- 1 / shape
    shape <- pmax(shape, .Machine$double.eps)

    MLEs <- c(xbar, shape)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_mean <- MLEs[1]
  obs_shape <- MLEs[2]
  obs_dispersion <- 1 / obs_shape

  profile_mean <- pmax(mean(x), .Machine$double.eps)

  W <- 2 * (sum(statmod::dinvgauss(x = x, mean = obs_mean, dispersion = obs_dispersion, log = TRUE)) -
    sum(statmod::dinvgauss(x = x, mean = profile_mean, dispersion = dispersion, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_dispersion - dispersion) * W^.5
  }

  return(W)
}

#' Test the dispersion parameter of an inverse gaussian distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param dispersion a number indicating the tested value of the dispersion parameter.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 1, dispersion = 2)
#' inverse_gaussian_dispersion_one_sample(x, 2, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rinvgauss(n = 100, mean = 1, dispersion = 2)
#' inverse_gaussian_dispersion_one_sample(x, 1, "greater")
#' @export
inverse_gaussian_dispersion_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_inv_gauss_dispersion, dispersion, 35, 0)

#' @keywords internal
calc_test_stat_inv_gauss_mu_one_way <- function(x, fctr) {
  # null
  get_MLEs <- function(x) {
    xbar <- mean(x)
    xbar <- pmax(xbar, .Machine$double.eps)

    harmonic <- 1 / mean(1 / x)
    shape <- (1 / harmonic) - (1 / xbar)
    shape <- 1 / shape
    shape <- pmax(shape, .Machine$double.eps)

    MLEs <- c(xbar, shape)

    return(MLEs)
  }

  MLEs <- get_MLEs(x)
  obs_mean <- MLEs[1]
  obs_shape <- MLEs[2]
  rm(MLEs)

  W1 <- sum(statmod::dinvgauss(x = x, mean = obs_mean, shape = obs_shape, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    deno <- 0
    means <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in seq_along(levels(fctr))) {
      tempX <- x[which(fctr == levels(fctr)[i])]
      harmonic <- length(tempX) / sum(1 / tempX)
      means[i] <- mean(tempX)
      C <- length(tempX) * (1 / harmonic - 1 / means[i])
      deno <- deno + C
    }
    profile_shape <- length(x) / deno

    group_MLEs <- c(profile_shape, means)
    group_MLEs <- pmax(group_MLEs, .Machine$double.eps)
    return(group_MLEs)
  }
  group_MLEs <- get_group_MLEs(x, fctr)
  profile_shape_HA <- group_MLEs[1]
  group_means <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = group_means[i], shape = profile_shape_HA, log = TRUE))
  }
  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of means of inverse gaussian distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All mus are equal. (mu1 = mu2 ... muk).
#' \item Alternative: At least one mu is not equal.
#' }
#' The shape parameter is assumed to be equal across all groups.
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 150, mean = 1, shape = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_mu_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(
#'   rinvgauss(n = 50, mean = 1, shape = 2),
#'   rinvgauss(n = 50, mean = 2, shape = 2),
#'   rinvgauss(n = 50, mean = 3, shape = 2)
#' )
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_mu_one_way(x, fctr, .95)
#' @export
inverse_gaussian_mu_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_inv_gauss_mu_one_way, inverse_gaussian_mu_one_sample, 70)

#' @keywords internal
calc_test_stat_inv_gauss_shape_one_way <- function(x, fctr) {
  # Means are nuisance parameters under both hypotheses. Only the shape is
  # restricted by the null, so the groups are not required to share a mean.
  # The null estimates k means and one shape. The alternative estimates k means
  # and k shapes. The difference is k - 1 parameters, matching the degrees of
  # freedom used to convert W into a p value.

  # Per group sufficient statistics. Every estimate below is a function of
  # these three quantities.
  group_n <- vector(mode = "numeric", length = length(levels(fctr)))
  group_sum <- vector(mode = "numeric", length = length(levels(fctr)))
  group_sum_recip <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    group_n[i] <- length(tempX)
    group_sum[i] <- sum(tempX)
    group_sum_recip[i] <- sum(1 / tempX)
  }

  # A group's mean is free under both hypotheses and maximizes at the group's
  # sample mean no matter what the shape is, so it is the same in both models.
  group_means <- pmax(group_sum / group_n, .Machine$double.eps)
  scaled_SS <- pmax(group_sum_recip - group_n / group_means, .Machine$double.eps)

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own. Both MLEs are closed form.
  group_shapes <- pmax(group_n / scaled_SS, .Machine$double.eps)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = group_means[i], shape = group_shapes[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  # The common shape pools the same sums of squares.
  profile_shape_H0 <- pmax(length(x) / sum(scaled_SS), .Machine$double.eps)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = group_means[i], shape = profile_shape_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of shape parameters of inverse gaussian distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: Null: All shapes are equal. (shape_1 = shape_2 ... shape_k).
#' \item Alternative: At least one shape is not equal.
#' }
#' The means are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 150, mean = 1, shape = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_shape_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(2)
#' x <- c(
#'   rinvgauss(n = 50, mean = 1, shape = 1),
#'   rinvgauss(n = 50, mean = 1, shape = 3),
#'   rinvgauss(n = 50, mean = 1, shape = 4)
#' )
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_shape_one_way(x, fctr, .95)
#' @export
inverse_gaussian_shape_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_inv_gauss_shape_one_way, inverse_gaussian_shape_one_sample, 70)

#' @keywords internal
calc_test_stat_inv_gauss_dispersion_one_way <- function(x, fctr) {
  # Means are nuisance parameters under both hypotheses. Only the dispersion is
  # restricted by the null, so the groups are not required to share a mean.
  # The null estimates k means and one dispersion. The alternative estimates k
  # means and k dispersions. The difference is k - 1 parameters, matching the
  # degrees of freedom used to convert W into a p value.

  # Per group sufficient statistics. Every estimate below is a function of
  # these three quantities.
  group_n <- vector(mode = "numeric", length = length(levels(fctr)))
  group_sum <- vector(mode = "numeric", length = length(levels(fctr)))
  group_sum_recip <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    group_n[i] <- length(tempX)
    group_sum[i] <- sum(tempX)
    group_sum_recip[i] <- sum(1 / tempX)
  }

  # A group's mean is free under both hypotheses and maximizes at the group's
  # sample mean no matter what the dispersion is, so it is the same in both
  # models.
  group_means <- pmax(group_sum / group_n, .Machine$double.eps)
  scaled_SS <- pmax(group_sum_recip - group_n / group_means, .Machine$double.eps)

  # alt
  # No parameter is shared, so the likelihood separates and each group is fit
  # on its own. Both MLEs are closed form.
  group_dispersions <- pmax(scaled_SS / group_n, .Machine$double.eps)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = group_means[i], dispersion = group_dispersions[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

  # null
  # The common dispersion pools the same sums of squares.
  profile_dispersion_H0 <- pmax(sum(scaled_SS) / length(x), .Machine$double.eps)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = group_means[i], dispersion = profile_dispersion_H0, log = TRUE))
  }
  W1 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of dispersion parameters of inverse gaussian distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: Null: All dispersion parameters are equal. (dispersion_1 = dispersion_2 ... dispersion_k).
#' \item Alternative: At least one dispersion is not equal.
#' }
#' The means are treated as nuisance parameters and are estimated separately for
#' each group.
#' @examples
#' library(LRTesteR)
#' library(statmod)
#'
#' # Null is true
#' set.seed(1)
#' x <- rinvgauss(n = 150, mean = 1, dispersion = 2)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_dispersion_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(
#'   rinvgauss(n = 50, mean = 1, dispersion = 1),
#'   rinvgauss(n = 50, mean = 1, dispersion = 3),
#'   rinvgauss(n = 50, mean = 1, dispersion = 4)
#' )
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' inverse_gaussian_dispersion_one_way(x, fctr, .95)
#' @export
inverse_gaussian_dispersion_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_inv_gauss_dispersion_one_way, inverse_gaussian_dispersion_one_sample, 70)
