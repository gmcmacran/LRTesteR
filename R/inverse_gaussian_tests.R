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
#' @inherit gaussian_mu_one_way details
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
    xbar <- mean(x)

    shapes <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in seq_along(levels(fctr))) {
      tempX <- x[which(fctr == levels(fctr)[i])]
      C <- sum((tempX - xbar)^2 / tempX)
      shapes[i] <- length(tempX) * (xbar^2) / C
    }

    group_MLEs <- c(xbar, shapes)
    group_MLEs <- pmax(group_MLEs, .Machine$double.eps)
    return(group_MLEs)
  }
  group_MLEs <- get_group_MLEs(x, fctr)
  profile_mean_HA <- group_MLEs[1]
  group_shapes <- group_MLEs[2:length(group_MLEs)]
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = profile_mean_HA, shape = group_shapes[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

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
#' set.seed(1)
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
  obs_dispersion <- 1 / obs_shape
  rm(MLEs)

  W1 <- sum(statmod::dinvgauss(x = x, mean = obs_mean, dispersion = obs_dispersion, log = TRUE))

  # alt
  get_group_MLEs <- function(x, fctr) {
    xbar <- mean(x)

    shapes <- vector(mode = "numeric", length = length(levels(fctr)))
    for (i in seq_along(levels(fctr))) {
      tempX <- x[which(fctr == levels(fctr)[i])]
      C <- sum((tempX - xbar)^2 / tempX)
      shapes[i] <- length(tempX) * (xbar^2) / C
    }

    group_MLEs <- c(xbar, shapes)
    group_MLEs <- pmax(group_MLEs, .Machine$double.eps)
    return(group_MLEs)
  }
  group_MLEs <- get_group_MLEs(x, fctr)
  profile_mean_HA <- group_MLEs[1]
  group_shapes <- group_MLEs[2:length(group_MLEs)]
  group_dispersions <- 1 / group_shapes
  rm(group_MLEs)

  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    likelihoods[i] <- sum(statmod::dinvgauss(x = tempX, mean = profile_mean_HA, dispersion = group_dispersions[i], log = TRUE))
  }
  W2 <- sum(likelihoods)

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
