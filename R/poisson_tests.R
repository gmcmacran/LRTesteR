#' @keywords internal
calc_test_stat_poisson_lambda <- function(x, lambda, alternative) {
  obs_lambda <- mean(x)

  W <- 2 * (sum(stats::dpois(x = x, lambda = obs_lambda, log = TRUE)) -
    sum(stats::dpois(x = x, lambda = lambda, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_lambda - lambda) * W^.5
  }

  return(W)
}

#' Test the lambda parameter of a poisson distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param lambda a number indicating the tested value of lambda
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rpois(100, 1)
#' poisson_lambda_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rpois(100, 2)
#' poisson_lambda_one_sample(x, 1, "greater")
#' @export
poisson_lambda_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_poisson_lambda, lambda, 15, 0)

#' @keywords internal
calc_test_stat_poisson_lambda_one_way <- function(x, fctr) {
  # Null
  obs_lambda <- base::mean(x)

  W1 <- sum(stats::dpois(x = x, lambda = obs_lambda, log = TRUE))

  # alt
  group_lambdas <- vector(mode = "numeric", length = length(levels(fctr)))
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- base::mean(tempX)
    group_lambdas[i] <- temp
    temp <- sum(stats::dpois(x = tempX, lambda = group_lambdas[i], log = TRUE))
    likelihoods[i] <- temp
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of lambda parameters of poisson distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item All lambdas are equal. (lambda_1 = lambda_2 ... lambda_k).
#' \item Alternative: At least one lambda is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rpois(150, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' poisson_lambda_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rpois(50, 1), rpois(50, 2), rpois(50, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' poisson_lambda_one_way(x, fctr, .95)
#' @export
poisson_lambda_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_poisson_lambda_one_way, poisson_lambda_one_sample, 30)
