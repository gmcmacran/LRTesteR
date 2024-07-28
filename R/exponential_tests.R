#' @keywords internal
calc_test_stat_exponential_rate <- function(x, rate, alternative) {
  obs_rate <- 1 / mean(x)

  W <- 2 * (sum(stats::dexp(x = x, rate = obs_rate, log = TRUE)) -
    sum(stats::dexp(x = x, rate = rate, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_rate - rate) * W^.5
  }

  return(W)
}

#' Test the rate parameter of a exponential distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @param rate a number indicating the tested value of rate.
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rexp(100, 1)
#' exponential_rate_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rexp(100, 3)
#' exponential_rate_one_sample(x, 1, "greater")
#' @export
exponential_rate_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_exponential_rate, rate, 15, 0)

#' @keywords internal
calc_test_stat_exponential_rate_one_way <- function(x, fctr) {
  # Null
  obs_lambda <- 1 / base::mean(x)

  W1 <- sum(stats::dexp(x = x, rate = obs_lambda, log = TRUE))

  # alt
  group_lambdas <- vector(mode = "numeric", length = length(levels(fctr)))
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- 1 / base::mean(tempX)
    group_lambdas[i] <- temp
    temp <- sum(stats::dexp(x = tempX, rate = group_lambdas[i], log = TRUE))
    likelihoods[i] <- temp
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of rate parameters of exponential distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All lambdas are equal. (lambda_1 = lambda_2 ... lambda_k).
#' \item Alternative: At least one lambda is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rexp(150, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' exponential_rate_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rexp(50, 1), rexp(50, 2), rexp(50, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' exponential_rate_one_way(x, fctr, .95)
#' @export
exponential_rate_one_way <- create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_exponential_rate_one_way, exponential_rate_one_sample, 30)
