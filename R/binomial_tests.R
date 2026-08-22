#' @keywords internal
calc_MLE_binomial_prob <- function(arg1, arg2) {
  obs_prob <- arg1 / arg2
  return(obs_prob)
}

#' @keywords internal
calc_test_stat_binomial_prob <- function(arg1, arg2, prob, alternative) {
  obs_prob <- calc_MLE_binomial_prob(arg1, arg2)
  W <- 2 * (sum(stats::dbinom(x = arg1, size = arg2, prob = obs_prob, log = TRUE)) -
    sum(stats::dbinom(x = arg1, size = arg2, prob = prob, log = TRUE)))
  W <- pmax(W, 0)

  if (alternative != "two.sided") {
    W <- sign(obs_prob - prob) * W^.5
  }

  return(W)
}

#' Test the prob parameter of a binomial distribution.
#'
#' @inheritParams gaussian_mu_test
#' @param x Number of successes.
#' @param size Number of trials.
#' @param prob Hypothesized probability of success.
#' @inherit gaussian_mu_test return
#' @inherit gaussian_mu_test source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true. 52 successes. 100 trials
#' binomial_prob_test(52, 100, .50, "two.sided")
#'
#' # Null is false. 75 successes. 100 trials
#' binomial_prob_test(75, 100, .50, "two.sided")
#' @export
binomial_prob_test <- LRTesteR:::create_test_function_one_sample_case_two(LRTesteR:::calc_MLE_binomial_prob, LRTesteR:::calc_test_stat_binomial_prob, x, size)

#' @keywords internal
calc_test_stat_prob_one_way <- function(x, size, fctr) {
  # Null
  obs_prob <- base::sum(x) / sum(size)

  W1 <- sum(stats::dbinom(x = x, size = size, prob = obs_prob, log = TRUE))

  # alt
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempSize <- size[index]
    tempProb <- tempX / tempSize
    likelihoods[i] <- sum(stats::dbinom(x = tempX, size = tempSize, prob = tempProb, log = TRUE))
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)
  W <- pmax(W, 0)

  return(W)
}

#' Test the equality of prob parameters of binomial distributions.
#'
#' @inheritParams gaussian_mu_one_way_test
#' @param x a numeric vector indicating number of successes per group.
#' @param size a numeric vector indicating number of attempts per group.
#' @inherit gaussian_mu_one_way_test return
#' @inherit gaussian_mu_one_way_test source
#' @details
#' \itemize{
#' \item Null: All probs are equal. (prob_1 = prob_2 ... prob_k).
#' \item Alternative: At least one prob is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true.
#' set.seed(1)
#' x <- rbinom(3, 50, .5)
#' size <- rep(50, length(x))
#' fctr <- factor(1:length(x))
#' binomial_prob_one_way_test(x, size, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- rbinom(3, 50, c(.25, .50, .75))
#' size <- rep(50, length(x))
#' fctr <- factor(1:length(x))
#' binomial_prob_one_way_test(x, size, fctr, .95)
#' @export
binomial_prob_one_way_test <- LRTesteR:::create_test_function_one_way_case_two(LRTesteR:::calc_test_stat_prob_one_way, binomial_prob_test)
