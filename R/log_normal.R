#' @keywords internal
calc_test_stat_log_normal_mu <- function(x, mu, alternative) {
  if (base::any(x <= 0)) {
    stop("All values in x must be positive.")
  }
  x <- base::log(x = x)
  W <- calc_test_stat_normal_mu(x = x, mu = mu, alternative = alternative)
  return(W)
}

#' Test the mean of a log normal distribution.
#'
#' @inheritParams gaussian_mu_one_sample
#' @inherit gaussian_mu_one_sample return
#' @inherit gaussian_mu_one_sample source
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rlnorm(100, 0, 1)
#' log_normal_mu_one_sample(x, 0, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rlnorm(100, 3, 1)
#' log_normal_mu_one_sample(x, 0, "greater")
#' @export
log_normal_mu_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_log_normal_mu, mu, 15)

#' @keywords internal
calc_test_stat_log_normal_sigma.squared <- function(x, sigma.squared, alternative) {
  if (base::any(x <= 0)) {
    stop("All values in x must be positive.")
  }
  x <- base::log(x = x)
  W <- calc_test_stat_normal_sigma.squared(x = x, sigma.squared = sigma.squared, alternative = alternative)
  return(W)
}

#' Test the variance of a log normal distribution.
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
#' x <- rlnorm(100, 0, 1)
#' log_normal_variance_one_sample(x, 1, "two.sided")
#'
#' # Null is false
#' set.seed(1)
#' x <- rlnorm(100, 0, 2)
#' log_normal_variance_one_sample(x, 1, "greater")
#' @export
log_normal_variance_one_sample <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_log_normal_sigma.squared, sigma.squared, 45, 0)

#' @keywords internal
calc_test_stat_log_normal_mu_one_way <- function(x, fctr) {
  if (base::any(x <= 0)) {
    stop("All values in x must be positive.")
  }
  x <- base::log(x = x)
  W <- calc_test_stat_normal_mu_one_way(x = x, fctr = fctr)
  return(W)
}

#' Test the equality of means of log normal distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All mus are equal. (mu1 = mu2 ... muk).
#' \item Alternative: At least one mu is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rlnorm(150, 1, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' log_normal_mu_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rlnorm(50, 1, 1), rlnorm(50, 2, 1), rlnorm(50, 3, 1))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' log_normal_mu_one_way(x, fctr, .95)
#' @export
log_normal_mu_one_way <- LRTesteR:::create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_log_normal_mu_one_way, log_normal_mu_one_sample, 30)

#' @keywords internal
calc_test_stat_log_normal_sigma.squared_one_way <- function(x, fctr) {
  if (base::any(x <= 0)) {
    stop("All values in x must be positive.")
  }
  x <- base::log(x = x)
  W <- calc_test_stat_normal_sigma.squared_one_way(x = x, fctr = fctr)
  return(W)
}

#' Test the equality of variance parameters of log normal distributions.
#'
#' @inheritParams gaussian_mu_one_way
#' @inherit gaussian_mu_one_way return
#' @inherit gaussian_mu_one_way source
#' @details
#' \itemize{
#' \item Null: All variances are equal. (o^2_1 = o^2_2 ... o^2_k).
#' \item Alternative: At least one variance is not equal.
#' }
#' @examples
#' library(LRTesteR)
#'
#' # Null is true
#' set.seed(1)
#' x <- rlnorm(150, 1, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' log_normal_variance_one_way(x, fctr, .95)
#'
#' # Null is false
#' set.seed(1)
#' x <- c(rlnorm(50, 1, 1), rlnorm(50, 1, 2), rlnorm(50, 1, 3))
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' log_normal_variance_one_way(x, fctr, .95)
#' @export
log_normal_variance_one_way <- LRTesteR:::create_test_function_one_way_case_one(LRTesteR:::calc_test_stat_log_normal_sigma.squared_one_way, log_normal_variance_one_sample, 90)
