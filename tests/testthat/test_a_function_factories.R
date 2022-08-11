###############################################
# Test Results
###############################################

f <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_mu, mu)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "mu", "alternative", "conf.level")))
})
rm(f)

###############################################
# Input checking
###############################################
calc_test_helper_one <- function(typo, mu, alternative) {}
calc_test_helper_two <- function(x, mu, typo) {}
calc_test_helper_three <- function(x, mu, alternative, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(1), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(calc_test_helper_one, mu), "calc_test_stat's first argument is not x.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_mu, sigma.squared), "calc_test_stat's second argument is does not match p0.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(calc_test_helper_two, mu), "calc_test_stat's third argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(calc_test_helper_three, mu), "calc_test_stat has too many arguments.")
})
rm(calc_test_helper_one, calc_test_helper_two, calc_test_helper_three)

test_that("LB input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, "asdf"), "LB should be numeric.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, c(1, 2)), "LB should have length one.")
})

###############################################
# Test Results
###############################################
calc_MLE <- function(arg1, arg2) {
  ops_p <- arg1 / arg2
  return(ops_p)
}

calc_test_stat <- function(arg1, arg2, p, alternative) {
  obs_p <- calc_MLE(arg1, arg2)
  W <- 2 * (sum(stats::dbinom(x = arg1, size = arg2, p = obs_p, log = TRUE)) -
    sum(stats::dbinom(x = arg1, size = arg2, p = p, log = TRUE)))

  if (alternative != "two.sided") {
    W <- sign(obs_p - p) * W^.5
  }

  return(W)
}

f <- LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, calc_test_stat, x, n)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "n", "p", "alternative", "conf.level")))
})
rm(f)

###############################################
# Input checking
###############################################
helper_one <- function(typo, arg2) {}
helper_two <- function(arg1, typo) {}
helper_three <- function(arg1, arg2, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(1), "Argument calc_MLE must be a function.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(helper_one), "calc_MLE's first argument is not arg1.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(helper_two), "calc_MLE's second argument is not arg2.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(helper_three), "calc_MLE has too many arguments.")
})
rm(helper_one, helper_two, helper_three)

helper_one <- function(typo, arg2, p, alternative) {}
helper_two <- function(arg1, typo, p, alternative) {}
helper_three <- function(arg1, arg2, typo, alternative) {}
helper_four <- function(arg1, arg2, p, typo) {}
helper_five <- function(arg1, arg2, p, alternative, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, 1), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_one), "calc_test_stat's first argument is not arg1.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_two), "calc_test_stat's second argument is not arg2.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_three), "calc_test_stat's third argument is not p.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_four), "calc_test_stat's fourth argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_five), "calc_test_stat has too many arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four, helper_five)

test_that("binom and neg binom arg check", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, calc_test_stat, x, foo), "Arg2 is not n or num_successes.")
})

###############################################
# Test Results
###############################################
calc_test_stat <- function(x, fctr) {
  obs_mean <- base::mean(x)

  profile_sd <- (sum((x - obs_mean)^2) / length(x))^.5

  group_means <- vector(mode = "numeric", length = length(levels(fctr)))

  W1 <- sum(stats::dnorm(x = x, mean = obs_mean, sd = profile_sd, log = TRUE))

  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- base::mean(tempX)
    group_means[i] <- temp
    names(group_means)[i] <- l
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

f <- LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, gaussian_mu_one_sample)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "fctr", "conf.level")))
})
rm(f)

###############################################
# Input checking
###############################################
helper_one <- function(typo, fctr) {}
helper_two <- function(x, typo) {}
helper_three <- function(x, fctr, extra) {
  test_that("calc_test_stat input checking works", {
    expect_error(LRTesteR:::create_test_function_continuous_one_way(1), "Argument calc_test_stat must be a function.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way(helper_one), "calc_test_stat's first argument is not x.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way(helper_two), "calc_test_stat's second argument is not fctr.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way(helper_three), "calc_test_stat has too many arguments.")
  })
}
rm(helper_one, helper_two, helper_three)

helper_one <- function(typo, mu, alternative, conf.level) {}
helper_two <- function(x, mu, typo, conf.level) {}
helper_three <- function(x, mu, alternative, typo) {}
helper_four <- function(x, mu, alternative, conf.level, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, 1), "Argument calc_individual_CI must be a function.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, helper_one), "calc_individual_CI's first argument is not x.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, helper_two), "calc_individual_CI's third argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, helper_three), "calc_individual_CI's fourth argument is not conf.level.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way(calc_test_stat, helper_four), "calc_individual_CI has too many arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four)

###############################################
# Test Results
###############################################
calc_test_stat <- function(x, n, fctr) {
  # Null
  obs_p <- base::sum(x) / sum(n)

  W1 <- sum(stats::dbinom(x = x, size = n, prob = obs_p, log = TRUE))

  # alt
  likelihoods <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in 1:length(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempN <- n[index]
    tempP <- tempX / tempN
    likelihoods[i] <- sum(stats::dbinom(x = tempX, size = tempN, prob = tempP, log = TRUE))
  }

  W2 <- sum(likelihoods)

  W <- 2 * (W2 - W1)

  return(W)
}

f <- LRTesteR:::create_test_function_continuous_one_way_case_two(calc_test_stat, binomial_p_one_sample)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "n", "fctr", "conf.level")))
})
rm(f)

###############################################
# Input checking
###############################################
helper_one <- function(typo, n, fctr) {}
helper_two <- function(x, typo, fctr) {}
helper_three <- function(x, n, typo) {}
helper_four <- function(x, n, fctr, extra) {
  test_that("calc_test_stat input checking works", {
    expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(1, binomial_p_one_sample), "Argument calc_test_stat must be a function.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(helper_one, binomial_p_one_sample), "calc_test_stat's first argument does not match calc_individual_CI first argument.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(helper_two, binomial_p_one_sample), "calc_test_stat's second argument does not match calc_individual_CI second argument.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(helper_three, binomial_p_one_sample), "calc_test_stat's third argument is not fctr.")
    expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(helper_four, binomial_p_one_sample), "calc_test_stat has too many arguments.")
  })
}
rm(helper_one, helper_two, helper_three, helper_four)

helper_one <- function(x, n, p, typo, conf.level) {}
helper_two <- function(x, n, p, alternative, type) {}
helper_three <- function(x, n, p, alternative, conf.level, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(calc_test_stat, 1), "Argument calc_individual_CI must be a function.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(calc_test_stat, helper_one), "calc_individual_CI's fourth argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(calc_test_stat, helper_two), "calc_individual_CI's fifth argument is not conf.level.")
  expect_error(LRTesteR:::create_test_function_continuous_one_way_case_two(calc_test_stat, helper_three), "calc_individual_CI has too many arguments.")
})
rm(helper_one, helper_two, helper_three)
