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
calc_test_stat <- function(x, fctr) {
  obs_mean <- base::mean(x)

  profile_sd <- (sum((x - obs_mean)^2) / length(x))^.5

  group_means <- vector(mode = "numeric", length = length(levels(fctr)))

  W1 <- sum(stats::dnorm(x = x, mean = obs_mean, sd = profile_sd, log = TRUE))

  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    temp <- base::mean(tempX)
    group_means[i] <- temp
    names(group_means)[i] <- l
  }

  SS <- 0
  for (i in seq_along(levels(fctr))) {
    l <- levels(fctr)[i]
    index <- which(fctr == l)
    tempX <- x[index]
    tempSS <- sum((tempX - group_means[i])^2)
    SS <- SS + tempSS
  }
  profile_sd_ha <- (SS / length(x))^.5

  likelihoods <- vector(mode = "numeric", length = length(group_means))
  for (i in seq_along(levels(fctr))) {
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

f <- LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, gaussian_mu_one_sample)
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
    expect_error(LRTesteR:::create_test_function_one_way_case_one(1), "Argument calc_test_stat must be a function.")
    expect_error(LRTesteR:::create_test_function_one_way_case_one(helper_one), "calc_test_stat's first argument is not x.")
    expect_error(LRTesteR:::create_test_function_one_way_case_one(helper_two), "calc_test_stat's second argument is not fctr.")
    expect_error(LRTesteR:::create_test_function_one_way_case_one(helper_three), "calc_test_stat has too many arguments.")
  })
}
rm(helper_one, helper_two, helper_three)

helper_one <- function(typo, mu, alternative, conf.level) {}
helper_two <- function(x, mu, typo, conf.level) {}
helper_three <- function(x, mu, alternative, typo) {}
helper_four <- function(x, mu, alternative, conf.level, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, 1), "Argument calc_individual_CI must be a function.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, helper_one), "calc_individual_CI's first argument is not x.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, helper_two), "calc_individual_CI's third argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, helper_three), "calc_individual_CI's fourth argument is not conf.level.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat, helper_four), "calc_individual_CI has too many arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four)
