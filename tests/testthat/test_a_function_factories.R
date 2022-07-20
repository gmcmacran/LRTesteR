###############################################
# Test Results
###############################################

f <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_mu, mu)
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
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous(1), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_continuous(calc_test_helper_one, mu), "calc_test_stat's first argument is not x.")
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_mu, sigma.squared), "calc_test_stat's second argument is does not match p0.")
  expect_error(LRTesteR:::create_test_function_continuous(calc_test_helper_two, mu), "calc_test_stat's third argument is not alternative.")
})
rm(calc_test_helper_one, calc_test_helper_two)

test_that("LB input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, "asdf"), "LB should be numeric.")
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, c(1, 2)), "LB should have length one.")
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

f <- LRTesteR:::create_test_function_discrete(calc_MLE, calc_test_stat, x, n)
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
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_discrete(1), "Argument calc_MLE must be a function.")
  expect_error(LRTesteR:::create_test_function_discrete(helper_one), "calc_MLE's first argument is not arg1.")
  expect_error(LRTesteR:::create_test_function_discrete(helper_two), "calc_MLE's second argument is not arg2.")
})
rm(helper_one, helper_two)

helper_one <- function(typo, arg2, p, alternative) {}
helper_two <- function(arg1, typo, p, alternative) {}
helper_three <- function(arg1, arg2, typo, alternative) {}
helper_four <- function(arg1, arg2, p, typo) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_discrete(calc_MLE, 1), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_discrete(calc_MLE, helper_one), "calc_test_stat's first argument is not arg1.")
  expect_error(LRTesteR:::create_test_function_discrete(calc_MLE, helper_two), "calc_test_stat's second argument is not arg2.")
  expect_error(LRTesteR:::create_test_function_discrete(calc_MLE, helper_three), "calc_test_stat's third argument is not p.")
  expect_error(LRTesteR:::create_test_function_discrete(calc_MLE, helper_four), "calc_test_stat's fourth argument is not alternative.")
})
rm(helper_one, helper_two, helper_three, helper_four)
