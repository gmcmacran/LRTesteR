###############################################
# Test Results
###############################################
calc_MLE <- calc_MLE_binomial_prob

calc_test_stat <- calc_test_stat_binomial_prob

f <- LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, calc_test_stat, x, size)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "size", "prob", "alternative", "conf.level")))
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
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(helper_three), "calc_MLE should have exactly two arguments.")
})
rm(helper_one, helper_two, helper_three)

helper_one <- function(typo, arg2, prob, alternative) {}
helper_two <- function(arg1, typo, prob, alternative) {}
helper_three <- function(arg1, arg2, typo, alternative) {}
helper_four <- function(arg1, arg2, prob, typo) {}
helper_five <- function(arg1, arg2, prob, alternative, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, 1), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_one), "calc_test_stat's first argument is not arg1.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_two), "calc_test_stat's second argument is not arg2.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_three), "calc_test_stat's third argument is not prob.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_four), "calc_test_stat's fourth argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, helper_five), "calc_test_stat should have exactly four arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four, helper_five)

test_that("binom and neg binom arg check", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_two(calc_MLE, calc_test_stat, x, foo), "Arg2 is not size or num_successes.")
})

###############################################
# Test Results
###############################################
calc_test_stat <- function(x, size, fctr) {
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

  return(W)
}

f <- LRTesteR:::create_test_function_one_way_case_two(calc_test_stat, binomial_prob_test)
test_that("Check structure.", {
  expect_true(class(f) == "function")
  expect_true(all(names(formals(f)) == c("x", "size", "fctr", "conf.level")))
})
rm(f)

###############################################
# Input checking
###############################################
helper_one <- function(typo, size, fctr) {}
helper_two <- function(x, typo, fctr) {}
helper_three <- function(x, size, typo) {}
helper_four <- function(x, size, fctr, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_way_case_two(1, binomial_prob_test), "Argument calc_test_stat must be a function.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(helper_one, binomial_prob_test), "calc_test_stat's first argument does not match calc_individual_CI first argument.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(helper_two, binomial_prob_test), "calc_test_stat's second argument does not match calc_individual_CI second argument.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(helper_three, binomial_prob_test), "calc_test_stat's third argument is not fctr.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(helper_four, binomial_prob_test), "calc_test_stat should have exactly three arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four)

helper_one <- function(x, size, prob, typo, conf.level) {}
helper_two <- function(x, size, prob, alternative, type) {}
helper_three <- function(x, size, prob, alternative, conf.level, extra) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_one_way_case_two(calc_test_stat, 1), "Argument calc_individual_CI must be a function.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(calc_test_stat, helper_one), "calc_individual_CI's fourth argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(calc_test_stat, helper_two), "calc_individual_CI's fifth argument is not conf.level.")
  expect_error(LRTesteR:::create_test_function_one_way_case_two(calc_test_stat, helper_three), "calc_individual_CI should have exactly five arguments.")
})
rm(helper_one, helper_two, helper_three)
