###############################################
# Test Results
###############################################

f <- LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_mu, mu, 15)
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

test_that("n_min input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, "asdf"), "n_min should be numeric.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, c(1, 2)), "n_min should have length one.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, -2), "n_min should be greater than one.")
})

test_that("LB input checking works", {
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, 45, "asdf"), "LB should be numeric.")
  expect_error(LRTesteR:::create_test_function_one_sample_case_one(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, 45, c(1, 2)), "LB should have length one.")
})

###############################################
# Test Results
###############################################
f <- LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, gaussian_mu_one_sample, 30)
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
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, 1), "Argument calc_individual_CI must be a function.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, helper_one), "calc_individual_CI's first argument is not x.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, helper_two), "calc_individual_CI's third argument is not alternative.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, helper_three), "calc_individual_CI's fourth argument is not conf.level.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, helper_four), "calc_individual_CI has too many arguments.")
})
rm(helper_one, helper_two, helper_three, helper_four)

test_that("n_min input checking works", {
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, gaussian_mu_one_sample, "asdf"), "n_min should be numeric.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, gaussian_mu_one_sample, c(1, 2)), "n_min should have length one.")
  expect_error(LRTesteR:::create_test_function_one_way_case_one(calc_test_stat_normal_mu_one_way, gaussian_mu_one_sample, -2), "n_min should be greater than three.")
})
