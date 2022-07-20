###############################################
# Test Results
###############################################
for (alt in c("two.sided", "greater", "less")) {
  f <- LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_mu, mu)
  test_that("Check structure.", {
    expect_true(class(f) == "function")
    expect_true(all(names(formals(f)) == c("x", "mu", "alternative", "conf.level")))
  })
}
rm(f)

###############################################
# Input checking
###############################################
calc_test_helper_one <- function(typo, mu, alternative) {}
calc_test_helper_two <- function(x, mu, typo) {}
test_that("calc_test_stat input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous(1), NULL)
  expect_error(LRTesteR:::create_test_function_continuous(calc_test_helper_one, mu), NULL)
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_mu, sigma.squared), NULL)
  expect_error(LRTesteR:::create_test_function_continuous(calc_test_helper_two, mu), NULL)
})
rm(calc_test_helper_one, calc_test_helper_two)

test_that("LB input checking works", {
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, "asdf"), NULL)
  expect_error(LRTesteR:::create_test_function_continuous(LRTesteR:::calc_test_stat_normal_sigma.squared, sigma.squared, c(1, 2)), NULL)
})
