library(MLTesteR)

# https://en.wikipedia.org/wiki/Negative_binomial_distribution
# https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/11%3A_Bernoulli_Trials/11.04%3A_The_Negative_Binomial_Distribution


###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  test <- negative_binomial_p_lr_test(50, 50, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value > .05)
  })
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  test <- binomial_p_lr_test(75, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}

for (alt in c("two.sided", "less")) {
  test <- binomial_p_lr_test(25, 100, .50, alt)

  test_that("Check structure.", {
    expect_true(class(test) == "mltest")
    expect_true(length(test) == 3)
    expect_true(all(names(test) == c("statistic", "p.value", "alternative")))
  })

  test_that("Check contents", {
    expect_true(test$p.value <= .05)
  })
}
