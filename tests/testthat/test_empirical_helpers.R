###############################################
# Inner empirical likelihood solver
###############################################
# With nuisance parameters held fixed, the stacked estimating equation
# problem is an empirical likelihood test that a multivariate mean is zero.
# emplik::el.test solves the same problem.
set.seed(7)
for (i in 1:3) {
  n <- sample(30:60, 1)
  x <- rgamma(n, 2, 1)
  mu <- mean(x)
  s2 <- var(x)
  est_vect <- cbind(x - mu, (x - mu)^2 - s2, (x - mu)^3 / s2^1.5 - 1)

  sol <- LRTesteR:::calc_el_solution(est_vect)
  ref <- emplik::el.test(est_vect, mu = c(0, 0, 0))

  test_that("Solver matches emplik", {
    expect_equal(sol$W, unname(ref$"-2LLR"), tolerance = .1^6)
    expect_equal(sum(sol$p), 1, tolerance = .1^6)
    expect_true(min(sol$p) > 0)
  })
}
