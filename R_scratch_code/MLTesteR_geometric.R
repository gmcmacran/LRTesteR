calc_mean <- function(prob) {
  return((1 - prob) / prob)
}

ps <- seq(.05, .95, .05)

calc_mean(ps)

set.seed(2)
results <- c()
for (p in ps) {
  m1 <- calc_mean(p)
  m2 <- mean(rgeom(200000, p))
  results <- c(results, m1 - m2)
}
mean(results)

results <- c()
for (p in ps) {
  results <- c(results, all(round(dnbinom(1:50, 1, p), 10) == round(dgeom(1:50, p), 10)))
}
mean(results)

# Mathematically equivalent.
# Floating point problems.
results <- c()
for (p in ps) {
  results <- c(results, all(round(dnbinom(1:50, 1, p), 11) == round(dgeom(1:50, p), 11)))
}
mean(results)

# Log is better
results <- c()
for (p in ps) {
  results <- c(results, all(round(dnbinom(1:50, 1, p, log = TRUE), 11) == round(dgeom(1:50, p, log = TRUE), 11)))
}
mean(results)
