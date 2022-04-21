# https://en.wikipedia.org/wiki/Negative_binomial_distribution
# https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/11%3A_Bernoulli_Trials/11.04%3A_The_Negative_Binomial_Distribution



library(MLTesteR)

calc_two_sided_p_value <- function(x, size, prob) {
  if (prob == 0) {
    (x == 0)
  } else if (prob == 1) {
    (x == size)
  } else {
    relErr <- 1 + 1e-07
    d <- dnbinom(x, size, prob)
    m <- size*(1-prob)/prob
    if (x == m) {
      1
    } else if (x < m) {
      nearInf <- m*20
      i <- seq.int(from = ceiling(m), to = nearInf)
      y <- sum(dnbinom(i, size, prob) <= d * relErr)
      pnbinom(x, size, prob) + pnbinom(nearInf - y, size, prob, lower.tail = FALSE)
    } else {
      i <- seq.int(from = 0, to = floor(m))
      y <- sum(dnbinom(i, size, prob) <= d * relErr)
      pnbinom(y - 1, size, prob) + pnbinom(x - 1, size, prob, lower.tail = FALSE)
    }
  }
}

negative_binomial_p_lr_test(5000, 5000, .50, "two.sided")
calc_two_sided_p_value(5000, 5000, .50)

negative_binomial_p_lr_test(5000, 5000, .50, "greater")
pnbinom(q = 4999, size = 5000, prob = .50, lower.tail = FALSE)

negative_binomial_p_lr_test(5000, 5000, .50, "less")
pnbinom(q = 5000, size = 5000, prob = .50, lower.tail = TRUE)




negative_binomial_p_lr_test(1000, 5000, .50, "two.sided")
calc_two_sided_p_value(1000, 5000, .50)


negative_binomial_p_lr_test(10, 50, .50, "greater")
pnbinom(q = 10, size = 50, prob = .50, lower.tail = TRUE)

negative_binomial_p_lr_test(10, 50, .50, "less")
pnbinom(q = 9, size = 50, prob = .50, lower.tail = FALSE)

calc_mean <- function(size, prob) {
  return(size * (1 - prob) / prob)
}

ps <- seq(.05, .95, .05)
sizes <- seq(05, 200, 10)

set.seed(2)
results <- c()
for (p in ps) {
  for (size in sizes) {
    m1 <- calc_mean(size, p)
    m2 <- mean(rnbinom(20000, size, p))
    results <- c(results, m1 - m2)
  }
}
mean(results)

calc_mean(200, .05)











# binomial lower tail
sum(dbinom(x = 0:40, size = 100, p = .50))
pbinom(q = 40, size = 100, prob = .50, lower.tail = TRUE)

# binomial upper tail
sum(dbinom(x = 41:100, size = 100, p = .50))
pbinom(q = 40, size = 100, prob = .50, lower.tail = FALSE)

# negative binomial lower tail
sum(dnbinom(x = 0:10, size = 3, p = .50))
pnbinom(q = 10, size = 3, prob = .50, lower.tail = TRUE)

# negative binomial upper tail
sum(dnbinom(x = 11:999, size = 3, p = .50))
pnbinom(q = 10, size = 3, prob = .50, lower.tail = FALSE)




# https://calcworkshop.com/discrete-probability-distribution/negative-binomial-distribution/
dnbinom(x = 3, size = 5, prob = .78)

dnbinom(x = 3, size = 5, prob = .78)

dnbinom(x = 3, size = 5, prob = .78) + dnbinom(x = 4, size = 4, prob = .78) + dnbinom(x = 5, size = 3, prob = .78) + dnbinom(x = 6, size = 2, prob = .78) + dnbinom(x = 7, size = 1, prob = .78) + dnbinom(x = 8, size = 0, prob = .78)

pnbinom(q = 3, size = 5, prob = .78, lower.tail = FALSE)
1 - pnbinom(q = 3, size = 5, prob = .78, lower.tail = TRUE)
