library(MLTesteR)

negative_binomial_p_lr_test(500, 500, .50, "two.sided")
2*min(pnbinom(q = 500, size = 500, prob = .50, lower.tail = TRUE), pnbinom(q = 500, size = 500, prob = .50, lower.tail = FALSE))

negative_binomial_p_lr_test(5000, 5000, .50, "greater")
pnbinom(q = 4900, size = 5000, prob = .50, lower.tail = FALSE)

negative_binomial_p_lr_test(500, 500, .50, "less")
pnbinom(q = 500, size = 500, prob = .50, lower.tail = TRUE)



negative_binomial_p_lr_test(500, 100, .50, "two.sided")
2*min(pnbinom(q = 100, size = 500, prob = .50, lower.tail = TRUE), pnbinom(q = 99, size = 500, prob = .50, lower.tail = FALSE))

negative_binomial_p_lr_test(500, 100, .50, "greater")
pnbinom(q = 99, size = 500, prob = .50, lower.tail = FALSE)

negative_binomial_p_lr_test(500, 100, .50, "less")
pnbinom(q = 100, size = 500, prob = .50, lower.tail = TRUE)


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
