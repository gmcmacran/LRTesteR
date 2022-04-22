############################
# For gaussian variance test, what is the power of the chi square test?
#
# For poisson, find an exact test to confirm results are similar?
# Can a glm model be used to confirm results?
# Why does seed matter so much
# chisq.pois test in stats package
# Can poisson approximation to binomial be used to make a nearly exact test?
# https://stats.stackexchange.com/questions/92627/how-to-use-the-chi-squared-test-to-determine-if-data-follow-the-poisson-distribu
#
# All tests assume large N. What should be the cut off?
#
# For binomial, can a glm model be used to confirm results?
# Why does N have to be so large for tests to pass?
#
# Should gamma version be made? Textbook uses this as a case study
# of it wilk's theoem being a bad approximation. Tests currently fail.
#
# For negative binomial, find an exact test or some form of confirmation
# test is correct.
# https://stats.libretexts.org/Bookshelves/Probability_Theory/Probability_Mathematical_Statistics_and_Stochastic_Processes_(Siegrist)/11%3A_Bernoulli_Trials/11.04%3A_The_Negative_Binomial_Distribution
# Confirm this test produces similar results to normal approximation of negative binomial
# calc_Z <- function(success, failure) {
#   p <- success / (success + failure)
#   N <- success + failure
#
#   Z <- (p*N - success) / (success*(1-p))^.5
#   return(Z)
# }
#
#
# calc_Z(50, 50)
#
# Z <- calc_Z(500, 500)
# pnorm(Z, lower.tail = FALSE)
# negative_binomial_p_lr_test(50, 50, .50, "two.sided")
#
# https://calcworkshop.com/discrete-probability-distribution/negative-binomial-distribution/
# dnbinom(x = 5, size = 8, prob = .78)
#
# Write script to test type one error rate at alpha .01, .05, .10.
#
# What is known about sampling distribution when null is false?
# Are there any publications around power?
# Use monte carlo methods to calculate power.
#
# Extend tests to multiple seeds
#


Z <- 3.5
2 * min(pnorm(-1*Z, lower.tail = TRUE), pnorm(Z, lower.tail = FALSE))
pchisq(Z^2, df = 1, lower.tail = FALSE)


