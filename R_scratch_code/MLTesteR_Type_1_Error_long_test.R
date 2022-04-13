########################################
# Overview
#
# A script to test type one error across
# many parameters.
########################################
library(MLTesteR)
library(dplyr)
library(purrr)
library(ggplot2)

################
# gaussian
###############
B <- 5000
N <- 100

mus <- seq(-10, 10, 1)
variances <- seq(1, 30, 1)

sim_results <- tibble()

for(mu in mus) {
  for(variance in variances) {

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_mean_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_mean_lr_test(x, mean, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_variance_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_variance_lr_test(x, variance, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, mu = mu, variance = variance, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(shape) %>%
  nrow() == length(shapes)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min() <= 1

sim_results %>%
  group_by(test, alt, shape, rate) %>%
  summarise(TypeI = mean(pvalue <= .05), meanStat = mean(stat)) %>%
  arrange(desc(TypeI))

################
# gamma
################

B <- 1000
N <- 50

shapes <- c(1:10, seq(15, 50, 10))
rates <- c(1:10, seq(15, 50, 10))

sim_results <- tibble()

for(shape in shapes) {
  for(rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_rate_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_rate_lr_test(x, rate, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_scale_lr_test"
      set.seed(2)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_scale_lr_test(x, rate, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp)
    }

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_shape_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_shape_lr_test(x, rate, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp)
    }
  }
}

sim_results %>%
  saveRDS(file = "R_scratch_code/gammaTypeOneDF.rds")

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 3

sim_results %>%
  distinct(shape) %>%
  nrow() == length(shapes)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(rate) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) <= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

temp <- sim_results %>%
  group_by(test, alt, shape, rate) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm=TRUE), meanStat = mean(stat, na.rm=TRUE)) %>%
  arrange(desc(TypeI))

temp

ggplot(temp, aes(x = factor(shape), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1))

ggplot(temp, aes(x = factor(rate), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1))

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1))
