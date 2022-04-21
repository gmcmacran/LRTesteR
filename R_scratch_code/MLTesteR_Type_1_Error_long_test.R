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
library(stringr)

################
# gaussian
###############
B <- 5000
N <- 200

mus <- -3:3
variances <- 1:5

sim_results <- tibble()

for (mu in mus) {
  for (variance in variances) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gaussian_mean_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnorm(n = N, mean = mu, sd = variance^.5)
        test <- gaussian_mean_lr_test(x, mu, alt)
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
  nrow() == 2

sim_results %>%
  distinct(mu) %>%
  nrow() == length(mus)

sim_results %>%
  distinct(variance) %>%
  nrow() == length(variances)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

temp <- sim_results %>%
  group_by(test, alt, mu, variance) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI))

ggplot(temp, aes(x = factor(mu), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Mu", y = "Type I Error")

ggplot(temp, aes(x = factor(variance), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Variance", y = "Type I Error")

ggplot(temp, aes(x = factor(alt), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Alternative Hypothesis", y = "Type I Error")

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Hypothesis Test", y = "Type I Error")

################
# gamma
################

B <- 1000
N <- 50

shapes <- c(1:10, seq(15, 50, 10))
rates <- c(1:10, seq(15, 50, 10))

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
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
  }
}
sim_results %>% saveRDS("R_scratch_code/gamma_type_one_rate.rds")
rm(sim_results)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_scale_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, scale = scale)
        try(test <- gamma_scale_lr_test(x, scale, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("R_scratch_code/gamma_type_one_scale.rds")
rm(sim_results)

sim_results <- tibble()
for (shape in shapes) {
  for (rate in rates) {
    scale <- 1 / rate

    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "gamma_shape_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rgamma(n = N, shape = shape, rate = rate)
        try(test <- gamma_shape_lr_test(x, shape, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape = shape, rate = rate, scale = scale, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("R_scratch_code/gamma_type_one_shape.rds")
rm(sim_results)

sim_results <- bind_rows(
  readRDS(file = "R_scratch_code/gamma_type_one_rate.rds"),
  readRDS(file = "R_scratch_code/gamma_type_one_scale.rds"),
  readRDS(file = "R_scratch_code/gamma_type_one_shape.rds")
)

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
  distinct(scale) %>%
  nrow() == length(rates)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

temp <- sim_results %>%
  mutate(scale = round(scale, 2)) %>%
  group_by(test, alt, shape, rate, scale) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI))

ggplot(temp, aes(x = factor(shape), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Shape", y = "Type I Error")

ggplot(temp, aes(x = factor(rate), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Rate", y = "Type I Error")

ggplot(temp, aes(x = factor(scale), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Scale", y = "Type I Error")

ggplot(temp, aes(x = factor(alt), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Alternative Hypothesis", y = "Type I Error")

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Hypothesis Test", y = "Type I Error")

################
# poisson
###############
B <- 5000
N <- 200

lambdas <- 1:15

sim_results <- tibble()

for (lambda in lambdas) {
  for (alt in c("two.sided", "less", "greater")) {
    stats <- vector(mode = "numeric", length = B)
    pvalues <- vector(mode = "numeric", length = B)
    alts <- vector(mode = "character", length = B)
    testName <- "poisson_lambda_lr_test"
    set.seed(1)
    for (i in 1:B) {
      x <- rpois(n = N, lambda = lambda)
      test <- poisson_lambda_lr_test(x, lambda, alt)
      stats[i] <- test$statistic
      pvalues[i] <- test$p.value
      alts[i] <- test$alternative
    }
    temp <- tibble(test = testName, lambda = lambda, stat = stats, pvalue = pvalues, alt = alts)
    sim_results <- sim_results %>% bind_rows(temp)
    rm(stats, pvalues, alts, testName, temp, i)
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(lambda) %>%
  nrow() == length(lambdas)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

temp <- sim_results %>%
  group_by(test, alt, lambda) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI))

ggplot(temp, aes(x = factor(lambda), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Lambda", y = "Type I Error")

ggplot(temp, aes(x = factor(alt), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Alternative Hypothesis", y = "Type I Error")

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Hypothesis Test", y = "Type I Error")

################
# beta
################

B <- 1000
N <- 200

shape1s <- c(1:10, seq(15, 50, 10))
shape2s <- c(1:10, seq(15, 50, 10))

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape1_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape1, shape2 = shape2)
        try(test <- beta_shape1_lr_test(x, shape1, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("R_scratch_code/beta_type_one_shape1.rds")
rm(sim_results)

sim_results <- tibble()
for (shape1 in shape1s) {
  for (shape2 in shape2s) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "beta_shape2_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rbeta(N, shape1 = shape1, shape2 = shape2)
        try(test <- beta_shape2_lr_test(x, shape2, alt))
        try(stats[i] <- test$statistic)
        try(pvalues[i] <- test$p.value)
        try(alts[i] <- test$alternative)
      }
      temp <- tibble(test = testName, shape1 = shape1, shape2 = shape2, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}
sim_results %>% saveRDS("R_scratch_code/beta_type_one_shape2.rds")
rm(sim_results)

sim_results <- bind_rows(
  readRDS(file = "R_scratch_code/beta_type_one_shape1.rds"),
  readRDS(file = "R_scratch_code/beta_type_one_shape2.rds")
)

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 2

sim_results %>%
  distinct(shape1) %>%
  nrow() == length(shape1s)

sim_results %>%
  distinct(shape2) %>%
  nrow() == length(shape2s)

sim_results %>%
  distinct(alt) %>%
  nrow() == 3

sim_results %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

temp <- sim_results %>%
  group_by(test, alt, shape1, shape2) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI))

ggplot(temp, aes(x = factor(shape1), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Shape 1", y = "Type I Error")

ggplot(temp, aes(x = factor(shape2), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Shape 2", y = "Type I Error")

ggplot(temp, aes(x = factor(alt), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Alternative Hypothesis", y = "Type I Error")

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Hypothesis Test", y = "Type I Error")
