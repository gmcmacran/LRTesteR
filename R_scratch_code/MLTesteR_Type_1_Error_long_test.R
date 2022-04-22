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

################
# negative binomial
################
B <- 5000

ps <- seq(.05, .95, .05)
sizes <- seq(05, 200, 10)

all(ps < 1)
all(ps > 0)
all(sizes > 0)

sim_results <- tibble()
for (p in ps) {
  for (size in sizes) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "negative_binomial_p_lr_test"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size, p)
        test <- negative_binomial_p_lr_test(x, size, p, alt)
        stats[i] <- test$statistic
        pvalues[i] <- test$p.value
        alts[i] <- test$alternative
      }
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts)
      sim_results <- sim_results %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
sim_results %>%
  distinct(test) %>%
  nrow() == 1

sim_results %>%
  distinct(p) %>%
  nrow() == length(ps)

sim_results %>%
  distinct(size) %>%
  nrow() == length(sizes)

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
  group_by(test, alt, p, size) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI)) %>%
  ungroup()

ggplot(temp, aes(x = factor(p), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "P", y = "Type I Error")

ggplot(temp, aes(x = factor(size), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Size", y = "Type I Error")

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

# summarize 3 alts into one row by max of type I
temp_02 <- temp %>%
  mutate(TypeI = round(TypeI, 2)) %>%
  group_by(test, p, size) %>%
  summarise(TypeI = max(TypeI)) %>%
  ungroup() %>%
  arrange(desc(TypeI))

ggplot(temp_02, aes(x = factor(p), y = factor(size), fill = TypeI, label = TypeI)) +
  geom_tile(height = .50, width = .50) +
  geom_text() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "P", y = "Size", fill = "Type I Error")

# exact test
sim_results_02 <- tibble()
for (p in ps) {
  for (size in sizes) {
    for (alt in c("two.sided", "less", "greater")) {
      stats <- vector(mode = "numeric", length = B)
      pvalues <- vector(mode = "numeric", length = B)
      alts <- vector(mode = "character", length = B)
      testName <- "exact"
      set.seed(1)
      for (i in 1:B) {
        x <- rnbinom(1, size, p)
        if (alt == "two.sided") {
          stats[i] <- x
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
          pvalues[i] <- calc_two_sided_p_value(x, size, p)
          alts[i] <- alt
        }
        if (alt == "less") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x-1, size = size, prob = p, lower.tail = FALSE)
          alts[i] <- alt
        }
        if (alt == "greater") {
          stats[i] <- x
          pvalues[i] <- pnbinom(q = x, size = size, prob = p, lower.tail = TRUE)
          alts[i] <- alt
        }
      }
      temp <- tibble(test = testName, p = p, size = size, stat = stats, pvalue = pvalues, alt = alts)
      sim_results_02 <- sim_results_02 %>% bind_rows(temp)
      rm(stats, pvalues, alts, testName, temp, i)
    }
  }
}

# Check structure
nrow(sim_results) == nrow(sim_results_02)

sim_results_02 %>%
  distinct(test) %>%
  nrow() == 1

sim_results_02 %>%
  distinct(p) %>%
  nrow() == length(ps)

sim_results_02 %>%
  distinct(size) %>%
  nrow() == length(sizes)

sim_results_02 %>%
  distinct(alt) %>%
  nrow() == 3

sim_results_02 %>%
  pull(pvalue) %>%
  min(na.rm = TRUE) >= 0

sim_results_02 %>%
  pull(pvalue) %>%
  max(na.rm = TRUE) <= 1

sim_results_03 <- sim_results %>%
  bind_rows(sim_results_02)

temp <- sim_results_03 %>%
  group_by(test, alt, p, size) %>%
  summarise(TypeI = mean(pvalue <= .05, na.rm = TRUE), meanStat = mean(stat, na.rm = TRUE), N = sum(!is.na(pvalue))) %>%
  arrange(desc(TypeI)) %>%
  ungroup()

ggplot(temp, aes(x = p, y = TypeI, group = p)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_x_continuous(breaks = seq(0, 1, .10), limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "P", y = "Type I Error") +
  facet_wrap(vars(test))

ggplot(temp, aes(x = factor(size), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Size", y = "Type I Error") +
  facet_wrap(vars(test))

ggplot(temp, aes(x = factor(alt), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Alternative Hypothesis", y = "Type I Error") +
  facet_wrap(vars(test))

ggplot(temp, aes(x = factor(test), y = TypeI)) +
  geom_boxplot() +
  geom_hline(yintercept = .05) +
  scale_y_continuous(breaks = seq(0, 1, .05), limits = c(0, 1)) +
  labs(x = "Hypothesis Test", y = "Type I Error")

# summarize 3 alts into one row by max of type I
temp_02 <- temp %>%
  mutate(TypeI = round(TypeI, 2)) %>%
  group_by(test, p, size) %>%
  summarise(TypeI = max(TypeI)) %>%
  ungroup() %>%
  arrange(desc(TypeI))

temp_02 %>%
  ggplot(aes(x = p, y = factor(size), fill = TypeI, label = TypeI, group = p)) +
  geom_tile(height = .50, width = .03) +
  geom_text() +
  scale_x_continuous(breaks = seq(0, 1, .10), limits = c(0, 1)) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(x = "P", y = "Size", fill = "Type I Error") +
  facet_wrap(vars(test))

temp %>%
  group_by(test) %>%
  summarise(
    minTypeI = min(TypeI),
    meanTypeI = mean(TypeI),
    maxTypeI = max(TypeI)) %>%
  ungroup()





