######################
# Confirm mean calculation is correct
######################
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

######################
# check comparison between negative binomial and geomatric distribution
######################
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

######################
# define exact p value calculations
######################
exact_test <- function(num_failures, p, alternative) {
  calc_two_sided_p_value <- function(x, prob) {
    if (prob == 0) {
      (as.numeric(x >= 0))
    } else if (prob == 1) {
      (as.numeric(x == 0))
    } else {
      relErr <- 1 + 1e-05
      d <- dgeom(x, prob)
      m <- (1 - prob) / prob
      if (x == m) {
        1
      } else if (x < m) {
        nearInf <- ceiling(m * 20)
        i <- seq.int(from = ceiling(m), to = nearInf)
        i <- setdiff(i, x)
        y <- sum(dgeom(i, prob) < d * relErr)
        pgeom(x, prob) + pgeom(pmax(nearInf - y, 0), prob, lower.tail = FALSE)
      } else {
        i <- seq.int(from = 0, to = floor(m))
        i <- setdiff(i, x)
        y <- sum(dgeom(i, prob) < d * relErr)
        pgeom(y - 1, prob) + pgeom(x - 1, prob, lower.tail = FALSE)
      }
    }
  }

  calc_left_p_value <- function(x, prob) {
    pgeom(q = x, prob = p, lower.tail = TRUE)
  }

  calc_right_p_value <- function(x, prob) {
    pgeom(q = x - 1, prob = p, lower.tail = FALSE)
  }

  if (alternative == "two.sided") {
    p.value <- calc_two_sided_p_value(num_failures, p)
  }
  if (alternative == "greater") {
    p.value <- calc_left_p_value(num_failures, p)
  }
  if (alternative == "less") {
    p.value <- calc_right_p_value(num_failures, p)
  }
  return(list(p.value = p.value))
}


######################
# Quality check function
######################
exact_test(0, .05, alt)

exact_test(0, 1.00, "two.sided")$p.value == 1
exact_test(1, 1.00, "two.sided")$p.value == 0

exact_test(0, 0, "two.sided")$p.value == 0
exact_test(1, 0, "two.sided")$p.value == 1
exact_test(2, 0, "two.sided")$p.value == 1

exact_test(0, .50, "two.sided")$p.value == 1
exact_test(1, .50, "two.sided")$p.value == 1

# Does p value increase as a function failure?
results <- c()
for (p in seq(.05, .95, .05))
  for (i in seq(2, 10, 1))
    results <- c(results, exact_test(i, p, "greater")$p.value < exact_test(i+1, p, "greater")$p.value)
all(results)

# Does p value decrease as a function failure?
results <- c()
for (p in seq(.05, .95, .05))
  for (i in seq(2, 10, 1))
    results <- c(results, exact_test(i, p, "less")$p.value > exact_test(i+1, p, "less")$p.value)
all(results)


exact_test(0, 1.00, "two.sided")$p.value
exact_test(0, 1.00, "greater")$p.value

exact_test(1, 0, "two.sided")$p.value
exact_test(1, 0, "less")$p.value


temp <- tibble()
for (failure in seq(0, 100, 1)) {
  ps <- seq(.05, .95, .05)
  temp <- temp %>%
    bind_rows(tibble(
      failure = failure,
      p = rep(ps, 3),
      alt = c(rep("two.sided", length(ps)), rep("greater", length(ps)), rep("less", length(ps)))
    ))
}
temp <- temp %>%
  rowwise() %>%
  mutate(p.value = exact_test(failure, p, alt)$p.value) %>%
  ungroup()
temp %>% pull(p.value) %>% min()
temp %>% pull(p.value) %>% max()

ggplot(temp, aes(x = failure, y = p.value)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  facet_grid(rows = vars(alt), cols = vars(p))

temp_02 <- temp %>%
  filter(alt == "two.sided") %>%
  select(p, failure, p.value) %>%
  mutate(type = "Two Sided") %>%
  bind_rows(
    temp %>%
      filter(alt %in% c("greater", "less")) %>%
      group_by(p, failure) %>%
      summarise(p.value = min(p.value)) %>%
      ungroup() %>%
      mutate(type = "One Sided")
  ) %>%
  arrange(p, failure, type)
temp_02

ggplot(temp_02, aes(x = failure, y = p.value)) +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 1, .05)) +
  facet_grid(rows = vars(type), cols = vars(p))

temp_03 <- temp %>%
  filter(alt == "two.sided") %>%
  mutate(twoSidedP = p.value) %>%
  select(p, failure, twoSidedP) %>%
  inner_join(temp %>%
               filter(alt %in% c("greater", "less")) %>%
               group_by(p, failure) %>%
               summarise(oneSidedP = min(p.value)) %>%
               ungroup(),
             by = c("p", "failure")) %>%
  arrange(p, failure) %>%
  mutate(twoMoreOne = twoSidedP >= oneSidedP)

temp_03 %>%
  pull(twoMoreOne) %>%
  mean()

temp_03 %>%
  mutate(diff = twoSidedP - oneSidedP) %>%
  summarise(minDiff = min(diff),
            meanDiff = mean(diff),
            maxDiff = max(diff))


######################
# Find values of p where lr test matches exact test
######################
ps <- seq(.05, .95, .01)
p_lr <- c()
p_exact <- c()
alt <- "greater"
for (p in ps) {
  p_lr <- c(p_lr, geometric_p_lr_test(0, p, alt)$p.value)
  p_exact <- c(p_exact, exact_test(0, p, alt)$p.value)
}
compare <- abs(p_lr - p_exact)
length(p_lr) == length(ps)
length(p_exact) == length(ps)

min(p_exact)
min(p_lr)

indx <- p_lr <= .05
indx
p_lr <- p_lr[indx]
p_exact <- p_exact[indx]
compare <- compare[indx]
ps <- ps[indx]

ps[which.min(compare)]
compare[which.min(compare)]

