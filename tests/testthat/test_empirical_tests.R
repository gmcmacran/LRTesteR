###############################################
# Null True
###############################################
for (alt in c("two.sided", "greater", "less")) {
  set.seed(1)
  x <- rnorm(200, 0, 1)
  test <- empirical_mu_one_sample(x, 0, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)


  test_that("Check contents", {
    expect_true(test$p.value > .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  # .0499 instead of .05 b/c of floating point error associated with convergence.
  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  test_that("Check CI", {
    expect_true(ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05) >= .0499)
    expect_true(ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05) >= .0499)
  })
  rm(CI1, CI2)
}

###############################################
# Null False
###############################################
for (alt in c("two.sided", "greater")) {
  set.seed(1)
  x <- rnorm(200, 3, 1)
  test <- empirical_mu_one_sample(x, 1, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

for (alt in c("two.sided", "less")) {
  set.seed(1)
  x <- rnorm(200, -3, 1)
  test <- empirical_mu_one_sample(x, -2, alt)

  test_that("Check structure.", {
    expect_true(all(class(test) == c("one_sample_case_three", "lrtest")))
    expect_true(length(test) == 5)
    expect_true(all(names(test) == c("statistic", "p.value", "conf.int", "conf.level", "alternative")))
  })

  # Compare with t test
  test_02 <- stats::t.test(x, alternative = alt)
  test_that("Check contents", {
    expect_true(test$p.value <= .05)
    expect_true(abs(test$p.value - test_02$p.value) < .01)
  })

  if (alt == "two.sided") {
    test_that("check contents", {
      expect_true(test$statistic >= 0)
      expect_equal(test$p.value, emplik::el.test(x, 0)$Pval)
    })
  }

  CI1 <- test$conf.int[1] + .Machine$double.eps # Avoid boundary
  CI2 <- test$conf.int[2] - .Machine$double.eps
  pval <- pmin(
    ifelse(is.finite(CI1), empirical_mu_one_sample(x, CI1, alt)$p.value, .05),
    ifelse(is.finite(CI2), empirical_mu_one_sample(x, CI2, alt)$p.value, .05)
  )
  test_that("Check CI", {
    expect_true(pval <= .0500001)
  })
  rm(CI1, CI2, pval)
}

###############################################
# Dr Owen's code R code
###############################################
# https://artowen.su.domains/empirical/
# https://artowen.su.domains/empirical/scel.R
emplik <- function(z, # matrix with one data vector per row, a column vector is ok when d=1
                   mu, # hypothesized mean, default (0 ... 0) in R^d
                   lam, # starting lambda, default (0 ... 0)
                   eps, # lower cutoff for -log( ), default 1/nrow(z)
                   M, # upper cutoff for -log( ), default Inf
                   thresh = 1e-30, # convergence threshold for log likelihood (default is aggressive)
                   itermax = 100, # upper bound on number of Newton steps (seems ample)
                   verbose = FALSE) # controls printed output
{
  # empirical likelihood test for whether
  # mean of (rows of) z is mu


  # Internal function mllog, modified -log( ) with derivatives

  mllog <- function(x, eps, M, der = 0) {
    # minus log and its first der derivatives, on  eps < x < M
    # 4th order Taylor approx to left of eps and right of M
    # der = 0 or 1 or 2
    # 4th order is lowest that gives self concordance

    if (missing(M)) {
      M <- Inf
    }
    if (eps > M) {
      stop("Thresholds out of order")
    }

    lo <- x < eps
    hi <- x > M
    md <- (!lo) & (!hi)

    # Coefficients for 4th order Taylor approx below eps
    coefs <- rep(0, 5)
    coefs[1] <- -log(eps)
    coefs[2:5] <- (-eps)^-(1:4) / (1:4)

    # Coefficients for 4th order Taylor approx above M
    Coefs <- rep(0, 5)
    Coefs[1] <- -log(M)
    Coefs[2:5] <- (-M)^-(1:4) / (1:4)

    # degree 4 polynomial approx to log
    h <- function(y, cvals) { # cvals are coefs at eps, Coefs at M
      # sum c[t+1] y^t
      tee <- 1:4
      ans <- y * 0
      ans <- ans + cvals[1]
      for (j in tee) {
        ans <- ans + y^j * cvals[j + 1]
      }
      ans
    }

    # first derivative of h at y, from approx at pt
    hp <- function(y, pt) {
      tee <- 0:3
      ans <- y * 0
      for (j in tee) {
        ans <- ans + (-y / pt)^j
      }
      ans <- ans * (-pt)^-1
      ans
    }

    # second derivative of h at y, from approx at pt
    hpp <- function(y, pt) {
      tee <- 0:2
      ans <- y * 0
      for (j in tee) {
        ans <- ans + (j + 1) * (-y / pt)^j
      }
      ans <- ans * (-pt)^-2
      ans
    }

    # function value
    f <- x * 0
    f[lo] <- h(x[lo] - eps, coefs)
    f[hi] <- h(x[hi] - M, Coefs)
    f[md] <- -log(x[md])

    if (der < 1) {
      return(cbind(f))
    }

    # first derivative
    fp <- x * 0
    fp[lo] <- hp(x[lo] - eps, eps)
    fp[hi] <- hp(x[hi] - M, M)
    fp[md] <- -1 / x[md]

    if (der < 2) {
      return(cbind(f, fp))
    }

    # second derivative
    fpp <- x * 0
    fpp[lo] <- hpp(x[lo] - eps, eps)
    fpp[hi] <- hpp(x[hi] - M, M)
    fpp[md] <- 1 / x[md]^2

    return(cbind(f, fp, fpp))
    # End of mllog()
  }

  # Internal function to do a linear model via SVD
  # Empirical likelihood's Newton steps are of
  # least squares type.

  svdlm <- function(X, y) {
    # Linear model regression coefficient via SVD

    # Tolerances for generalized inverse via SVD
    RELTOL <- 1e-9
    ABSTOL <- 1e-100

    # Get Xplus = generalized inverse of X
    # If svd algorithm failures are encountered
    # it sometimes helps to try svd(t(X)) and
    # translate back. First check to ensure that
    # X does not contain NaN or Inf or -Inf.
    svdX <- svd(X)
    d <- svdX$d
    lo <- d < (RELTOL * max(d) + ABSTOL)
    dinv <- 1 / d
    dinv[lo] <- 0
    Xplus <- svdX$v %*% diag(dinv, nrow = length(dinv)) %*% t(svdX$u)
    # taking care with diag when dinv is 1x1
    # to avoid getting the identity matrix of
    # size floor(dinv)

    # Return X^+ y
    Xplus %*% matrix(y, ncol = 1)
  }
  # end of svdlm


  # Backtracking line search parameters [Tweak only with extreme caution.]
  # See Boyd and Vandenberghe, pp464-466.
  ALPHA <- 0.3 # seems better than 0.01 on some 2d test data (sometimes fewer iters)
  BETA <- 0.8
  # We need  0 < ALPHA < 1/2   and 0 < BETA < 1

  # Backtrack threshold: you can miss by this much.
  BACKEPS <- 0
  # Consider replacing 0 by 1e-10 if backtracking seems to be
  # failing due to round off.

  if (is.vector(z)) {
    z <- matrix(z, ncol = 1)
  }

  n <- nrow(z)
  d <- ncol(z)

  if (missing(mu)) {
    mu <- rep(0, d)
  }
  z <- t(t(z) - mu) # subtract mu from each z[i,]

  if (missing(eps)) eps <- 1 / n
  if (missing(M)) M <- Inf

  #
  # Use lam = 0 or initial lam, whichever is best
  #

  init0 <- mllog(rep(1, n), eps = eps, M = M, der = 2) # i.e. lam = 0

  if (missing(lam)) {
    init <- init0
    lam <- rep(0, d)
  } else {
    init <- mllog(1 + z %*% lam, eps = eps, M = M, der = 2)
    if (sum(init0[, 1]) < sum(init[, 1])) {
      lam <- rep(0, d)
      init <- init0
    }
  }

  # Initial f, g
  fold <- sum(init[, 1])
  gold <- apply(z * init[, 2], 2, sum)

  converged <- FALSE
  iter <- 0
  oldvals <- init
  while (!converged) {
    iter <- iter + 1

    # Get Newton Step
    rootllpp <- sqrt(oldvals[, 3]) # sqrt 2nd deriv of -llog lik
    zt <- z
    for (j in 1:d) {
      zt[, j] <- zt[, j] * rootllpp
    }
    yt <- oldvals[, 2] / rootllpp
    step <- -svdlm(zt, yt) #  more reliable than step = -lm( yt~zt-1 )$coef

    backtrack <- FALSE
    s <- 1 # usually called t, but R uses t for transpose
    while (!backtrack) {
      newvals <- mllog(1 + z %*% (lam + s * step), eps = eps, M = M, der = 2)
      fnew <- sum(newvals[, 1])
      targ <- fold + ALPHA * s * sum(gold * step) + BACKEPS # (BACKEPS for roundoff, should not be needed)
      if (fnew <= targ) {
        # backtracking has converged
        backtrack <- TRUE
        oldvals <- newvals
        fold <- fnew
        gold <- apply(z * oldvals[, 2], 2, sum)
        # take the step
        lam <- lam + s * step
      } else {
        s <- s * BETA
      }
    }

    # Newton decrement and gradient norm
    ndec <- sqrt(sum((step * gold)^2))
    gradnorm <- sqrt(sum(gold^2))

    if (verbose) print(c(fold, gradnorm, ndec, lam))

    converged <- (ndec^2 <= thresh)
    if (iter > itermax) break
  }

  wts <- (1 / n) / (1 + z %*% lam)
  logelr <- sum(mllog(1 + z %*% lam, eps = eps, M = M, der = 0))

  list(
    logelr = logelr, lam = lam, wts = wts,
    converged = converged, iter = iter, ndec = ndec, gradnorm = gradnorm
  )
}

set.seed(1)
x <- rgamma(25, 1, 2)
test <- empirical_mu_one_sample(x = x, mu = .5, alternative = "two.sided")
test_02 <- emplik(z = x, mu = .5)

test_that("check statistic", {
  expect_equal(test$statistic, -2 * test_02$logelr)
})

rm(x, test, test_02)

set.seed(1)
x <- rnorm(25, 0, 1)
test <- empirical_mu_one_sample(x = x, mu = 0, alternative = "two.sided")
test_02 <- emplik(z = x, mu = 0)

test_that("check statistic", {
  expect_equal(test$statistic, -2 * test_02$logelr)
})
rm(x, test, test_02, emplik)


###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_mu_one_sample(c()), "Argument x should have positive length.")
  expect_error(empirical_mu_one_sample(rep("foo", 50)), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(50)
test_that("mu input checking works", {
  expect_error(empirical_mu_one_sample(x, c(1, 2)), "The tested parameter should have length one.")
  expect_error(empirical_mu_one_sample(x, "foo"), "The tested parameter should be numeric.")
  expect_error(empirical_mu_one_sample(x, min(x)), "The tested parameter must be greater than the min of x.")
  expect_error(empirical_mu_one_sample(x, max(x)), "The tested parameter must be less than the max of x.")
})

test_that("alternative input checking works", {
  expect_error(empirical_mu_one_sample(x, 0, c("two.sided", "less")), "Argument alternative should have length one.")
  expect_error(empirical_mu_one_sample(x, 0, 1), "Argument alternative should be a character.")
  expect_error(empirical_mu_one_sample(x, 0, "lesss"), "Argument alternative should be 'two.sided', 'less', or 'greater.")
})

test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_sample(x, 1, "less", c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_mu_one_sample(x, 1, "less", "foo"), "conf.level should be numeric.")
  expect_error(empirical_mu_one_sample(x, 1, "less", 0), "conf.level should between zero and one.")
  expect_error(empirical_mu_one_sample(x, 1, "less", 1), "conf.level should between zero and one.")
})

###############################################
# Null True
###############################################
# Owen 1991 lists out stat
# Equation 6.1 and 6.2.
# Only left hand side of 6.1 so testing for approximate match. Not exact.
set.seed(1)
x <- rnorm(150, 1, 1)
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_mu_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

dat <- data.frame(fctr = fctr, x = x)
model_00 <- lm(x ~ 1, data = dat)
model_01 <- lm(x ~ fctr, data = dat)

test_02 <- lmtest::lrtest(model_00, model_01)
test_that("Check contents", {
  expect_true(test$p.value > .05)
  expect_true(test$statistic >= 0)
  expect_true(abs(test$p.value - test_02[["Pr(>Chisq)"]][2]) < .02)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_mu_one_sample(x[which(fctr == 1)], 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2, dat, model_00, model_01)

###############################################
# Null False
###############################################

set.seed(1)
x <- c(rnorm(50, 1.5, 1), rnorm(50, 2, 1), rnorm(50, 2.25, 1))
fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
fctr <- factor(fctr, levels = c("1", "2", "3"))
test <- empirical_mu_one_way(x, fctr, .95)

test_that("Check structure.", {
  expect_true(all(class(test) == c("one_way_case_three", "lrtest")))
  expect_true(length(test) == 6)
  expect_true(all(names(test) == c("statistic", "p.value", "conf.ints", "overall.conf", "individ.conf", "alternative")))
})

dat <- data.frame(fctr = fctr, x = x)
model_00 <- lm(x ~ 1, data = dat)
model_01 <- lm(x ~ fctr, data = dat)

test_02 <- lmtest::lrtest(model_00, model_01)
test_that("Check contents", {
  expect_true(test$p.value < .05)
  expect_true(test$statistic >= 0)
  expect_true(abs(test$p.value - test_02[["Pr(>Chisq)"]][2]) < .02)
})

# make sure CIs match
CI1 <- unname(test$conf.ints[[1]])
CI2 <- empirical_mu_one_sample(x[which(fctr == 1)], 0, test$alternative, test$individ.conf)$conf.int
test_that("Check CI", {
  expect_equal(CI1, CI2)
})
rm(CI1, CI2, dat, model_00, model_01)

###############################################
# Check statistic against publication.
###############################################
calc_reference_stat <- function(x, fctr) {
  si2 <- vector(mode = "numeric", length = length(levels(fctr)))
  means <- vector(mode = "numeric", length = length(levels(fctr)))
  nis <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(si2)) {
    index <- fctr == levels(fctr)[i]
    tempX <- x[index]
    means[i] <- mean(tempX)
    nis[i] <- length(tempX)
    si2[i] <- sum((tempX - means[i])^2) / nis[i]
  }

  numerator <- vector(mode = "numeric", length = length(levels(fctr)))
  denominator <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(numerator)) {
    numerator[i] <- nis[i] * means[i] / si2[i]
    denominator[i] <- nis[i] / si2[i]
  }
  muHat <- sum(numerator) / sum(denominator)

  testStatElements <- vector(mode = "numeric", length = length(levels(fctr)))
  for (i in seq(testStatElements)) {
    testStatElements[i] <- nis[i] * (means[i] - muHat)^2 / si2[i]
  }
  testStat <- sum(testStatElements)
  return(testStat)
}


set.seed(1)
x <- rnorm(300, 1.5, 1)
fctr <- c(rep(1, 150), rep(2, 150))
fctr <- factor(fctr, levels = c("1", "2"))
ts_01 <- empirical_mu_one_way(x, fctr, .95)$statistic
ts_02 <- calc_reference_stat(x, fctr)
test_that("Check statistic", {
  expect_true(abs(ts_01 - ts_02) < .1)
})

set.seed(1)
x <- rgamma(900, 1.5, 1)
fctr <- c(rep(1, 300), rep(2, 300), rep(3, 300))
fctr <- factor(fctr, levels = c("1", "2", "3"))
ts_01 <- empirical_mu_one_way(x, fctr, .95)$statistic
ts_02 <- calc_reference_stat(x, fctr)
test_that("Check statistic", {
  expect_true(abs(ts_01 - ts_02) < .1)
})

rm(x, fctr, ts_01, ts_02, calc_reference_stat)

###############################################
# Check against emplik2
###############################################
library(emplik2)
library(datasets)

x <- c(USJudgeRatings$ORAL, USJudgeRatings$WRIT)
fctr <- c(rep(1, length(USJudgeRatings$ORAL)), rep(2, length(USJudgeRatings$WRIT)))
fctr <- factor(fctr, levels = c("1", "2"))

t1 <- empirical_mu_one_way(x, fctr, .95)
t2 <- el2.cen.EMs(
  x = x[fctr == "1"],
  dx = rep(1, length(x[fctr == "1"])),
  y = x[fctr == "2"],
  dy = rep(1, length(x[fctr == "2"])),
  fun = function(x, y) {
    x - y
  },
  mean = 0,
  maxit = 100
)

test_that("Check statistic", {
  expect_true(abs(t1$statistic - t2$`-2LLR`) < .01)
  expect_true(abs(t1$p.value - t2$Pval) < .01)
})

set.seed(1)
x <- c(rnorm(100, 1, 1), rnorm(100, 1, 1))
fctr <- c(rep(1, 100), rep(2, 100))
fctr <- factor(fctr, levels = c("1", "2"))

t1 <- empirical_mu_one_way(x, fctr, .95)
t2 <- el2.cen.EMs(
  x = x[fctr == "1"],
  dx = rep(1, length(x[fctr == "1"])),
  y = x[fctr == "2"],
  dy = rep(1, length(x[fctr == "2"])),
  fun = function(x, y) {
    x - y
  },
  mean = 0,
  maxit = 100
)

test_that("Check statistic", {
  expect_true(abs(t1$statistic - t2$`-2LLR`) < .01)
  expect_true(abs(t1$p.value - t2$Pval) < .01)
})

###############################################
# Input checking
###############################################
test_that("x input checking works", {
  expect_error(empirical_mu_one_way(c()), "Argument x should have positive length.")
  expect_error(empirical_mu_one_way("foo"), "Argument x should be numeric.")
})

set.seed(1)
x <- rnorm(100)
fctr1 <- factor(rep(1, 100), levels = c("1", "2", "3"))
test_that("fctr input checking works", {
  expect_error(empirical_mu_one_way(x, "foo"), "Argument fctr should have same length as x.")
  expect_error(empirical_mu_one_way(x, rep("foo", 100)), "Argument fctr should be a factor.")
  expect_error(empirical_mu_one_way(x, factor(rep("foo", 100))), "Argument fctr should have at least two unique values.")
  expect_error(empirical_mu_one_way(x, fctr1), "Argument fctr should have at least two unique values.")
})
rm(fctr1)

fctr <- c(rep(1, 50), rep(2, 50))
fctr <- factor(fctr, levels = c("1", "2"))
test_that("conf.level input checking works", {
  expect_error(empirical_mu_one_way(x, fctr, c(.50, .75)), "conf.level should have length one.")
  expect_error(empirical_mu_one_way(x, fctr, "foo"), "conf.level should be numeric.")
  expect_error(empirical_mu_one_way(x, fctr, 0), "conf.level should between zero and one.")
  expect_error(empirical_mu_one_way(x, fctr, 1), "conf.level should between zero and one.")
})
