#' @keywords internal
#' Probabilities under the null for the variance tests. Two constraints. One
#' for the mean (nuisance) and one for the variance. Matches _opt_var in
#' statsmodels' emplike module. The nuisance mean is profiled out. It must
#' stay strictly inside the range of x for the mean constraint to be solvable.
#' The profile can have disjoint feasible regions in mu, so a coarse grid
#' search brackets the minimum before Brent polishes it.
calc_null_p_variance <- function(x, sigma.squared) {
  build_est_vect <- function(mu) {
    est_vect <- cbind(x - mu, (x - mu)^2 - sigma.squared)
    return(est_vect)
  }
  profile_helper <- function(mu) {
    out <- calc_el_solution(build_est_vect(mu))$W
    return(out)
  }
  buffer <- (max(x) - min(x)) * .Machine$double.eps^.5
  mus <- seq(min(x) + buffer, max(x) - buffer, length.out = 21)
  Ws <- vapply(mus, profile_helper, numeric(1))
  i <- which.min(Ws)
  opt <- stats::optim(
    par = mus[i], fn = profile_helper, method = "Brent",
    lower = mus[max(i - 1, 1)], upper = mus[min(i + 1, length(mus))]
  )

  p <- calc_el_solution(build_est_vect(opt$par))$p

  # division by near zero numbers can cause -Inf and Inf
  # underflow
  p <- pmax(p, .Machine$double.eps)
  p <- pmin(p, 1 - .Machine$double.eps)

  return(p)
}

#' @keywords internal
#' One group's -2 log empirical likelihood ratio at a hypothesized value.
#' Used by the one way tests. Not checked here because candidate values
#' during the outer optimization may be infeasible for a group. An infeasible
#' value produces a large W, steering the optimizer away.
calc_group_W <- function(x, value, calc_null_p) {
  obs_p <- rep(1 / length(x), length(x))
  null_p <- calc_null_p(x, value)
  W <- 2 * (sum(log(obs_p)) - sum(log(null_p)))
  W <- pmax(W, 0) # underflow
  return(W)
}

#' @keywords internal
#' Helper to solve the inner empirical likelihood problem for a matrix of
#' estimating equations. Each row of est_vect is an observation. Each column
#' is a constraint. Lambda (one element per constraint) is found by maximizing
#' the log star objective with Newton's method. The log star function extends
#' log below 1/n so the objective is defined for all lambda.
#' Based on chapter 12 of Owen's Empirical Likelihood book and the
#' emplike module of statsmodels.
calc_el_solution <- function(est_vect) {
  n <- nrow(est_vect)

  log_star <- function(z) {
    ifelse(z >= 1 / n, log(pmax(z, 1 / n)), log(1 / n) - 1.5 + 2 * n * z - (n * z)^2 / 2)
  }
  log_star_d1 <- function(z) {
    ifelse(z >= 1 / n, 1 / pmax(z, 1 / n), 2 * n - n^2 * z)
  }
  log_star_d2 <- function(z) {
    ifelse(z >= 1 / n, -1 / pmax(z, 1 / n)^2, -n^2)
  }

  calc_objective <- function(lambda) {
    z <- as.vector(1 + est_vect %*% lambda)
    return(sum(log_star(z)))
  }

  lambda <- rep(0, ncol(est_vect))
  for (i in seq_len(100)) {
    z <- as.vector(1 + est_vect %*% lambda)
    grad <- as.vector(crossprod(est_vect, log_star_d1(z)))
    if (max(abs(grad)) < .Machine$double.eps^.5) {
      break
    }

    neg_hess <- crossprod(est_vect * sqrt(-log_star_d2(z)))
    step <- tryCatch(as.vector(solve(neg_hess, grad)), error = function(e) NULL)
    if (is.null(step)) {
      break
    }

    step_size <- 1
    while (calc_objective(lambda + step_size * step) < calc_objective(lambda) && step_size > .1^4) {
      step_size <- step_size / 2
    }
    lambda <- lambda + step_size * step
  }

  z <- as.vector(1 + est_vect %*% lambda)
  p <- 1 / (n * z)
  W <- 2 * sum(log_star(z))

  return(list(p = p, W = W))
}
