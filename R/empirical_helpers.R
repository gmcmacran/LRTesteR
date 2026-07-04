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
