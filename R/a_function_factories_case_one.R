# fix check.
# Not actually global.
utils::globalVariables(c("x", "alternative", "conf.level", "p", "fctr"))

#' @keywords internal
#' A function factory
#' Function to return a function that performs likelihood ratio test.
#' Main work hourse of one sample tests
create_test_function_one_sample_case_one <- function(calc_test_stat, p0, n_min, LB = -Inf) {
  p0 <- rlang::ensym(p0)
  force(LB)

  force(calc_test_stat)
  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  args <- names(formals(calc_test_stat))
  if (args[1] != "x") {
    stop("calc_test_stat's first argument is not x.")
  }
  if (args[2] != rlang::as_string(p0)) {
    stop("calc_test_stat's second argument is does not match p0.")
  }
  if (args[3] != "alternative") {
    stop("calc_test_stat's third argument is not alternative.")
  }
  if (length(args) != 3) {
    stop("calc_test_stat has too many arguments.")
  }
  rm(args)

  force(n_min)
  if (length(n_min) != 1) {
    stop("n_min should have length one.")
  }
  if (!is.numeric(n_min)) {
    stop("n_min should be numeric.")
  }
  if (n_min <= 1) {
    stop("n_min should be greater than one.")
  }

  if (length(LB) != 1) {
    stop("LB should have length one.")
  }
  if (!is.numeric(LB)) {
    stop("LB should be numeric.")
  }

  calc_CI <- function(x, alternative, conf.level) {
    alpha <- 1 - conf.level

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      side_one <- helper(pmax(-9999999, LB + 20 * .Machine$double.eps))
      side_two <- helper(9999999)
      if (sign(side_one) != sign(side_two)) {
        out <- stats::uniroot(helper, lower = pmax(-9999999, LB + 20 * .Machine$double.eps), upper = 9999999, tol = .Machine$double.eps^.50)$root
      } else {
        out <- NA_real_
      }
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      side_one <- helper(pmax(-9999999, LB + 20 * .Machine$double.eps))
      side_two <- helper(9999999)
      if (sign(side_one) != sign(side_two)) {
        out <- stats::uniroot(helper, lower = pmax(-9999999, LB + 20 * .Machine$double.eps), upper = 9999999, tol = .Machine$double.eps^.50)$root
      } else {
        out <- NA_real_
      }
      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
    } else if (alternative == "less") {
      CI <- c(LB, calc_right_side_CI(alpha))
    } else {
      CI <- c(calc_left_side_CI(alpha), Inf)
    }

    return(CI)
  }

  # Build function
  args <- rlang::pairlist2(x = , holder = , alternative = "two.sided", conf.level = 0.95)
  names(args)[2] <- rlang::as_string(p0)

  body <- rlang::expr({
    if (length(x) < n_min) {
      msg <- stringr::str_c("Argument x should have at least ", n_min, " data points.")
      stop(msg)
    }
    if (!is.numeric(x)) {
      stop("Argument x should be numeric.")
    }
    if (length(!!p0) != 1) {
      stop("The tested parameter should have length one.")
    }
    if (!is.numeric(!!p0)) {
      stop("The tested parameter should be numeric.")
    }
    if (!!p0 <= LB) {
      stop(paste("The tested parameter should be above ", LB, ".", sep = ""))
    }
    if (length(alternative) != 1) {
      stop("Argument alternative should have length one.")
    }
    if (!is.character(alternative)) {
      stop("Argument alternative should be a character.")
    }
    if (!(alternative %in% c("two.sided", "less", "greater"))) {
      stop("Argument alternative should be 'two.sided', 'less', or 'greater.'")
    }
    if (length(conf.level) != 1) {
      stop("conf.level should have length one.")
    }
    if (!is.numeric(conf.level)) {
      stop("conf.level should be numeric.")
    }
    if (conf.level <= 0 || conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(x, !!p0, alternative)

    # calculate p value
    if (alternative == "two.sided") {
      p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
    } else if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    } else {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }

    CI <- calc_CI(x, alternative, conf.level)

    out <- list(statistic = W, p.value = p.value, conf.int = CI, conf.level = conf.level, alternative = alternative)
    class(out) <- c("one_sample_case_one", "lrtest")
    return(out)
  })

  exec_globals <- list(LB = LB, n_min = n_min, calc_test_stat = calc_test_stat, calc_CI = calc_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

# A function factory
# depends on single sample tests for CIs.
create_test_function_one_way_case_one <- function(calc_test_stat, calc_individual_CI, n_min) {
  force(calc_test_stat)
  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  args <- names(formals(calc_test_stat))
  if (args[1] != "x") {
    stop("calc_test_stat's first argument is not x.")
  }
  if (args[2] != "fctr") {
    stop("calc_test_stat's second argument is not fctr.")
  }
  if (length(args) != 2) {
    stop("calc_test_stat has too many arguments.")
  }
  rm(args)

  force(calc_individual_CI)
  # Confirm function looks right
  if (!inherits(calc_individual_CI, "function")) {
    stop("Argument calc_individual_CI must be a function.")
  }
  args <- names(formals(calc_individual_CI))
  if (args[1] != "x") {
    stop("calc_individual_CI's first argument is not x.")
  }
  if (args[3] != "alternative") {
    stop("calc_individual_CI's third argument is not alternative.")
  }
  if (args[4] != "conf.level") {
    stop("calc_individual_CI's fourth argument is not conf.level.")
  }
  if (length(args) != 4) {
    stop("calc_individual_CI has too many arguments.")
  }
  rm(args)

  force(n_min)
  if (length(n_min) != 1) {
    stop("n_min should have length one.")
  }
  if (!is.numeric(n_min)) {
    stop("n_min should be numeric.")
  }
  if (n_min <= 3) {
    stop("n_min should be greater than three.")
  }

  # Build function
  args <- rlang::pairlist2(x = , fctr = , conf.level = 0.95)

  body <- rlang::expr({
    if (length(x) < n_min) {
      msg <- stringr::str_c("Argument x should have at least ", n_min, " data points.")
      stop(msg)
    }
    if (!is.numeric(x)) {
      stop("Argument x should be numeric.")
    }
    if (length(fctr) != length(x)) {
      stop("Argument fctr should have same length as x.")
    }
    if (!is.factor(fctr)) {
      stop("Argument fctr should be a factor.")
    }
    if (length(base::unique(fctr)) < 2) {
      stop("Argument fctr should have at least two unique values.")
    }
    if (length(unique(fctr)) != length(unique(levels(fctr)))) {
      stop("Each level in fctr needs to be present.")
    }
    if (any(as.vector(by(x, fctr, length)) < 2)) {
      msg <- stringr::str_c("Each groups needs to contain at least ", n_min / 2, " data points for CIs to be accurate.")
      stop(msg)
    }
    if (length(conf.level) != 1) {
      stop("conf.level should have length one.")
    }
    if (!is.numeric(conf.level)) {
      stop("conf.level should be numeric.")
    }
    if (conf.level <= 0 || conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(x, fctr)

    # Under null, 1 parameter (overall value) is allowed to vary
    # Under alternative, parameter for each group is allowed to vary
    df <- length(levels(fctr)) - 1

    p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

    # Bonferroni correction and convert back to confidence
    alpha <- 1 - conf.level
    alpha <- alpha / length(levels(fctr))
    individual.conf.level <- 1 - alpha

    CI <- list()
    for (i in seq_along(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempX <- x[index]
      tempCI <- calc_individual_CI(tempX, 1, "two.sided", individual.conf.level)
      tempCI <- tempCI$conf.int
      CI[[l]] <- tempCI
    }

    out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
    class(out) <- c("one_way_case_one", "lrtest")
    return(out)
  })

  exec_globals <- list(calc_test_stat = calc_test_stat, calc_individual_CI = calc_individual_CI, n_min = n_min)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
