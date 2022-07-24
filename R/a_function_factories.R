# fix check.
# Not actually global.
utils::globalVariables(c("x", "alternative", "conf.level", "p"))

#' @keywords internal
#' A function factory
#' Function to return a function that performs likelihood ratio test.
create_test_function_continuous <- function(calc_test_stat, p0, LB = -Inf) {
  force(calc_test_stat)
  p0 <- rlang::ensym(p0)
  force(LB)

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
  rm(args)

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
      out <- stats::uniroot(helper, lower = pmax(-9999999, LB + .Machine$double.eps), upper = 9999999, tol = .Machine$double.eps^.50)$root
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(x, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      out <- stats::uniroot(helper, lower = pmax(-9999999, LB + 10 * .Machine$double.eps), upper = 9999999, tol = .Machine$double.eps^.50)$root
      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
    }
    else if (alternative == "less") {
      CI <- c(LB, calc_right_side_CI(alpha))
    }
    else {
      CI <- c(calc_left_side_CI(alpha), Inf)
    }

    return(CI)
  }

  # Build function
  args <- rlang::pairlist2(x = , holder = , alternative = "two.sided", conf.level = 0.95)
  names(args)[2] <- rlang::as_string(p0)

  body <- rlang::expr({
    if (length(x) < 50) {
      stop("Argument x should have at least 50 data points.")
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
    if (conf.level <= 0 | conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(x, !!p0, alternative)

    # calculate p value
    if (alternative == "two.sided") {
      p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
    }
    else if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    }
    else {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }

    CI <- calc_CI(x, alternative, conf.level)

    out <- list(statistic = W, p.value = p.value, conf.int = CI, alternative = alternative)
    class(out) <- "lrtest"
    return(out)
  })

  exec_globals <- list(LB = LB, calc_test_stat = calc_test_stat, calc_CI = calc_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

#' @keywords internal
#' A function factory
#' Function to return a function that performs likelihood ratio test.
create_test_function_discrete <- function(calc_MLE, calc_test_stat, arg1, arg2) {
  arg1 <- rlang::ensym(arg1)
  arg2 <- rlang::ensym(arg2)

  # Confirm function looks right
  if (!inherits(calc_MLE, "function")) {
    stop("Argument calc_MLE must be a function.")
  }
  args <- names(formals((calc_MLE)))
  if (args[1] != "arg1") {
    stop("calc_MLE's first argument is not arg1.")
  }
  if (args[2] != "arg2") {
    stop("calc_MLE's second argument is not arg2.")
  }
  rm(args)

  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  args <- names(formals(calc_test_stat))
  if (args[1] != "arg1") {
    stop("calc_test_stat's first argument is not arg1.")
  }
  if (args[2] != "arg2") {
    stop("calc_test_stat's second argument is not arg2.")
  }
  if (args[3] != "p") {
    stop("calc_test_stat's third argument is not p.")
  }
  if (args[4] != "alternative") {
    stop("calc_test_stat's fourth argument is not alternative.")
  }
  rm(args)

  force(calc_MLE)
  force(calc_test_stat)

  LB <- 0
  UB <- 1

  if (rlang::as_string(arg2) == "n") {
    # binomial case
    sizeCheck <- rlang::expr(!!arg2 < 50)
    rangeCheck <- rlang::expr(
      if (!!arg1 > !!arg2) {
        stop("Argument x cannot be larger than n.")
      }
    )
  } else if (rlang::as_string(arg2) == "num_success") {
    # negative binomial case
    sizeCheck <- rlang::expr(!!arg1 + !!arg2 < 50)
    rangeCheck <- rlang::expr(
      if (!!arg2 <= 1) {
        stop("There must be at least one success.")
      }
    )
  }


  calc_CI <- function(arg1, arg2, alternative, conf.level) {
    alpha <- 1 - conf.level
    ops_p <- calc_MLE(arg1, arg2)

    calc_left_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(arg1, arg2, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = FALSE)
        return(out)
      }
      searchLB <- LB + 10 * .Machine$double.eps
      searchUB <- UB - 10 * .Machine$double.eps
      out <- stats::uniroot(helper, lower = searchLB, upper = searchUB, tol = .Machine$double.eps^.50)$root
      return(out)
    }
    calc_right_side_CI <- function(alpha) {
      helper <- function(param) {
        W <- calc_test_stat(arg1, arg2, param, "less")
        out <- W - stats::qnorm(p = alpha, lower.tail = TRUE)
        return(out)
      }
      searchLB <- LB + 10 * .Machine$double.eps
      searchUB <- UB - 10 * .Machine$double.eps
      out <- stats::uniroot(helper, lower = searchLB, upper = searchUB, tol = .Machine$double.eps^.50)$root
      return(out)
    }

    if (alternative == "two.sided") {
      alpha <- alpha / 2
      # deal with edge case of MLE on boundary
      if (ops_p == 1) {
        CI <- c(calc_left_side_CI(alpha), UB)
      } else if (ops_p == 0) {
        CI <- c(LB, calc_right_side_CI(alpha))
      } else {
        CI <- c(calc_left_side_CI(alpha), calc_right_side_CI(alpha))
      }
    }
    else if (alternative == "less") {
      if (ops_p == 1) {
        CI <- c(calc_left_side_CI(alpha), UB)
      } else {
        CI <- c(LB, calc_right_side_CI(alpha))
      }
    }
    else {
      if (ops_p == 0) {
        CI <- c(LB, calc_right_side_CI(alpha))
      } else {
        CI <- c(calc_left_side_CI(alpha), UB)
      }
    }

    return(CI)
  }

  # Build function
  args <- rlang::pairlist2(holder1 = , holder2 = , p = , alternative = "two.sided", conf.level = 0.95)
  names(args)[1] <- rlang::as_string(arg1)
  names(args)[2] <- rlang::as_string(arg2)

  body <- rlang::expr({
    if (length(!!arg1) != 1) {
      stop("First argument should have length 1.")
    }
    if (!is.numeric(!!arg1)) {
      stop("First argument should be numeric.")
    }
    if (!!arg1 != as.integer(!!arg1)) {
      stop("First argument should be an integer.")
    }
    if (!!arg1 < 0) {
      stop("First argument should be 0 or above.")
    }
    if (length(!!arg2) != 1) {
      stop("Second argument should have length 1.")
    }
    if (!is.numeric(!!arg2)) {
      stop("Second argument should be numeric.")
    }
    if (!!arg2 != as.integer(!!arg2)) {
      stop("Second argument should be an integer.")
    }
    if (!!arg2 < 0) {
      stop("Second argument should be 0 or above.")
    }
    if (!!sizeCheck) {
      stop("At least 50 trials should be done for likelihood ratio test.")
    }
    !!rangeCheck

    if (!is.numeric(p)) {
      stop("Argument p should be numeric.")
    }
    if (length(p) != 1) {
      stop("Argument p should have length one.")
    }
    if (p < 0 | p > 1) {
      stop("Argument p should be between 0 and 1.")
    }
    if (length(alternative) != 1) {
      stop("Argument alternative should have length one.")
    }
    if (!is.character(alternative)) {
      stop("Argument alternative should be a character.")
    }
    if (!(alternative %in% c("two.sided", "less", "greater"))) {
      stop("Argument alternative should be 'two.sided', 'less', or 'greater'")
    }
    if (length(conf.level) != 1) {
      stop("conf.level should have length one.")
    }
    if (!is.numeric(conf.level)) {
      stop("conf.level should be numeric.")
    }
    if (conf.level <= 0 | conf.level >= 1) {
      stop("conf.level should between zero and one.")
    }

    W <- calc_test_stat(!!arg1, !!arg2, p, alternative)

    # calculate p value
    if (alternative == "two.sided") {
      p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
    }
    else if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    }
    else {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }

    CI <- calc_CI(!!arg1, !!arg2, alternative, conf.level)

    out <- list(statistic = W, p.value = p.value, conf.int = CI, alternative = alternative)
    class(out) <- "lrtest"
    return(out)
  })

  exec_globals <- list(LB = LB, UB = UB, calc_MLE = calc_MLE, calc_test_stat = calc_test_stat, calc_CI = calc_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
