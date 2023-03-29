# fix check.
# Not actually global.
utils::globalVariables(c("x", "alternative", "conf.level", "p", "fctr"))

#' @keywords internal
#' A function factory
#' Function to return a function that performs likelihood ratio test.
#' Main work hourse of one sample tests
create_test_function_one_sample_case_one <- function(calc_test_stat, p0, LB = -Inf) {
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

  exec_globals <- list(LB = LB, calc_test_stat = calc_test_stat, calc_CI = calc_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

# A function factory
# depends on single sample tests for CIs.
create_test_function_one_way_case_one <- function(calc_test_stat, calc_individual_CI) {
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

  # Build function
  args <- rlang::pairlist2(x = , fctr = , conf.level = 0.95)

  body <- rlang::expr({
    if (length(x) < 50) {
      stop("Argument x should have at least 50 data points.")
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
    if (any(as.vector(by(x, fctr, length)) < 50)) {
      stop("Each groups needs to contain at least 50 points for CIs to be accurate.")
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
    for (i in 1:length(levels(fctr))) {
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

  exec_globals <- list(calc_test_stat = calc_test_stat, calc_individual_CI = calc_individual_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}

create_test_function_one_way_case_two <- function(calc_test_stat, calc_individual_CI) {
  force(calc_test_stat)
  force(calc_individual_CI)
  # Confirm function looks right
  if (!inherits(calc_test_stat, "function")) {
    stop("Argument calc_test_stat must be a function.")
  }
  # Confirm function looks right
  if (!inherits(calc_individual_CI, "function")) {
    stop("Argument calc_individual_CI must be a function.")
  }

  args <- names(formals(calc_test_stat))
  args_02 <- names(formals(calc_individual_CI))
  if (args[1] != args_02[1]) {
    stop("calc_test_stat's first argument does not match calc_individual_CI first argument.")
  }
  if (args[2] != args_02[2]) {
    stop("calc_test_stat's second argument does not match calc_individual_CI second argument.")
  }
  if (args[3] != "fctr") {
    stop("calc_test_stat's third argument is not fctr.")
  }
  if (length(args) != 3) {
    stop("calc_test_stat has too many arguments.")
  }
  rm(args, args_02)

  args <- names(formals(calc_individual_CI))
  if (args[4] != "alternative") {
    stop("calc_individual_CI's fourth argument is not alternative.")
  }
  if (args[5] != "conf.level") {
    stop("calc_individual_CI's fifth argument is not conf.level.")
  }
  if (length(args) != 5) {
    stop("calc_individual_CI has too many arguments.")
  }
  rm(args)

  arg1 <- rlang::sym(names(formals(calc_individual_CI))[1])
  arg2 <- rlang::sym(names(formals(calc_individual_CI))[2])
  if (rlang::as_string(arg2) == "n") {
    # binomial case
    sizeCheck <- rlang::expr(sum(!!arg2) < 50)
    rangeCheck <- rlang::expr(
      if (any(!!arg1 > !!arg2)) {
        stop("No values in  x can be larger than values in n.")
      }
    )
  } else if (rlang::as_string(arg2) == "num_successes") {
    # negative binomial case
    sizeCheck <- rlang::expr(sum(!!arg1 + !!arg2) < 50)
    rangeCheck <- rlang::expr(
      if (any(!!arg2 < 1)) {
        stop("There must be at least one success in num_successes per group.")
      }
    )
  } else {
    stop("arg2 was not n or num_successes.")
  }

  # Build function
  args <- rlang::pairlist2(holder1 = , holder2 = , fctr = , conf.level = 0.95)
  for (i in 1:2) {
    names(args)[i] <- names(formals(calc_individual_CI))[i]
  }

  body <- rlang::expr({
    if (length(!!arg1) != length(!!arg2)) {
      stop("The first two arguments should have the same length.")
    }
    if (length(!!arg1) < 1) {
      stop("First argument should have positive length.")
    }
    if (!is.numeric(!!arg1)) {
      stop("First argument should be numeric.")
    }
    if (any(!!arg1 != as.integer(!!arg1))) {
      stop("First argument should only contain integers.")
    }
    if (any(!!arg1 < 0)) {
      stop("All elements in first argument should be 0 or above.")
    }
    if (length(!!arg2) < 1) {
      stop("Second argument should have positive length.")
    }
    if (!is.numeric(!!arg2)) {
      stop("Second argument should be numeric.")
    }
    if (any(!!arg2 != as.integer(!!arg2))) {
      stop("Second argument should only contain integers.")
    }
    if (any(!!arg2 < 0)) {
      stop("All elements in second argument should be 0 or above.")
    }
    if (!!sizeCheck) {
      stop("At least 50 trials should be done for likelihood ratio test.")
    }
    !!rangeCheck
    if (length(fctr) != length(!!arg1)) {
      stop("Argument fctr should have same length as first argument.")
    }
    if (!is.factor(fctr)) {
      stop("Argument fctr should be a factor.")
    }
    if (length(base::unique(fctr)) < 2) {
      stop("Argument fctr should have at least two unique values.")
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

    W <- calc_test_stat(!!arg1, !!arg2, fctr)

    # Under null, 1 parameter (overall value) is allowed to vary
    # Under alternative, parameter for each group is allowed to vary
    df <- length(levels(fctr)) - 1

    p.value <- stats::pchisq(q = W, df = df, lower.tail = FALSE)

    # Bonferroni correction and convert back to confidence
    alpha <- 1 - conf.level
    alpha <- alpha / length(levels(fctr))
    individual.conf.level <- 1 - alpha

    CI <- list()
    for (i in 1:length(levels(fctr))) {
      l <- levels(fctr)[i]
      index <- which(fctr == l)
      tempOne <- !!arg1
      tempOne <- tempOne[index]
      tempTwo <- !!arg2
      tempTwo <- tempTwo[index]
      tempCI <- calc_individual_CI(tempOne, tempTwo, .5, "two.sided", individual.conf.level)
      tempCI <- tempCI$conf.int
      CI[[l]] <- tempCI
    }

    out <- list(statistic = W, p.value = p.value, conf.ints = CI, overall.conf = conf.level, individ.conf = individual.conf.level, alternative = "two.sided")
    class(out) <- c("one_way_case_two", "lrtest")
    return(out)
  })

  exec_globals <- list(calc_test_stat = calc_test_stat, calc_individual_CI = calc_individual_CI)
  exec_env <- rlang::new_environment(data = exec_globals, parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}