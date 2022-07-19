# fix check.
# Not actually global.
utils::globalVariables(c("x", "alternative"))

#' @keywords internal
#' A function factory
#' Function to return a function that performs likelihood ratio test.
create_test_function_continuous <- function(calc_test_stat, p0, LB = -Inf) {
  force(calc_test_stat)
  p0 <- rlang::ensym(p0)
  force(LB)

  # Confirm function looks right
  if (class(calc_test_stat) != "function") {
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

  # Build function
  args <- rlang::pairlist2(x = , holder = , alternative = "two.sided")
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

    W <- calc_test_stat(x, !!p0, alternative)

    if (alternative == "two.sided") {
      p.value <- stats::pchisq(q = W, df = 1, lower.tail = FALSE)
    }
    else if (alternative == "less") {
      p.value <- stats::pnorm(q = W, lower.tail = TRUE)
    }
    else {
      p.value <- stats::pnorm(q = W, lower.tail = FALSE)
    }

    out <- list(statistic = W, p.value = p.value, alternative = alternative)
    class(out) <- "lrtest"
    return(out)
  })

  exec_env <- rlang::new_environment(data = list(LB = LB, calc_test_stat = calc_test_stat), parent = rlang::base_env())

  f <- rlang::new_function(args, body, env = exec_env)

  return(f)
}
