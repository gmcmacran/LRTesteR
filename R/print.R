#' Print results of tests.
#'
#' @param x a test from LRTesteR.
#' @param ... arguments passed to other methods.
#' @examples
#' library(LRTesteR)
#'
#' set.seed(1)
#' x <- rnorm(100, 0, 1)
#' test <- gaussian_mu_one_sample(x, 0, "two.sided")
#' print(test)
#'
#' set.seed(1)
#' x <- rnorm(150, 1, 1)
#' fctr <- c(rep(1, 50), rep(2, 50), rep(3, 50))
#' fctr <- factor(fctr, levels = c("1", "2", "3"))
#' test <- gaussian_mu_one_way(x, fctr, .95)
#' print(test)
#' @export
print.lrtest <- function(x, ...) {
  cat(paste("Log Likelihood Statistic:", round(x$statistic, 2), sep = " "))
  cat("\n")

  cat(paste("p value:", round(x$p.value, 3), sep = " "))
  cat("\n")
  if (class(x)[1] %in% c("one_sample_case_one", "one_sample_case_two", "one_sample_case_three", "one_sample_case_four")) {
    cat(paste("Confidence Level: ", round(x[["conf.level"]][1], 3) * 100, "%", sep = ""))
    cat("\n")

    cat(paste("Confidence Interval: (", round(x[["conf.int"]][1], 3), ", ", round(x[["conf.int"]][2], 3), ")", sep = ""))
    cat("\n")
  } else {
    CIs <- x$conf.ints
    cat(paste("Confidence Level Of Set: ", round(x[["overall.conf"]][1], 3) * 100, "%", sep = ""))
    cat("\n")

    cat(paste("Individual Confidence Level: ", round(x[["individ.conf"]][1], 3) * 100, "%", sep = ""))
    cat("\n")
    for (i in seq(1, length(CIs), 1)) {
      cat(paste("Confidence Interval For Group ", names(CIs)[i], ": (", round(CIs[[i]][1], 3), ", ", round(CIs[[i]][2], 3), ")", sep = ""))
      cat("\n")
    }
  }

  invisible(x)
}
