#' Expand Count Data into Binary Vectors
#'
#' Converts count data (number of successes and total sample size) into a binary vector
#' of TRUE/FALSE values. This is useful for converting summary statistics back into
#' raw data format for analysis functions that require individual-level data.
#'
#' @param x Integer (or vector of integers) representing the number of successes.
#' @param n Integer (or vector of integers) representing the total number of participants.
#'
#' @return A logical vector where TRUE represents a success and FALSE represents a failure.
#'   The length of the vector equals the sum of all sample sizes.
#'
#' @details
#' For each pair of values in `x` and `n`, the function creates a vector with `x` TRUE values
#' followed by `n-x` FALSE values. If multiple pairs are provided, the resulting vectors are
#' concatenated in order.
#'
#' @examples
#' # Convert 4 successes out of 13 participants to binary data
#' expand(4, 13)
#'
#' # Convert multiple groups of data
#' # Group 1: 9 successes out of 10
#' # Group 2: 3 successes out of 10
#' expand(c(9, 3), c(10, 10))
#'
#' @export
expand <- function(x, n){
  # check inputs ---------------------------------------------------------------
  check_integerish(x)
  check_integerish(n)
  check_range(x, range = c(0, Inf), include_bounds = c(TRUE, FALSE))
  check_range(n, range = c(0, Inf), include_bounds = c(TRUE, FALSE))
  check_identical_length(x, n)
  if(any(x > n)){
    cli::cli_abort("{.arg x} must be smaller than {.arg n}")
  }

  purrr::map2(x, n, \(x1, n1){
    c(rep(TRUE, times = x1), rep(FALSE, times = n1-x1))
  }
  ) |>
    purrr::reduce(c)
}


#' To get the n's and response totals with out without strata
#' @keywords internal
#' @noMd
get_counts <- function(x, by, strata = 1) {
  dplyr::tibble(
    x = x,
    by = as.numeric(forcats::as_factor(by)),
    strata = strata
  ) |>
    dplyr::group_by(by, strata) |>
    dplyr::summarise(n = dplyr::n(),
                     response = sum(x)) |>
    tidyr::complete(strata, fill = list("n" = 0, "response" = 0)) |>
    tidyr::pivot_wider(names_from = "by", values_from = c("n", "response"))

}


#' Function to combine strata via interaction if strata is passed as a vector
#' @keywords internal
#' @noMd
combine_strata <- function(x, strata){
  if(length(strata) %% length(x) != 0){
    cli::cli_abort("The length {.arg strata} must divisable by the length {.arg x}")
  }
  factor <- length(strata) / length(x)
  split(strata, rep(1:factor, each = length(x))) |>
    interaction()
}

#' @export
print.prop_ci_uni <- function(x, ...){
  cli::cli_h1(x$method)
  cli::cli_li("{x$n} response{?s} out of {x$N}")
  cli::cli_li("Estimate: {x$estimate}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
}


#' @export
print.stratified_wilson <- function(x, ...){
  if(!is.null(names(x$weights))){
    name_str <- paste0(names(x$weights), " = ")
  } else {
    name_str <- ""
  }
  weight_str <- paste0(name_str, round(x$weights, 3), collapse = ", ")
  cli::cli_h1(x$method)
  cli::cli_li("{x$n} response{?s} out of {x$N}")
  cli::cli_li("Weights: {weight_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
}

#' @export
print.prop_ci_bi <- function(x, ...){
  diff_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{diff_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  if(!is.null(x$delta)){
    cli::cli_h3("Delta")
    dplyr::tibble(d = x$delta,
                  s = x$statistic,
                  p = x$p.value) |>
      purrr::pmap(\(d,s,p){
        cli::cli_li("At {d} the statistic is {round(s, 3)} and the p-value is {round(p, 4)}")
      })
  }
  invisible(x)
}


#' @export
print.stratified_miettinen_nurminen <- function(x, ...){

  if(!is.null(names(x$weights))){
    name_str <- paste0(names(x$weights), " = ")
  } else {
    name_str <- ""
  }

  weight_str <- paste0(name_str, round(x$weights, 3), collapse = ", ")

  diff_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{diff_str}")
  cli::cli_li("Weights: {weight_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  if(!is.null(x$delta)){
    cli::cli_h3("Delta")
    dplyr::tibble(d = x$delta,
                  s = x$statistic,
                  p = x$p.value) |>
      purrr::pmap(\(d,s,p){
        cli::cli_li("At {d} the statistic is {round(s, 3)} and the p-value is {round(p, 4)}")
      })
  }
  invisible(x)
}


#' @export
print.ci_prop_diff_mh_strata <- function(x, ...){

  diff_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{diff_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("Variance: {round(x$variance, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  invisible(x)
}

#' @export
print.ci_rel_risk_cmh_strata <- function(x, ...){
  diff_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{diff_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("Variance: {round(x$variance, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("\u00a0\u00a0({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  invisible(x)
}
