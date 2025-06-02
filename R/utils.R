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
    by = as.numeric(as.factor(by)),
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
