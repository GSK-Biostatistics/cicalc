
#' Wald CI
#'
#' Calculates the Wald interval by following the usual textbook definition
#'   for a single proportion confidence interval using the normal approximation.
#'
#' \deqn{\hat{p} \pm z_{\alpha/2} \sqrt{\frac{\hat{p}(1 - \hat{p})}{n}}}
#'
#' @param x (`binary`/`numeric`/`logical`)\cr
#'   vector of a binary values, i.e. a logical vector, or numeric with values `c(0, 1)`
#' @param conf.level (`scalar numeric`) \cr a scalar in (0,1) indicating the confidence level. Default is 0.95
#' @param correct (`logical`)\cr apply continuity correction.
#' @param data (`data.frame`) \cr Optional data frame containing the variables specified in `x` and `by`.
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
#'
#' @export
#' @examples
#' # example code
#'x <- c(
#' TRUE, TRUE, TRUE, TRUE, TRUE,
#' FALSE, FALSE, FALSE, FALSE, FALSE
#' )
#'
#' ci_prop_wald(x, conf.level = 0.9)
#'
ci_prop_wald <- function(x, conf.level = 0.95, correct = FALSE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_wald(
        x = x ,
        conf.level = conf.level,
        correct = correct
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)
  check_class(x = correct, "logical")
  check_scalar(correct)

  x <- stats::na.omit(x)

  n <- length(x)
  p_hat <- mean(x)
  z <- stats::qnorm((1 + conf.level) / 2)
  q_hat <- 1 - p_hat
  correction_factor <- ifelse(correct, 1 / (2 * n), 0)

  err <- z * sqrt(p_hat * q_hat) / sqrt(n) + correction_factor
  l_ci <- max(0, p_hat - err)
  u_ci <- min(1, p_hat + err)

  structure(
    list(
      N = n,
      n = sum(x),
      estimate = p_hat,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Wald Confidence Interval {ifelse(correct, 'with', 'without')} Continuity Correction")
    ),
    class = c("wald", "prop_ci_uni", "cicada")
  )
}


#' Wilson CI
#'
#' Calculates the Wilson interval by calling [stats::prop.test()].
#'  Also referred to as Wilson score interval.
#'
#'
#' \deqn{\frac{\hat{p} +
#' \frac{z^2_{\alpha/2}}{2n} \pm z_{\alpha/2} \sqrt{\frac{\hat{p}(1 - \hat{p})}{n} +
#' \frac{z^2_{\alpha/2}}{4n^2}}}{1 + \frac{z^2_{\alpha/2}}{n}}}
#' @inheritParams ci_prop_wald
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
#'
#' @export
ci_prop_wilson <- function(x, conf.level = 0.95, correct = FALSE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "broom")

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_wilson(
        x = x ,
        conf.level = conf.level,
        correct = correct
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_class(x = correct, "logical")
  check_scalar(correct)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)

  x <- stats::na.omit(x)

  n <- length(x)
  y <- stats::prop.test(x = sum(x), n = n, correct = correct, conf.level = conf.level)

  structure(
    list(
      N = n,
      n = sum(x),
      conf.level = conf.level
    ) |>
      utils::modifyList(val = broom::tidy(y) |> as.list()) |>
      utils::modifyList(
        list(
          method =
            glue::glue("Wilson Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
        )
      ),
    class = c("wilson", "prop_ci_uni", "cicada")
  )
}

#' Clopper-Pearson CI
#'
#' Calculates the Clopper-Pearson interval by calling [stats::binom.test()].
#'   Also referred to as the `exact` method.
#' @inheritParams ci_prop_wald
#'
#' @details
#' \deqn{
#' \left( \frac{k}{n} \pm z_{\alpha/2} \sqrt{\frac{\frac{k}{n}(1-\frac{k}{n})}{n} +
#' \frac{z^2_{\alpha/2}}{4n^2}} \right)
#' / \left( 1 + \frac{z^2_{\alpha/2}}{n} \right)}
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
#'
#' @export
ci_prop_clopper_pearson <- function(x, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # check installed packages ---------------------------------------------------
  check_pkg_installed(pkg = "broom")

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_clopper_pearson(
        x = x ,
        conf.level = conf.level
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }


  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)

  x <- stats::na.omit(x)
  n <- length(x)

  y <- stats::binom.test(x = sum(x), n = n, conf.level = conf.level)

  structure(
    list(N = n, n = sum(x), conf.level = conf.level) |>
      utils::modifyList(val = broom::tidy(y) |> as.list()) |>
      utils::modifyList(list(method = "Clopper-Pearson Confidence Interval")),
    class = c("clopper_pearson", "prop_ci_uni", "cicada")
  )
}

#' Agresti-Coull CI
#'
#' Calculates the `Agresti-Coull` interval (created by `Alan Agresti` and `Brent Coull`) by
#'   (for 95% CI) adding two successes and two failures to the data and then using the Wald formula to construct a CI.
#' @inheritParams ci_prop_wald
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
#'
#' @details
#' \deqn{
#' \left( \frac{\tilde{p} + z^2_{\alpha/2}/2}{n + z^2_{\alpha/2}} \pm
#' z_{\alpha/2} \sqrt{\frac{\tilde{p}(1 - \tilde{p})}{n} +
#' \frac{z^2_{\alpha/2}}{4n^2}} \right)}
#'
#' @export
ci_prop_agresti_coull <- function(x, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_agresti_coull(
        x = x ,
        conf.level = conf.level
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)

  x <- stats::na.omit(x)

  n <- length(x)
  x_sum <- sum(x)
  z <- stats::qnorm((1 + conf.level) / 2)

  # Add here both z^2 / 2 successes and failures.
  x_sum_tilde <- x_sum + z^2 / 2
  n_tilde <- n + z^2

  # Then proceed as with the Wald interval.
  p_tilde <- x_sum_tilde / n_tilde
  q_tilde <- 1 - p_tilde
  err <- z * sqrt(p_tilde * q_tilde) / sqrt(n_tilde)
  l_ci <- max(0, p_tilde - err)
  u_ci <- min(1, p_tilde + err)

  structure(
    list(
      N = n,
      n = sum(x),
      estimate = mean(x),
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method = "Agresti-Coull Confidence Interval"
    ),
    class = c("agresti_coull", "prop_ci_uni", "cicada")
  )
}

#' Jeffreys CI
#'
#' Calculates the Jeffreys interval, an equal-tailed interval based on the
#'   non-informative Jeffreys prior for a binomial proportion.
#'
#' @inheritParams ci_prop_wald
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
#'
#' @details
#' \deqn{\left( \text{Beta}\left(\frac{k}{2} + \frac{1}{2}, \frac{n - k}{2} + \frac{1}{2}\right)_\alpha,
#' \text{Beta}\left(\frac{k}{2} + \frac{1}{2}, \frac{n - k}{2} + \frac{1}{2}\right)_{1-\alpha} \right)}
#'
#' @export
ci_prop_jeffreys <- function(x, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_jeffreys(
        x = x ,
        conf.level = conf.level
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)
  x <- stats::na.omit(x)

  n <- length(x)
  x_sum <- sum(x)

  alpha <- 1 - conf.level
  l_ci <- ifelse(
    x_sum == 0,
    0,
    stats::qbeta(alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  u_ci <- ifelse(
    x_sum == n,
    1,
    stats::qbeta(1 - alpha / 2, x_sum + 0.5, n - x_sum + 0.5)
  )

  structure(
    list(
      N = n,
      n = sum(x),
      estimate = mean(x),
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method = glue::glue("Jeffreys Interval")
    ),
    class = c("jeffreys", "prop_ci_uni", "cicada")
  )
}


#' Stratified Wilson CI
#'
#' Calculates the stratified Wilson confidence
#'   interval for unequal proportions as described in
#'   Xin YA, Su XG. Stratified Wilson and Newcombe confidence intervals
#'   for multiple binomial proportions. _Statistics in Biopharmaceutical Research_. 2010;2(3).
#'
#' \deqn{\frac{\hat{p}_j + \frac{z^2_{\alpha/2}}{2n_j} \pm
#' z_{\alpha/2} \sqrt{\frac{\hat{p}_j(1 - \hat{p}_j)}{n_j} +
#' \frac{z^2_{\alpha/2}}{4n_j^2}}}{1 + \frac{z^2_{\alpha/2}}{n_j}}}
#'
#' @inheritParams ci_prop_diff_mn_strata
#' @param weights (`numeric`)\cr weights for each level of the strata. If `NULL`, they are
#'   estimated using the iterative algorithm that
#'   minimizes the weighted squared length of the confidence interval.
#' @param max.iterations (positive `integer`)\cr maximum number of iterations for the iterative procedure used
#'   to find estimates of optimal weights.
#' @param correct (scalar `logical`)\cr include the continuity correction. For further information, see for example
#'   [stats::prop.test()].
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{weights}{Weights of each strata, will be the same as the input unless
#'   unspecified, then it will be the dynamically calculated weights.}
#'   \item{method}{Type of method used}
#'
#' @examples
#' # Stratified Wilson confidence interval with unequal probabilities
#'
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' strata_data <- data.frame(
#'   x = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' strata <- interaction(strata_data)
#' n_strata <- ncol(table(rsp, strata)) # Number of strata
#'
#' ci_prop_wilson_strata(
#'   x = rsp, strata = strata,
#'   conf.level = 0.90
#' )
#'
#' # Not automatic setting of weights
#' ci_prop_wilson_strata(
#'   x = rsp, strata = strata,
#'   weights = rep(1 / n_strata, n_strata),
#'   conf.level = 0.90
#' )
#'
#' @export
ci_prop_wilson_strata <- function(x,
                                  strata,
                                  weights = NULL,
                                  conf.level = 0.95,
                                  max.iterations = 10L,
                                  correct = FALSE,
                                  data = NULL) {
  set_cli_abort_call()

  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_jeffreys(
        x = x ,
        conf.level = conf.level
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(strata)
  check_binary(x)
  check_class(correct, "logical")
  check_scalar(correct)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)
  # Change strata to be a the same length as x
  strata <- combine_strata(x, strata)
  if(!is.null(weights))
    check_identical_length(unique(strata), weights)

  if (!inherits(x, "logical")) x <- as.logical(x)
  # check all TRUE/FALSE, if so, not calculable
  if (all(x) || all(!x)) {
    cli::cli_abort("All values in {.arg x} argument are either {.code TRUE} or {.code FALSE} and CI is not estimable.")
  }



  tbl <- table(factor(x, levels = c(FALSE, TRUE)), strata, useNA = "no")
  n_strata <- length(unique(strata))

  # Checking the weights and maximum number of iterations.
  do_iter <- FALSE
  if (is.null(weights)) {
    weights <- rep(1 / n_strata, n_strata) # Initialization for iterative procedure
    do_iter <- TRUE

    # Iteration parameters
    if (!rlang::is_scalar_integerish(max.iterations) || max.iterations < 1) {
      cli::cli_abort("Argument {.arg max.iterations} must be a positive integer.")
    }
  }
  check_range(weights, range = c(0, 1), include_bounds = c(TRUE, TRUE))
  sum_weights <- sum(weights) |>
    round() |>
    as.integer()
  if (sum_weights != 1L || abs(sum_weights - sum(weights)) > sqrt(.Machine$double.eps)) {
    cli::cli_abort("The sum of the {.arg weights} argument must be {.val {1L}}")
  }

  xs <- tbl["TRUE", ]
  ns <- colSums(tbl)
  use_stratum <- (ns > 0)
  ns <- ns[use_stratum]
  xs <- xs[use_stratum]
  ests <- xs / ns
  vars <- ests * (1 - ests) / ns

  strata_qnorm <- .strata_normal_quantile(vars, weights, conf.level)

  # Iterative setting of weights if they were not passed in `weights` argument
  weights_new <- if (do_iter) {
    .update_weights_strat_wilson(vars, strata_qnorm, weights, ns, max.iterations, conf.level)$weights
  } else {
    weights
  }

  strata_conf.level <- 2 * stats::pnorm(strata_qnorm) - 1

  ci_by_strata <- Map(
    function(x, n) {
      # Classic Wilson's confidence interval
      suppressWarnings(stats::prop.test(x, n, correct = correct, conf.level = strata_conf.level)$conf.int)
    },
    x = xs,
    n = ns
  )
  lower_by_strata <- sapply(ci_by_strata, "[", 1L)
  upper_by_strata <- sapply(ci_by_strata, "[", 2L)

  lower <- sum(weights_new * lower_by_strata)
  upper <- sum(weights_new * upper_by_strata)

  # Return values
  structure(
    list(
      N = length(x),
      n = sum(x),
      estimate = mean(x),
      conf.low = lower,
      conf.high = upper,
      conf.level = conf.level,
      weights = if (do_iter) weights_new else weights,
      method =
        glue::glue("Stratified Wilson Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
    ),
    class = c("stratified_wilson", "prop_ci_uni", "cicada")
  )
}


#' Helper Function for the Estimation of Stratified Quantiles
#'
#' This function wraps the estimation of stratified percentiles when we assume
#' the approximation for large numbers. This is necessary only in the case
#' proportions for each strata are unequal.
#'
#' @inheritParams ci_prop_wilson_strata
#'
#' @return Stratified quantile.
#'
#' @seealso [ci_prop_wilson_strata()]
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' strata_data <- table(data.frame(
#'   "f1" = sample(c(TRUE, FALSE), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' ))
#' ns <- colSums(strata_data)
#' ests <- strata_data["TRUE", ] / ns
#' vars <- ests * (1 - ests) / ns
#' weights <- rep(1 / length(ns), length(ns))
#'
#' cicalc:::.strata_normal_quantile(vars, weights, 0.95)
.strata_normal_quantile <- function(vars, weights, conf.level) {
  summands <- weights^2 * vars
  # Stratified quantile
  sqrt(sum(summands)) / sum(sqrt(summands)) * stats::qnorm((1 + conf.level) / 2)
}

#' Helper Function for the Estimation of Weights for `ci_prop_wilson_strata()`
#'
#' This function wraps the iteration procedure that allows you to estimate
#' the weights for each proportional strata. This assumes to minimize the
#' weighted squared length of the confidence interval.
#'
#' @keywords internal
#' @inheritParams ci_prop_wilson_strata
#' @param vars (`numeric`)\cr normalized proportions for each strata.
#' @param strata_qnorm (`numeric`)\cr initial estimation with identical weights of the quantiles.
#' @param initial_weights (`numeric`)\cr initial weights used to calculate `strata_qnorm`. This can
#'   be optimized in the future if we need to estimate better initial weights.
#' @param n_per_strata (`numeric`)\cr number of elements in each strata.
#' @param max.iterations (`count`)\cr maximum number of iterations to be tried. Convergence is always checked.
#' @param tol (`number`)\cr tolerance threshold for convergence.
#'
#' @return A `list` of 3 elements: `n_it`, `weights`, and `diff_v`.
#'
#' @seealso For references and details see [`ci_prop_wilson_strata()`].
#'
#' @noRd
#' @examples
#' vs <- c(0.011, 0.013, 0.012, 0.014, 0.017, 0.018)
#' sq <- 0.674
#' ws <- rep(1 / length(vs), length(vs))
#' ns <- c(22, 18, 17, 17, 14, 12)
#'
#' cicalc:::.update_weights_strat_wilson(vs, sq, ws, ns, 100, 0.95, 0.001)
.update_weights_strat_wilson <- function(vars,
                                         strata_qnorm,
                                         initial_weights,
                                         n_per_strata,
                                         max.iterations = 50,
                                         conf.level = 0.95,
                                         tol = 0.001) {
  it <- 0
  diff_v <- NULL

  while (it < max.iterations) {
    it <- it + 1
    weights_new_t <- (1 + strata_qnorm^2 / n_per_strata)^2
    weights_new_b <- (vars + strata_qnorm^2 / (4 * n_per_strata^2))
    weights_new <- weights_new_t / weights_new_b
    weights_new <- weights_new / sum(weights_new)
    strata_qnorm <- .strata_normal_quantile(vars, weights_new, conf.level)
    diff_v <- c(diff_v, sum(abs(weights_new - initial_weights)))
    if (diff_v[length(diff_v)] < tol) break
    initial_weights <- weights_new
  }

  if (it == max.iterations) {
    warning("The heuristic to find weights did not converge with max.iterations = ", max.iterations)
  }

  list(
    "n_it" = it,
    "weights" = weights_new,
    "diff_v" = diff_v
  )
}
