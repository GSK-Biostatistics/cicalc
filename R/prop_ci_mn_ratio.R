#' Miettinen-Nurminen / Asymptotic (Koopman) Score Confidence Intervals for a Relative Risk
#'
#' Calculates the relative risk and Miettinen-Nurminen (1985) score confidence interval
#' and the asymptotic (Koopman, 1984) score confidence interval (without the finite-sample correction)
#' for two independent binomial proportions.
#'
#' The relative risk estimate is computed as:
#'
#' \deqn{\hat{RR} = \frac{\hat{p}_x}{\hat{p}_y} = \frac{s_x / n_x}{s_y / n_y}}
#'
#' where \eqn{s_x} and \eqn{s_y} are the number of events in each group, and
#' \eqn{n_x} and \eqn{n_y} are the group sizes.
#'
#' The Koopman/Miettinen-Nurminen score statistic for a hypothesized ratio
#' \eqn{RR} is:
#'
#' \deqn{z = \frac{s_x - n_x \tilde{p}_x}{(1 - \tilde{p}_x)
#' \sqrt{\hat{\sigma}^2}}}
#'
#' where \eqn{\tilde{p}_x = RR \, \tilde{p}_y} and
#' \eqn{\tilde{p}_y} is the constrained maximum likelihood estimate of
#' \eqn{p_y} under \eqn{H_0: RR = p_x / p_y}, obtained as the
#' appropriate root of the quadratic
#'
#' \deqn{(n_x + n_y) RR \, \tilde{p}_y^2 - \left[(s_y + n_x) RR + s_x +
#' n_y\right] \tilde{p}_y + (s_x + s_y) = 0}
#'
#' and the variance term is
#'
#' \deqn{\hat{\sigma}^2 = c \cdot \frac{n_x n_y \tilde{p}_x}
#' {n_x(RR - \tilde{p}_x) + n_y(1 - \tilde{p}_x)}}
#'
#' with the finite-sample correction factor \eqn{c = N/(N-1)} (where
#' \eqn{N = n_x + n_y}) applied when `method = "score"`, and \eqn{c = 1} when
#' `method = "asymptotic"`. The confidence limits are the values of \eqn{RR} that
#' solve \eqn{z = \pm z_{1-\alpha/2}}, found numerically via
#' [stats::uniroot()].
#'
#' @param x Binary response vector (or unquoted column name if `data` is supplied).
#' @param by Grouping vector with exactly two levels (or unquoted column name).
#' @param conf.level Confidence level. Default `0.95`.
#' @param method String specifying the method to use. Either `"score"`
#'   (default), which applies the Miettinen-Nurminen finite-sample bias
#'   correction, or `"asymptotic"`, which returns the uncorrected Koopman
#'   (1984) asymptotic score CI.
#' @param delta Optional null hypothesis value(s) for the relative risk. Must be
#'   positive. May be a vector, in which case a score test of `H0: RR = delta`
#'   is performed for each value. If supplied (not `NULL`), the returned object
#'   additionally includes `delta`, `statistic` and `p.value`, each with one
#'   element per value of `delta`.
#' @param data Optional data frame containing `x` and `by`.
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses in each group}
#'   \item{N}{Total number in each group}
#'   \item{estimate}{The relative risk point estimate}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{delta}{Null value(s) tested, when `delta` is supplied}
#'   \item{statistic}{Score test statistic(s), when `delta` is supplied}
#'   \item{p.value}{p-value for the score test, when `delta` is supplied}
#'   \item{method}{Description of the method used}
#'
#' When neither group has events (or, for the odds ratio, both groups have all
#' events), `estimate` is `NA`, the interval is `(0, Inf)`, and the score
#' statistic is `0` with p-value `1`.
#'
#' If `delta` is not provided, `delta`, `statistic` and `p.value` are absent (`NULL`).
#' @export
#' @references
#' Koopman PAR (1984). Confidence intervals for the ratio of two binomial
#' proportions. Biometrics, 40(2), 513-517.
#'
#' Miettinen OS, Nurminen M (1985). Comparative analysis of two rates. Statistics in Medicine, 4(2), 213-226.
#'
#' Farrington CP, Manning G (1990). Test statistics and sample size formulae for comparative binomial trials with null hypothesis of non-zero risk difference. Statistics in Medicine, 9(12), 1447-1454.
#'
#' @examples
#' # Generate binary samples
#' responses  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
#' by <- c(rep(1, 40), rep(0, 40))
#'
#' # Calculate the RR and CI for Miettinen-Nurminen score method
#' ci_rel_risk_mn(x = responses, by = by)
#'
#' # Calculate the RR and CI for the uncorrected asymptotic score method
#' ci_rel_risk_mn(x = responses, by = by, method = "asymptotic")
#'
#' # Calculate the RR and CI for Miettinen-Nurminen score method, and test H0: RR = 1
#' ci_rel_risk_mn(x = responses, by = by, delta = 1)
ci_rel_risk_mn <- function(x, by, conf.level = 0.95, method = c("score", "asymptotic"), delta = NULL, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if (is.data.frame(data)) {
    return(eval(substitute(ci_rel_risk_mn(
      x = x, by = by, conf.level = conf.level, method = method, delta = delta
    )), envir = data, enclos = parent.frame()))
  }

  method <- match.arg(method)
  check_not_missing(x)
  check_binary(x)
  check_not_missing(by)
  check_n_levels(by, n_levels = 2)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_identical_length(x, by)
  if (!is.null(delta)) {
    check_numeric(delta)
    check_range(delta, range = c(0, Inf), include_bounds = c(FALSE, FALSE))
  }

  df <- get_counts(x = x, by = by)

  s_x <- df$response_1
  n_x <- df$n_1
  s_y <- df$response_2
  n_y <- df$n_2

  alpha  <- 1 - conf.level
  z_crit <- stats::qnorm(1 - alpha / 2)

  p1 <- s_x / n_x
  p2 <- s_y / n_y
  rr_hat <- p1 / p2

  # Not estimable when neither group has events: the interval is the
  # uninformative (0, Inf). Other boundary cases (zero events or all events in
  # one group) still have score intervals.
  if (s_x == 0 && s_y == 0) {
    method_label <- if (method == "score") {
      "RR Miettinen-Nurminen / Farrington-Manning Score Confidence Interval"
    } else {
      "RR Koopman Asymptotic Score Confidence Interval"
    }

    warning(
      "Risk ratio is not estimable when neither group has events.",
      call. = FALSE
    )

    out <- structure(
      list(
        n = c(s_x, s_y),
        N = c(n_x, n_y),
        estimate = NA_real_,
        conf.low = 0,
        conf.high = Inf,
        conf.level = conf.level,
        method = glue::glue(method_label)
      ),
      class = c("mn_score", "prop_ci_bi", "cicada")
    )

    if (!is.null(delta)) {
      # The score numerator is exactly 0 when both groups have identical
      # boundary counts, so the statistic is 0 (matches gsDesign::testBinomial).
      out$delta <- delta
      out$statistic <- rep(0, length(delta))
      out$p.value <- rep(1, length(delta))
    }

    return(out)
  }

  N <- n_x + n_y
  fm_mult <- if (method == "score") N / (N - 1) else 1

  # Koopman (1984) score statistic for phi = p_x / p_y
  score_stat_rr <- function(phi, s_x, n_x, s_y, n_y, mult = 1) {
    A <- (n_x + n_y) * phi
    B <- -((s_y + n_x) * phi + s_x + n_y)
    Cc <- s_y + s_x

    disc <- pmax(B^2 - 4 * A * Cc, 0)
    pi_y_tilde <- (-B - sqrt(disc)) / (2 * A)
    pi_x_tilde <- pi_y_tilde * phi

    # Variance of (p_x_hat - phi * p_y_hat) under H0; stays finite when
    # pi_x_tilde = 1 (all events in the x group)
    pi_y_tilde <- pi_x_tilde / phi
    var_term <- mult * (pi_x_tilde * (1 - pi_x_tilde) / n_x +
      phi^2 * pi_y_tilde * (1 - pi_y_tilde) / n_y)

    (s_x / n_x - phi * s_y / n_y) / sqrt(var_term)
  }

  # NaNs can arise while uniroot probes extreme phi values at the boundaries
  obj <- function(phi) {
    suppressWarnings(score_stat_rr(phi, s_x, n_x, s_y, n_y, mult = fm_mult)^2 - z_crit^2)
  }

  if (rr_hat <= 0) {
    lower_ci <- 0
    upper_ci <- tryCatch(
      stats::uniroot(obj, lower = 1e-8, upper = 1, extendInt = "yes", tol = 1e-8)$root,
      error = function(e) 1
    )
  } else if (is.infinite(rr_hat)) {
    lower_ci <- tryCatch(
      stats::uniroot(obj, lower = 1e-8, upper = 1, extendInt = "yes", tol = 1e-8)$root,
      error = function(e) 0
    )
    upper_ci <- Inf
  } else {
    # The score statistic is 0/0 at the estimate when both groups have all
    # events (rr_hat = 1), so bracket just off the estimate in that case.
    eps <- if (s_x == n_x && s_y == n_y) 1e-6 else 0
    lower_ci <- stats::uniroot(obj, lower = 1e-8, upper = rr_hat * (1 - eps),
                                extendInt = "yes", tol = 1e-8)$root
    upper_ci <- stats::uniroot(obj, lower = rr_hat * (1 + eps), upper = rr_hat * 50,
                                extendInt = "yes", tol = 1e-8)$root
  }

  method_label <- if (method == "score") {
    "RR Miettinen-Nurminen / Farrington-Manning Score Confidence Interval"
  } else {
    "RR Koopman Asymptotic Score Confidence Interval"
  }

  out <- structure(
    list(
      n = c(s_x, s_y),
      N = c(n_x, n_y),
      estimate = rr_hat,
      conf.low = lower_ci,
      conf.high = upper_ci,
      conf.level = conf.level,
      method = glue::glue(method_label)
    ),
    class = c("mn_score", "prop_ci_bi", "cicada")
  )

  if (!is.null(delta)) {
    score_statistic <- score_stat_rr(delta, s_x, n_x, s_y, n_y, mult = fm_mult)
    # 0/0 at delta = 1 when both groups have all events; the numerator is 0
    if (s_x == n_x && s_y == n_y) {
      score_statistic[delta == 1] <- 0
    }
    p_value <- 2 * stats::pnorm(-abs(score_statistic))

    out$delta <- delta
    out$statistic <- score_statistic
    out$p.value <- p_value
  }

  out
}


# Print score test results for each supplied delta
print_delta_tests <- function(x) {
  if (is.null(x$delta)) return(invisible(NULL))
  cli::cli_h3("Score test")
  for (i in seq_along(x$delta)) {
    cli::cli_li(
      "H0: ratio = {x$delta[i]}: statistic = {round(x$statistic[i], 3)}, p-value = {format.pval(x$p.value[i], digits = 4)}"
    )
  }
  invisible(NULL)
}

#' @export
print.mn_score <- function(x, ...) {
  rr_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{rr_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("  ({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  print_delta_tests(x)
  invisible(x)
}




#' Miettinen-Nurminen / Asymptotic Score Confidence Intervals for an Odds Ratio
#'
#' Calculates the odds ratio and Miettinen-Nurminen (1985) score confidence
#' interval and the asymptotic score confidence interval for two independent binomial proportions.
#'
#' The odds ratio estimate is computed as:
#'
#' \deqn{\hat{OR} = \frac{\hat{p}_x / (1 - \hat{p}_x)}
#' {\hat{p}_y / (1 - \hat{p}_y)}}
#'
#' where \eqn{\hat{p}_x = s_x / n_x} and \eqn{\hat{p}_y = s_y / n_y} are
#' the observed proportions in each group.
#'
#' The Miettinen-Nurminen/Farrington-Manning score statistic for a
#' hypothesized odds ratio \eqn{OR} is:
#'
#' \deqn{z = \frac{s_x - n_x \tilde{p}_x}{\sqrt{\hat{\sigma}^2}}}
#'
#' where \eqn{\tilde{p}_y} is the constrained maximum likelihood
#' estimate of \eqn{p_y} under \eqn{H_0: OR = [p_x/(1-p_x)] /
#' [p_y/(1-p_y)]}, obtained as the appropriate root of the quadratic
#'
#' \deqn{n_y(OR - 1) \tilde{p}_y^2 + \left[n_x \, OR + n_y -
#' (s_x + s_y)(OR - 1)\right] \tilde{p}_y - (s_x + s_y) = 0}
#'
#' with \eqn{\tilde{p}_x = \dfrac{OR \, \tilde{p}_y}
#' {1 + \tilde{p}_y(OR - 1)}}, and the variance term is
#'
#' \deqn{\hat{\sigma}^2 = c \cdot \frac{n_x \tilde{p}_x(1-\tilde{p}_x)
#' \, n_y \tilde{p}_y(1-\tilde{p}_y)}{n_x \tilde{p}_x(1-\tilde{p}_x) +
#' n_y \tilde{p}_y(1-\tilde{p}_y)}}
#'
#' with the finite-sample correction factor \eqn{c = N/(N-1)} (where
#' \eqn{N = n_x + n_y}) applied when `method = "score"`, and \eqn{c = 1} when
#' `method = "asymptotic"`. The confidence limits are the values of \eqn{OR} that
#' solve \eqn{z = \pm z_{1-\alpha/2}}, found numerically via
#' [stats::uniroot()].
#'
#' @param x Binary response vector (or unquoted column name if `data` is supplied).
#' @param by Grouping vector with exactly two levels (or unquoted column name).
#' @param conf.level Confidence level. Default `0.95`.
#' @param method String specifying the method to use. Either `"score"`
#'   (default), which applies the Miettinen-Nurminen finite-sample bias
#'   correction to the variance, or `"asymptotic"`, which returns the
#'   uncorrected asymptotic score CI.
#' @param delta Optional null hypothesis value(s) for the odds ratio. Must be
#'   positive. May be a vector, in which case each value is tested separately.
#'   If supplied, the returned object additionally includes `delta` and the
#'   Miettinen-Nurminen / Farrington-Manning score `statistic` and two-sided
#'   `p.value` for testing `H0: OR = delta` (one element per value of `delta`),
#'   evaluated using the same `method` setting.
#' @param data Optional data frame containing `x` and `by`.
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses in each group}
#'   \item{N}{Total number in each group}
#'   \item{estimate}{The odds ratio point estimate}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{delta}{Null value(s) tested, when `delta` is supplied}
#'   \item{statistic}{Score test statistic(s), when `delta` is supplied}
#'   \item{p.value}{P-value for the score test, when `delta` is supplied}
#'   \item{method}{Description of the method used}
#'
#' When neither group has events (or, for the odds ratio, both groups have all
#' events), `estimate` is `NA`, the interval is `(0, Inf)`, and the score
#' statistic is `0` with p-value `1`.
#'
#' If `delta` is not provided, `delta`, `statistic` and `p.value` are absent (`NULL`).
#' @export
#' @references
#' Miettinen OS, Nurminen M (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#' Farrington CP, Manning G (1990). Test statistics and sample size formulae
#' for comparative binomial trials with null hypothesis of non-zero risk
#' difference. Statistics in Medicine, 9(12), 1447-1454.
#'
#' @examples
#' # Generate binary samples
#' responses  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
#' by <- c(rep(1, 40), rep(0, 40))
#'
#' # Calculate the OR and CI for Miettinen-Nurminen score method
#' ci_odds_ratio_mn(x = responses, by = by)
#'
#' # Calculate the OR and CI for the uncorrected asymptotic score method
#' ci_odds_ratio_mn(x = responses, by = by, method = "asymptotic")
#'
#' # Calculate the OR and CI for Miettinen-Nurminen score method, and test H0: OR = 1
#' ci_odds_ratio_mn(x = responses, by = by, delta = 1)
ci_odds_ratio_mn <- function(x, by, conf.level = 0.95, method = c("score", "asymptotic"), delta = NULL, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if (is.data.frame(data)) {
    return(eval(substitute(ci_odds_ratio_mn(
      x = x, by = by, conf.level = conf.level, method = method, delta = delta
    )), envir = data, enclos = parent.frame()))
  }

  method <- match.arg(method)
  check_not_missing(x)
  check_binary(x)
  check_not_missing(by)
  check_n_levels(by, n_levels = 2)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_identical_length(x, by)
  if (!is.null(delta)) {
    check_numeric(delta)
    check_range(delta, range = c(0, Inf), include_bounds = c(FALSE, FALSE))
  }

  df <- get_counts(x = x, by = by)

  s_x <- df$response_1
  n_x <- df$n_1
  s_y <- df$response_2
  n_y <- df$n_2

  alpha  <- 1 - conf.level
  z_crit <- stats::qnorm(1 - alpha / 2)

  p1 <- s_x / n_x
  p2 <- s_y / n_y

  # Not estimable when neither group has events or both have all events: the
  # interval is the uninformative (0, Inf). Other boundary cases still have
  # score intervals.
  if ((s_x == 0 && s_y == 0) || (s_x == n_x && s_y == n_y)) {
    method_label <- if (method == "score") {
      "OR Miettinen-Nurminen / Farrington-Manning Score Confidence Interval"
    } else {
      "OR Uncorrected Asymptotic Score Confidence Interval"
    }

    warning(
      "Odds ratio is not estimable when both groups have no events or both have all events.",
      call. = FALSE
    )

    estimate <- NA_real_

    out <- structure(
      list(
        n = c(s_x, s_y),
        N = c(n_x, n_y),
        estimate = estimate,
        conf.low = 0,
        conf.high = Inf,
        conf.level = conf.level,
        method = glue::glue(method_label)
      ),
      class = c("mn_score_or", "prop_ci_bi", "cicada")
    )

    if (!is.null(delta)) {
      out$delta <- delta
      out$statistic <- rep(0, length(delta))
      out$p.value <- rep(1, length(delta))
    }

    return(out)
  }

  or_hat <- if (s_y == 0 || s_x == n_x) {
    Inf
  } else if (s_x == 0 || s_y == n_y) {
    0
  } else {
    (p1 / (1 - p1)) / (p2 / (1 - p2))
  }

  N <- n_x + n_y
  fm_mult <- if (method == "score") N / (N - 1) else 1

  # Miettinen-Nurminen / Farrington-Manning score statistic for the odds
  # ratio psi = odds_x / odds_y, obtained via the constrained MLE of p2
  # under H0: psi = psi0.
  score_stat_or <- function(psi, s_x, n_x, s_y, n_y, mult = 1) {
    m1 <- s_x + s_y
    A <- n_y * (psi - 1)
    B <- n_x * psi + n_y - m1 * (psi - 1)
    Cc <- -m1

    p2_tilde <- ifelse(
      abs(A) < 1e-10,
      -Cc / B,
      (-B + sqrt(pmax(B^2 - 4 * A * Cc, 0))) / (2 * A)
    )
    p1_tilde <- p2_tilde * psi / (1 + p2_tilde * (psi - 1))

    var_term <- mult * (n_x * p1_tilde * (1 - p1_tilde) * n_y * p2_tilde * (1 - p2_tilde)) /
      (n_x * p1_tilde * (1 - p1_tilde) + n_y * p2_tilde * (1 - p2_tilde))

    (s_x - n_x * p1_tilde) / sqrt(var_term)
  }

  # NaNs can arise while uniroot probes extreme psi values at the boundaries
  obj <- function(psi) {
    suppressWarnings(score_stat_or(psi, s_x, n_x, s_y, n_y, mult = fm_mult)^2 - z_crit^2)
  }

  if (or_hat <= 0) {
    lower_ci <- 0
    upper_ci <- tryCatch(
      stats::uniroot(obj, lower = 1e-8, upper = 1, extendInt = "yes", tol = 1e-8)$root,
      error = function(e) 1
    )
  } else if (is.infinite(or_hat)) {
    lower_ci <- tryCatch(
      stats::uniroot(obj, lower = 1e-8, upper = 1, extendInt = "yes", tol = 1e-8)$root,
      error = function(e) 0
    )
    upper_ci <- Inf
  } else {
    lower_ci <- stats::uniroot(obj, lower = 1e-8, upper = or_hat,
                             extendInt = "yes", tol = 1e-8)$root
    upper_ci <- stats::uniroot(obj, lower = or_hat, upper = or_hat * 100,
                             extendInt = "yes", tol = 1e-8)$root
  }

  method_label <- if (method == "score") {
    "OR Miettinen-Nurminen / Farrington-Manning Score Confidence Interval"
  } else {
    "OR Uncorrected Asymptotic Score Confidence Interval"
  }

  out <- list(
    n = c(s_x, s_y),
    N = c(n_x, n_y),
    estimate = or_hat,
    conf.low = lower_ci,
    conf.high = upper_ci,
    conf.level = conf.level,
    method = glue::glue(method_label)
  )

  if (!is.null(delta)) {
    stat <- score_stat_or(delta, s_x, n_x, s_y, n_y, mult = fm_mult)
    out$delta <- delta
    out$statistic <- stat
    out$p.value <- 2 * stats::pnorm(-abs(stat))
  }

  structure(out, class = c("mn_score_or", "prop_ci_bi", "cicada"))
}


#' @export
print.mn_score_or <- function(x, ...) {
  or_str <- paste0(x$n, "/", x$N, collapse = " - ")
  cli::cli_h1(x$method)
  cli::cli_li("{or_str}")
  cli::cli_li("Estimate: {round(x$estimate, 3)}")
  cli::cli_li("{x$conf.level*100}% Confidence Interval:")
  cli::cli_text("  ({round(x$conf.low, 4)}, {round(x$conf.high, 4)})")
  print_delta_tests(x)
  invisible(x)
}


