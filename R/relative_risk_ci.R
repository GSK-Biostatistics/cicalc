#' Mantel-Haenszel Stratified Reltative Risk Confidence Interval
#'
#' Calculates the confidence interval for the Mantel-Haenszel estimate of the
#' common reltative risk across multiple 2x2 tables (strata)
#'
#' The Mantel-Haenszel reltative risk difference is computed as:
#'
#' \deqn{RR_{MH} = \frac{\sum_{k} s_{xk}~n_{yk}/N_k}{\sum_{k} s_{yk}~n_{xk}/N_k}}
#'
#' The variance is:
#'
#' \deqn{\hat{\sigma}^2 = \hat{Var}(log(RR_{MH})) =
#' \frac{\sum_{k}(n_{xk}~n_{yk}~(s_{xk}+s_{yk}) - s_{xk}~s_{yk}~N_k)/N_k^2}
#' {(\sum_{k}s_{xk}~n_{yk}/N_k)(\sum_{k}s_{yk}~n_{xk}/N_k)}}
#'
#'
#' The confidence interval is then \eqn{\left(RR_{MH}\times exp(-z_{1-\alpha/2} \sqrt{\hat{\sigma}^2},
#' RR_{MH}\times exp(z_{1-\alpha/2} \sqrt{\hat{\sigma}^2}\right)}.
#'
#' @inheritParams ci_prop_diff_mn_strata
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The Mantel-Haeszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{variance}{Mantel-Haenszel variance estimate \eqn{Var(log(RR_MH))}}
#'   \item{statistic}{Z-Statistic under the null hypothesis, assuming a common risk difference of 0}
#'   \item{p.value}{p-value under the null hypothesis, assuming a common risk difference of 0}
#'   \item{method}{Description of the method used ("Mantel-Haenszel Common Relattive Risk Confidence Interval")}
#' @export
#' @references
#' Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley & Sons, Hoboken, NJ
#'
#' @examples
#' # Generate binary samples with strata
#' responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
#' arm <- rep(c("treat", "control"), 20)
#' strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))
#'
#' # Calculate common risk difference
#' ci_rel_risk_cmh_strata(x = responses, by = arm, strata = strata)
ci_rel_risk_cmh_strata <- function(x, by, strata, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    return(
      ci_risk_diff_mh_strata(
        x = x ,
        by = by ,
        strata = strata,
        conf.level = conf.level
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_not_missing(by)
  check_n_levels(by, n_levels = 2)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_identical_length(x, by)

  strata <- combine_strata(x, strata)

  response_df <- get_counts(x = x, by = by, strata = strata)

  n_x <- response_df$n_1
  s_x <- response_df$response_1
  n_y <- response_df$n_2
  s_y <- response_df$response_2
  N_k <- n_x + n_y

  rel_rik_denom <- sum(s_y*n_x/N_k)
  if(any(rel_rik_denom != 0)){
    rel_risk_mh <- sum(s_x*n_y/N_k)/rel_rik_denom
  } else {
    cli::cli_abort("Denominator of the Mental Haenszel Estimator can not sum to 0")
  }

  var_rr <- sum((n_x*n_y*(s_x+s_y)-s_x*s_y*N_k)/N_k^2)/(sum(s_x*n_y/N_k)*sum(s_y*n_x/N_k))

  z_alpha <- stats::qnorm((1 + conf.level) / 2)

  lower_ci <- rel_risk_mh*exp(-z_alpha*sqrt(var_rr))
  upper_ci <- rel_risk_mh*exp(z_alpha*sqrt(var_rr))

  z_stat <- log(rel_risk_mh)/sqrt(var_rr)

  p.value <- 2 * (1 - pnorm(abs(z_stat)))

  df <- get_counts(x = x, by = by)
  # Output
  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = rel_risk_mh,
      conf.low = lower_ci,
      conf.high = upper_ci,
      conf.level = conf.level,
      variance = var_rr,
      statistic = z_stat,
      p.value = p.value,
      method =
        glue::glue("Mantel-Haenszel Common Relattive Risk Confidence Interva")
    ),
    class = c("ci_rel_risk_cmh_strata", "cicalc")
  )
}
