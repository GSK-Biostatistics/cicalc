#' Mantel-Haenszel Common Risk Difference Confidence Interval (Sato Variance)
#'
#' Calculates the confidence interval for the Mantel-Haenszel estimate of the
#' common risk difference across multiple 2x2 tables (strata), using the Sato
#' variance estimator.
#'
#' The Mantel-Haenszel common risk difference is computed as:
#'
#' \deqn{\hat{\delta}_{MH} = \frac{\sum_{k} w_k \hat{\delta}_k }{\sum_{k} w_k}}
#'
#' where \eqn{w_k = \frac{n_{xk} n_{yk}}{N_k}},
#' \eqn{\hat{\delta}_k = s_{xk}/n_{xk} - y_{yk}/n_{yk}},
#' \eqn{N_k = n_{xk} + n_{yk}},
#' \eqn{s_{xk}} and \eqn{s_{yk}} are the number of events in each group, and \eqn{n_{xk}},
#' and \eqn{n_{yk}} are the group sizes in stratum \eqn{k}.
#'
#' The Sato variance is:
#'
#' \deqn{\hat{\sigma}^2(\hat{\delta}_{MH}) = \frac{\hat{d}_{MH} \sum_{k}{P_k} + \sum_k Q_k}{\left( \sum_k w_k \right)^2}}
#'
#' where \eqn{P_k = \frac{n_{xk}^2 s_{yk} - n_{yk}^2 s_{xk} + n_{xk} n_{yk} (n_{yk} - n_{xk})/2}{N_k^2}}
#' and \eqn{Q_k = \frac{s_{xk}(n_{yk} - s_{yk}) + s_{yk}(n_{xk} - s_{xk})}{2 N_k}}.
#'
#' The confidence interval is then \eqn{\hat{\delta}_{MH} \pm z_{1-\alpha/2} \sqrt{\hat{\sigma}^2(\hat{d}_{MH})}}.
#'
#' @inheritParams ci_prop_diff_mn_strata
#' @param sato_var (`logical`)\cr Use Sato variance estimate
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The Mantel-Haeszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{variance}{Sato variance estimate}
#'   \item{statistic}{Z-Statistic under the null hypothesis, assuming a common risk difference of 0}
#'   \item{p.value}{p-value under the null hypothesis, assuming a common risk difference of 0}
#'   \item{method}{Description of the method used ("Mantel-Haenszel Confidence Interval, Sato Variance")}
#' @export
#' @references
#' Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley & Sons, Hoboken, NJ p. 231
#'
#' @examples
#' # Generate binary samples with strata
#' responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
#' arm <- rep(c("treat", "control"), 20)
#' strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))
#'
#' # Calculate common risk difference
#' ci_risk_diff_mh_strata(x = responses, by = arm, strata = strata)
ci_prop_diff_mh_strata <- function(x, by, strata, conf.level = 0.95, sato_var = TRUE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    return(
      ci_prop_diff_mh_strata(
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
  check_logical(sato_var)

  strata <- combine_strata(x, strata)

  response_df <- get_counts(x = x, by = by, strata = strata)

  n_x <- response_df$n_1
  s_x <- response_df$response_1
  n_y <- response_df$n_2
  s_y <- response_df$response_2
  N_k <- n_x + n_y
  # Calculate weights and diff in weighted proportions
  weights <-(n_x * n_y) / (n_x + n_y)
  names(weights) <- response_df$strata
  tot_w <- sum(weights)

  weights_k <- weights/tot_w
  d_hat_k <- s_x/n_x - s_y/n_y

  d_mh <- sum(d_hat_k*weights_k)

  p_k <- (n_x^2*s_y - n_y^2*s_x + n_x*n_y*(n_y-n_x)/2)/N_k^2
  q_k <- (s_x*(n_y - s_y) + s_y*(n_x - s_x))/(2*N_k)

  if(sato_var) {
    est_var <- (d_mh*sum(p_k) + sum(q_k))/sum(n_x*n_y/N_k)^2
    var_title <- ", Sato Variance"
  } else {
    est_var <- sum((((s_x/100) * (1 - (s_x/100)) / n_x) + ((s_y/100) * (1 - (s_y/100)) / s_y)) * weights_k^2)
    var_title <- ""
  }


  alpha <- 1 - conf.level
  z_alpha <- stats::qnorm((1 + conf.level) / 2)

  lower_ci <- d_mh - z_alpha*sqrt(est_var)
  upper_ci <- d_mh + z_alpha*sqrt(est_var)

  z_stat <- d_mh/sqrt(sato_var)

  p.value <- 2 * (1 - pnorm(abs(z_stat)))

  df <- get_counts(x = x, by = by)
  # Output
  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = d_mh,
      conf.low = lower_ci,
      conf.high = upper_ci,
      conf.level = conf.level,
      variance = est_var,
      statistic = z_stat,
      p.value = p.value,
      method =
        glue::glue("Mantel-Haenszel Risk Difference Confidence Interval{var_title}")
    ),
    class = c("ci_prop_diff_mh_strata", "cicalc")
  )
}

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
      ci_rel_risk_cmh_strata(
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
    cli::cli_abort("Denominator of the Mantel Haenszel Estimator can not sum to 0")
  }

  var_rr <- sum((n_x*n_y*(s_x+s_y)-s_x*s_y*N_k)/N_k^2)/(sum(s_x*n_y/N_k)*sum(s_y*n_x/N_k))

  z_alpha <- stats::qnorm((1 + conf.level) / 2)

  lower_ci <- rel_risk_mh*exp(-z_alpha*sqrt(var_rr))
  upper_ci <- rel_risk_mh*exp(z_alpha*sqrt(var_rr))

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
      method =
        glue::glue("Mantel-Haenszel Common Relattive Risk Confidence Interval")
    ),
    class = c("ci_rel_risk_cmh_strata", "cicalc")
  )
}

