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
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The Mentel-Haeszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{variance}{Sato variance estimate}
#'   \item{statistic}{Z-Statistic under the null hypothesis, assuming a common risk difference of 0}
#'   \item{p.value}{p-value under the null hypothesis, assuming a common risk difference of 0}
#'   \item{method}{Description of the method used ("Mentel-Haenszel Confidence Interval, Sato Variance")}
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
ci_risk_diff_mh_strata <- function(x, by, strata, conf.level = 0.95, data = NULL) {
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
  # Calculate weights and diff in weighted proportions
  weights <-(n_x * n_y) / (n_x + n_y)
  names(weights) <- response_df$strata
  tot_w <- sum(weights)

  weights_k <- weights/tot_w
  d_hat_k <- s_x/n_x - s_y/n_y

  d_mh <- sum(d_hat_k*weights_k)

  p_k <- (n_x^2*s_y - n_y^2*s_x + n_x*n_y*(n_y-n_x)/2)/N_k^2
  q_k <- (s_x*(n_y - s_y) + s_y*(n_x - s_x))/(2*N_k)

  sato_var <- (d_mh*sum(p_k) + sum(q_k))/sum(n_x*n_y/N_k)^2

  alpha <- 1 - conf.level
  z_alpha <- stats::qnorm((1 + conf.level) / 2)

  lower_ci <- d_mh - z_alpha*sqrt(sato_var)
  upper_ci <- d_mh + z_alpha*sqrt(sato_var)

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
      variance = sato_var,
      statistic = z_stat,
      p.value = p.value,
      method =
        glue::glue("Mentel-Haenszel Risk Difference Confidence Interval, Sato Variance")
    ),
    class = c("ci_risk_diff_mh_strata", "cicalc")
  )
}
