#' Mantel-Haenszel Common Risk Difference Confidence Interval (Sato Variance)
#'
#' Calculates the confidence interval for the Mantel-Haenszel estimate of the common risk difference across multiple 2x2 tables (strata), using the Sato variance estimator.
#'
#' The Mantel-Haenszel common risk difference is computed as:
#'
#' \deqn{\hat{d}_{MH} = \sum_{h=1}^H w_h \left( \frac{s_{xh}}{n_{xh}} - \frac{s_{yh}}{n_{yh}} \right)}
#'
#' where \eqn{w_h = \frac{n_{xh} n_{yh}}{n_{xh} + n_{yh}} / \sum_{h=1}^H \frac{n_{xh} n_{yh}}{n_{xh} + n_{yh}}} is the normalized stratum weight, \eqn{s_{xh}} and \eqn{s_{yh}} are the number of events in each group, and \eqn{n_{xh}}, \eqn{n_{yh}} are the group sizes in stratum \eqn{h}.
#'
#' The Sato variance is:
#'
#' \deqn{\mathrm{Var}_{Sato}(\hat{d}_{MH}) = \frac{\hat{d}_{MH} \sum_h p_h + \sum_h q_h}{\left( \sum_h \frac{n_{xh} n_{yh}}{n_{xh} + n_{yh}} \right)^2}}
#'
#' where \eqn{p_h = \frac{n_{xh}^2 s_{yh} - n_{yh}^2 s_{xh} + n_{xh} n_{yh} (n_{yh} - n_{xh})/2}{N_h^2}} and \eqn{q_h = \frac{s_{xh}(n_{yh} - s_{yh}) + s_{yh}(n_{xh} - s_{xh})}{2 N_h}}, with \eqn{N_h = n_{xh} + n_{yh}}.
#'
#' The confidence interval is then \eqn{\hat{d}_{MH} \pm z_{1-\alpha/2} \sqrt{\mathrm{Var}_{Sato}(\hat{d}_{MH})}}.
#'
#' @inheritParams ci_prop_diff_mn_strata
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The Mentel-Haeszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{statistic}{Z-Statistic under the null hypothesis, assuming a common risk difference of 0}
#'   \item{p.value}{p-value under the null hypothesis, assuming a common risk difference of 0}
#'   \item{method}{Description of the method used ("Mentel-Haenszel Confidence Interval, Sato Variance")}
#' @export
#'
#' @examples
#' # Example with simulated data
#' set.seed(1)
#' strata <- rep(1:3, each = 40)
#' by <- rep(c("A", "B"), times = 60)
#' x <- rbinom(120, 1, prob = rep(c(0.2, 0.3, 0.4), each = 40))
#' result <- ci_prop_common_risk_diff_sato(x = x, by = by, strata = strata)
#' print(result)
ci_prop_common_risk_diff_sato <- function(x, by, strata, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    return(
      ci_prop_common_risk_diff_sato(
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
  N_h <- n_x + n_y
  # Calculate weights and diff in weighted proportions
  weights <-(n_x * n_y) / (n_x + n_y)
  names(weights) <- response_df$strata
  tot_w <- sum(weights)

  weights_h <- weights/tot_w
  d_hat_h <- s_x/n_x - s_y/n_y

  d_mh <- sum(d_hat_h*weights_h)

  p_h <- (n_x^2*s_y - n_y^2*s_x + n_x*n_y*(n_y-n_x)/2)/N_h^2
  q_h <- (s_x*(n_y - s_y) + s_y*(n_x - s_x))/(2*N_h)

  sato_var <- (d_mh*sum(p_h) + sum(q_h))/sum(n_x*n_y/N_h)^2

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
      statistic = z_stat,
      p.value = p.value,
      method =
        glue::glue("Mentel-Haenszel Confidence Interval, Sato Variance")
    ),
    class = c("cmh-common-risk-diff", "ci_prop_common_risk_diff_sato", "cicalc")
  )
}
