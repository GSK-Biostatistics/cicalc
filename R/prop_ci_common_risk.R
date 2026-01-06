#' Mantel-Haenszel Common Risk Difference Confidence Interval
#'
#' Calculates the confidence interval for the Mantel-Haenszel estimate of the
#' common risk difference across multiple 2x2 tables (strata), using the Sato
#' or Independent Binomial variance estimator.
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
#' The Cochran Independent Binomial variance is:
#'
#' \deqn{\hat{\sigma}^2(\hat{\delta}_{C}) = \sum_{k} w_k^2 \left[
#' \frac{\hat{p}_{1k}(1 - \hat{p}_{1k})}{n_{1k}} +
#' \frac{\hat{p}_{2k}(1 - \hat{p}_{2k})}{n_{2k}}
#' \right]}
#'
#' where \eqn{\hat{p}_{1k} = \frac{s_{xk}}{n_{xk}}}
#' and \eqn{\hat{p}_{2k} = \frac{s_{yk}}{n_{yk}}}.
#'
#' The confidence interval is then \eqn{\hat{\delta}_{MH} \pm z_{1-\alpha/2} \sqrt{\hat{\sigma}^2(\hat{d}_{MH})}}.
#'
#' @inheritParams ci_prop_diff_mn_strata
#' @param sato_var (`logical`)\cr Use Sato variance estimate
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The Mantel-Haenszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{variance}{Variance estimate}
#'   \item{statistic}{Z-Statistic under the null hypothesis, assuming a common risk difference of 0}
#'   \item{p.value}{p-value under the null hypothesis, assuming a common risk difference of 0}
#'   \item{method}{Description of the method used ("Mantel-Haenszel Confidence Interval, Sato Variance")
#'   or ("Mantel-Haenszel Confidence Interval, Independent Binomial") }
#' @export
#' @references
#' Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley & Sons, Hoboken, NJ p. 231
#' Cochran, W.G. (1954). The Combination of estimates from different experiments. Biometrics, 10(1), p.101-129
#'
#' @examples
#' # Generate binary samples with strata
#' responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
#' arm <- rep(c("treat", "control"), 20)
#' strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))
#'
#' # Calculate common risk difference
#' ci_prop_diff_mh_strata(x = responses, by = arm, strata = strata)
#' # Calculate risk difference with independent binomial variance
#' ci_prop_diff_mh_strata(x = responses, by = arm, strata = strata, sato_var = FALSE)
ci_prop_diff_mh_strata <- function(x, by, strata, conf.level = 0.95, sato_var = TRUE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    return(
      ci_prop_diff_mh_strata(
        x = x ,
        by = by ,
        strata = strata,
        conf.level = conf.level,
        sato_var = sato_var
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
    p1 <- s_y/n_y
    p2 <- s_x/n_x
    est_var <- sum(((p1 * (1 - p1) / n_y) + (p2 * (1 - p2) / n_x)) * weights_k^2)
    var_title <- ", Independent Binomial"
  }


  alpha <- 1 - conf.level
  z_alpha <- stats::qnorm((1 + conf.level) / 2)

  lower_ci <- d_mh - z_alpha*sqrt(est_var)
  upper_ci <- d_mh + z_alpha*sqrt(est_var)

  z_stat <- d_mh/sqrt(sato_var)

  p.value <- 2 * (1 - stats::pnorm(abs(z_stat)))

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
      weights = weights_k,
      method =
        glue::glue("Mantel-Haenszel Risk Difference Confidence Interval{var_title}")
    ),
    class = c("ci_prop_diff_mh_strata", "cicalc")
  )
}

#' Mantel-Haenszel Stratified Relative Risk Confidence Interval
#'
#' Calculates the confidence interval for the Mantel-Haenszel estimate of the
#' common relative risk across multiple 2x2 tables (strata)
#'
#' The Mantel-Haenszel relative risk difference is computed as:
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
#'   \item{estimate}{The Mantel-Haenszel estimated common risk difference}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{variance}{Mantel-Haenszel variance estimate \eqn{Var(log(RR_MH))}}
#'   \item{method}{Description of the method used ("Mantel-Haenszel Common Relative Risk Confidence Interval")}
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

#' Stratified Newcombe Common Risk Difference Confidence Interval
#'
#' Calculates the stratified Newcombe confidence interval for unequal proportions
#' as described in Yan X, Su XG. Stratified Wilson and Newcombe confidence intervals
#' or multiple binomial proportions. Weights are estimated using CMH or Wilson methods.
#'
#' \deqn{
#' L = \hat{d}_{\rm MH} - z_{\alpha/2} \sqrt{
#'   \sum_h w_h^2 L_{2h} (1 - L_{2h}) +
#'   \sum_h w_h^2 U_{1h} (1 - U_{1h})
#' }
#' }
#'
#' \deqn{
#' U = \hat{d}_{\rm MH} + z_{\alpha/2} \sqrt{
#'   \sum_h w_h^2 L_{2h} (1 - L_{2h}) +
#'   \sum_h w_h^2 U_{1h} (1 - U_{1h})
#' }
#' }
#'
#' Where:
#' - \eqn{\hat{d}_{\rm MH}}: Mantel-Haenszel common risk difference
#' - \eqn{z_{\alpha/2}}: standard normal critical value
#' - \eqn{w_h}: stratum weights
#' - \eqn{L_{2h}}, \eqn{U_{1h}}: Wilson-type CI limits for stratum h
#'
#' @inheritParams ci_prop_diff_mn_strata
#' @param weights_method (`character`)\cr Can be either "wilson" or "cmh" and directs the way weights are estimated.
#' @param correct (scalar `logical`)\cr include the continuity correction. For further information, see for example
#'   [ci_prop_diff_nc())].
#'
#' @return An object containing the following components:
#'
#'   \item{n}{Number of responses}
#'   \item{N}{Total number}
#'   \item{estimate}{The point estimate of the proportion}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{weights}{Weights of each strata calculated as per the specified "weights_method" argument.}
#'   \item{method}{Type of method used}
#'
#' @examples
#' set.seed(1)
#' rsp <- sample(c(TRUE, FALSE), 100, TRUE)
#' grp <- sample(c("Placebo", "Treatment"), 100, TRUE)
#' strata_data <- data.frame(
#'   "f1" = sample(c("a", "b"), 100, TRUE),
#'   "f2" = sample(c("x", "y", "z"), 100, TRUE),
#'   stringsAsFactors = TRUE
#' )
#' strata <- interaction(strata_data)
#'
#' ci_prop_diff_nc_strata(
#'   x = rsp, by = grp, strata = strata, weights_method = "cmh",
#'   conf.level = 0.95
#' )
#'
#' @export
ci_prop_diff_nc_strata <- function(x,
                                   by,
                                   strata,
                                   conf.level = 0.95,
                                   correct = FALSE,
                                   weights_method = c("wilson", "cmh"),
                                   data = NULL) {
  set_cli_abort_call()

  weights_method <- match.arg(weights_method)

  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_nc(
        x = x,
        by = by,
        conf.level = conf.level,
        correct = correct
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


  if (any(tapply(x, strata, length) < 5)) {
    cli::cli_warn("Less than 5 observations in some strata.")
  }

  rsp_by_grp <- split(x, f = by)
  strata_by_grp <- split(strata, f = by)

  # Finding the weights
  weights <- if (identical(weights_method, "cmh")) {
    ci_prop_diff_mh_strata(
      x = x,
      by = by,
      strata = strata,
      conf.level = conf.level
    )$weights
  } else if (identical(weights_method, "wilson")) {
    ci_prop_wilson_strata(
      x = x,
      strata = strata,
      conf.level = conf.level,
      correct = correct
    )$weights
  }
  weights[levels(strata)[!levels(strata) %in% names(weights)]] <- 0

  # reformat method for printing
  weights_method <- if(weights_method == "cmh"){
    "CMH"
  } else{
    "Wilson"
  }

  # Calculating lower (`l`) and upper (`u`) confidence bounds per group.
  strat_wilson_by_grp <- Map(
    ci_prop_wilson_strata,
    x = rsp_by_grp,
    strata = strata_by_grp,
    conf.level = conf.level,
    correct = correct,
    weights = list(weights, weights)
  )

  l_1 <- strat_wilson_by_grp[[1]]$conf.low
  u_1 <- strat_wilson_by_grp[[1]]$conf.high
  l_2 <- strat_wilson_by_grp[[2]]$conf.low
  u_2 <- strat_wilson_by_grp[[2]]$conf.high

  # Estimating the diff and n_1, n_2 (it allows different weights to be used)
  t_tbl <- table(
    factor(x, levels = c("FALSE", "TRUE")),
    by,
    strata
  )
  n_1 <- colSums(t_tbl[1:2, 1, ])
  n_2 <- colSums(t_tbl[1:2, 2, ])
  use_stratum <- (n_1 > 0) & (n_2 > 0)
  n_1 <- n_1[use_stratum]
  n_2 <- n_2[use_stratum]
  p_1 <- t_tbl[2, 1, use_stratum] / n_1
  p_2 <- t_tbl[2, 2, use_stratum] / n_2
  est1 <- sum(weights * p_1)
  est2 <- sum(weights * p_2)
  diff_est <- est2 - est1

  lambda1 <- sum(weights^2 / n_1)
  lambda2 <- sum(weights^2 / n_2)
  z <- stats::qnorm((1 + conf.level) / 2)

  lower <- diff_est - z * sqrt(lambda2 * l_2 * (1 - l_2) + lambda1 * u_1 * (1 - u_1))
  upper <- diff_est + z * sqrt(lambda1 * l_1 * (1 - l_1) + lambda2 * u_2 * (1 - u_2))

  # Return values
  structure(
    list(
      N = length(x),
      n = sum(x),
      estimate = diff_est,
      conf.low = lower,
      conf.high = upper,
      conf.level = conf.level,
      weights = weights,
      method =
        glue::glue("Stratified Newcombe Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction, {weights_method}")
    ),
    class = c("stratified_newcombe", "stratified_wilson", "cicada")
  )
}
