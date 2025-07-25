#' Miettinen-Nurminen Confidence Interval for Difference in Proportions
#'
#' Calculates the Miettinen-Nurminen (MN) confidence interval for the difference
#' between two proportions. This method can be more accurate than traditional
#' methods, especially with small sample sizes or proportions close to 0 or 1.
#'
#' @inheritParams ci_prop_wald
#' @param by (`string`) \cr A character or factor vector with exactly two unique levels
#'   identifying the two groups to compare. Can also be a column name if a data
#'   frame provided in the `data` argument.
#' @param delta (`numeric`) \cr Optionally a single number or a vector of
#'   numbers between -1 and 1 (not inclusive) to set the difference between two
#'   groups under the null hypothesis. If provided, the function returns the
#'   test statistic and p-value under the `delta`
#'   hypothesis.
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The point estimate of the difference in proportions (p_x - p_y)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{delta}{delta value(s) used}
#'   \item{statistic}{Z-Statistic under the null hypothesis based on the given 'delta'}
#'   \item{p.value}{p-value under the null hypothesis based on the given 'delta'}
#'   \item{method}{Description of the method used ("Miettinen-Nurminen Confidence Interval")}
#'
#' If `delta` is not provided statistic and p.value will be NULL
#'
#'
#' @details The function implements the Miettinen-Nurminen method to compute
#' confidence intervals for the difference between two proportions. This approach:
#' \itemize{
#' \item Calculates the Miettinen-Nurminen score test statistic for different
#' possible values of the proportion difference (delta)
#' \item Identifies the delta values where the test statistic equals the critical
#' value corresponding to the desired confidence level
#' \item Returns these boundary values as the confidence interval limits
#' }
#'
#' The method uses a score test with a small-sample correction factor, making it
#' more accurate than normal approximation methods, especially for small samples
#' or extreme proportions. The equation for the test statistics is as follows:
#'
#' \deqn{H_0: \hat{d}-\delta <= 0 \qquad \text{vs.} \qquad H_1: \hat{d}-\delta > 0}
#'
#' \deqn{ T_\delta = \frac{\hat{p_x} - \hat{p_y} - \delta}{\sigma_{mn}(\delta)}}
#'
#' where \eqn{\hat{p_*} = s_*/n_*} represent the observed number of successes
#' divided by the number of participant in that group. The \eqn{\sigma_{mn}(\delta)} is a
#' function of the delta values and is create with the following equation"
#' \eqn{\tilde{p_*}} represent the MLE of the proportions.
#' \deqn{
#' \sigma_{mn}(\delta) = \sqrt{\left[\frac{\tilde{p_y}(1-\tilde{p_y})}{n_x}+\frac{\tilde{p_x}(1-\tilde{p_x})}{n_y} \right]\left(\frac{N}{N-1}\right)}
#' }
#' \eqn{ \tilde{p_x} = 2p\cdot{cos(a)} - \frac{L_2}{3L_3}} and \eqn{ \tilde{p_y} = \tilde{p_x} + \delta}
#' where:
#' \itemize{
#'  \item \eqn{p = \pm \sqrt{\frac{L_2^2}{(3L_3)^2} - \frac{L_1}{3L_3}}}
#'  \item \eqn{a = 1/3[\pi + cos^{-1}(q/p^3)]}
#'  \item \eqn{q = \frac{L_2^3}{(3L_3)^3} - \frac{L_1L_2}{6L_3^2} + \frac{L_0}{2L_3}}
#'  \item \eqn{L_3 = n_x + n_y }
#'  \item \eqn{L_2 = (n_x + 2 n_y)\delta - N - (s_x + s_y)}
#'  \item \eqn{L_1 = (n_y\delta - L_3 - 2s_y)\delta + s_x + s_y}
#'  \item \eqn{L_0 = s_y\delta(1-\delta)}
#' }
#'
#' For more information about these equations see Miettinen (1985)
#'
#'
#' @references Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of
#' two rates. Statistics in Medicine, 4(2), 213-226.
#'
#'
#' @examples
#' # Generate binary samples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_mn(x = responses, by = arm)
#'
#' # Calculate 99% confidence interval
#' ci_prop_diff_mn(x = responses, by = arm, conf.level = 0.99)
#'
#' # Calculate the p-value under the null hypothesis delta = -0.1
#' ci_prop_diff_mn(x = responses, by = arm, delta = -0.1)
#'
#' # Calculate from a data.frame
#' data <- data.frame(responses, arm)
#' ci_prop_diff_mn(x = responses, by = arm, data = data)
#' @export
ci_prop_diff_mn <- function(x, by, conf.level = 0.95, delta = NULL, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_mn(
        x = x ,
        by = by ,
        conf.level = conf.level,
        delta = delta
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
  check_numeric(delta, allow_empty = TRUE)
  check_range(delta,allow_empty = TRUE,
              range = c(-1, 1), include_bounds = c(FALSE, FALSE))

  # convert vectors to count data
  df <- get_counts(x = x, by = by)

  alpha <- 1 - conf.level

  if(df$response_1 == 0 | df$response_2 == 0){
    # Manually count at 0
    z <- stats::qnorm((1 + conf.level) / 2)

    delta_vec <- seq(-0.99999, 0.99999, length.out = 1000000)
    T_scores <- test_score_mn(s_x = df$response_1, n_x = df$n_1,
                              s_y = df$response_2, n_y = df$n_2, delta = delta_vec)
    potential_vals <- delta_vec[which(-z < T_scores & T_scores < z)]
    lower_ci <- min(potential_vals)
    upper_ci <- max(potential_vals)
  } else {
    lower_ci <- stats::uniroot(z_distance, interval=c(-0.999,0.999),
                                       fx=test_score_mn,
                                       ref_z = stats::qnorm(1 - alpha / 2),
                                       s_x = df$response_1, n_x = df$n_1,
                                       s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root

    upper_ci <- stats::uniroot(z_distance, interval=c(lower_ci,0.999999),
                                       fx=test_score_mn,
                                       ref_z = stats::qnorm(alpha / 2),
                                       s_x = df$response_1, n_x = df$n_1,
                                       s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root

  }

  statistic = NULL
  p.value = NULL

  if(!is.null(delta)){
    check_not_missing(delta)
    statistic <- test_score_mn(s_x = df$response_1, n_x = df$n_1,
                               s_y = df$response_2, n_y = df$n_2, delta = delta)
    p.value <- (1 - stats::pnorm(abs(statistic)))
  }

  # Output
  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = df$response_1/df$n_1 - df$response_2/ df$n_2,
      conf.low = lower_ci,
      conf.high = upper_ci,
      conf.level = conf.level,
      delta = delta,
      statistic = statistic,
      p.value = p.value,
      method =
        glue::glue("Miettinen-Nurminen Confidence Interval")
    ),
    class = c("miettinen-nurminen", "prop_ci_bi", "cicada")
  )

}

#' Calculate Miettinen-Nurminen Test Statistic
#'
#' This function calculates the Miettinen-Nurminen test statistic for a given
#' delta (difference in proportions) using the method described in Miettinen and
#' Nurminen (1985).
#'
#' @param s_x A numeric vector of successes in the first group.
#' @param n_x A numeric vector of sample sizes in the first group.
#' @param s_y A numeric vector of successes in the second group.
#' @param n_y A numeric vector of sample sizes in the second group.
#' @param delta A numeric value representing the hypothesized difference in proportions (p_x - p_y).
#'              Must be between -1 and 1, inclusive.
#'
#' @return A numeric value representing the Miettinen-Nurminen test statistic.
#'
#'
#' @references
#' Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#'
#' @keywords internal
#' @noRd
test_score_mn <- function(s_x, n_x, s_y, n_y, delta){
  p_hat_x <- s_x / n_x
  p_hat_y <- s_y / n_y

  var_delta <- variance_mn(s_x, n_x, s_y, n_y, delta)

  T_stat <- (p_hat_x - p_hat_y - delta) / sqrt(var_delta)
  T_stat
}


#' Stratified Miettinen-Nurminen Confidence Interval for Difference in Proportions
#'
#' Calculates Stratified Miettinen-Nurminen (MN) confidence intervals and
#' corresponding point estimates for the difference between two proportions
#'
#' @inheritParams ci_prop_diff_mn
#' @param strata (`numeric`) \cr A vector specifying the stratum for each observation. It needs
#'   to be the length of x or a multiple of x if multiple levels of strata are
#'   present. Can also be a column name (or vector of column names NOT quoted)
#'   if a data frame provided in the `data` argument.
#' @param method (`string`) \cr Specifying how the CIs should be calculated. It must
#'   equal either 'score' or 'summary score'. See details for more information
#'   about the implementation differences.
#'
#'
#' @return An object containing the following components:
#'
#'   \item{estimate}{The point estimate of the difference in proportions (p_x -
#'   p_y)} \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used} \item{delta}{delta value(s) used}
#'   \item{statistic}{Z-Statistic under the null hypothesis based on the given 'delta'}
#'   \item{p.value}{p-value under the null hypothesis based on the given 'delta'}
#'   \item{method}{Description of the method used ("Stratified \{method\}
#'   Miettinen-Nurminen Confidence Interval")}
#'
#'   If `delta` is not provided statistic and p.value will be NULL
#'
#'
#' @details The function implements the stratified Miettinen-Nurminen method to compute
#'   confidence intervals for the difference between two proportions across multiple strata.
#'   \deqn{H_0: \hat{d}-\delta <= 0 \qquad \text{vs.} \qquad H_1: \hat{d}-\delta > 0}
#'
#'   The "score" method is a weighted MN score first described in the original 1985
#'   paper. The formula is:
#'   \itemize{
#'     \item Calculates weights for each stratum as \eqn{w_i = \frac{n_{xi} \cdot n_{yi}}{n_{xi} + n_{yi}}}
#'     \item Computes the overall weighted difference \eqn{\hat{d} = \frac{\sum w_i \hat{p}_{xi}}{\sum w_i} -
#'           \frac{\sum w_i \hat{p}_{yi}}{\sum w_i}}
#'     \item Uses the stratified test statistic: \deqn{Z_{\delta} = \frac{\hat{d} - \delta}
#'           {\sqrt{\sum_{i=1}^k \left(\frac{w_i}{\sum w_i}\right)^2 \cdot \hat{\sigma}_{mn}^2({d})}}}
#'     \item Finds the range of all values of \eqn{\delta} for which the stratified test statistic (\eqn{Z_\delta})
#'           falls in the acceptance region \eqn{\{ Z_\delta < z_{\alpha/2}\}}
#'   }
#'
#'   The \eqn{\hat{\sigma}_{mn}^2(\hat{d})} is the Miettinen-Nurminen variance estimate.
#'   See the details of [ci_prop_diff_mn()] for how \eqn{\hat{\sigma}_{mn}^2(\delta)} is calculated.
#'
#'   The "summary score" method follows the meta-analyses proposed in Agresti
#'   2013 and is consistent with the "Summary Score Confidence Limits" method
#'   used in SAS. The formula is:
#'   \itemize{
#'   \item The point estimate of the stratified risk difference is a weighted average of the midpoints of the within-stratum MN confidence intervals:
#'     \deqn{
#'       \hat{d}_{\text{S}} = \sum_i \hat{d}_i w_i
#'       }
#'   \item Define \eqn{s_i} as the width of the CI for the \eqn{i}th stratum divided by \eqn{2 \times z_{\alpha/2}} and then stratum weights are given by
#'   \deqn{
#'     w_i = \left( \frac{1}{s_i^2} \right) \bigg/ \sum_i \left( \frac{1}{s_i^2} \right)
#'     }
#'   \item The variance of \eqn{\hat{d}_{\text{S}} } is computed as
#'   \deqn{
#'      \widehat{\text{Var}}(\hat{d}_{\text{S}}) = \frac{1}{\sum_i \left( \frac{1}{s_i^2} \right) }
#'     }
#'   \item Confidence limits for the stratified risk difference estimate are
#'   \deqn{
#'     \hat{d}_{\text{S}} \pm \left( z_{\alpha /2} \times \widehat{\text{Var}}(\hat{d}_{\text{S}}) \right)
#'     }
#'   }
#'
#'
#' @references
#' Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#' \href{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details63.htm}{Common Risk Difference :: Base SAS(R) 9.4 Procedures Guide: Statistical Procedures, Third Edition}
#'
#' Agresti, A. (2013). Categorical Data Analysis. 3rd Edition. John Wiley & Sons, Hoboken, NJ
#'
#' @examples
#' # Generate binary samples with strata
#' responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
#' arm <- rep(c("treat", "control"), 20)
#' strata <- rep(c("stratum1", "stratum2"), times = c(20, 20))
#'
#' # Calculate stratified confidence interval for difference in proportions
#' ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata)
#'
#' # Using the summary score method
#' ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
#'                       method = "summary score")
#'
#' # Calculate 99% confidence interval
#' ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
#'                       conf.level = 0.99)
#'
#' # Calculate p-value under null hypothesis delta = 0.2
#' ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata,
#'                       delta = 0.2)
#'
#' @export
ci_prop_diff_mn_strata <- function(x, by, strata, method = c("score", "summary score"),
                                   conf.level = 0.95, delta = NULL, data = NULL){
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    return(
      ci_prop_diff_mn_strata(
        x = x ,
        by = by ,
        strata = strata,
        method = method,
        conf.level = conf.level,
        delta = delta
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }
  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(by)
  check_not_missing(strata)
  check_binary(x)
  check_n_levels(by, n_levels = 2)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_identical_length(x, by)
  check_numeric(delta, allow_empty = TRUE)
  check_range(delta,allow_empty = TRUE,
              range = c(-1, 1), include_bounds = c(FALSE, FALSE))

  method <- match.arg(method, c("score", "summary score"))
  alpha <- 1 - conf.level

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_mn_strata(
        x = x ,
        by = by ,
        strata = strata,
        conf.level = conf.level,
        delta = delta
      ) |>
        substitute() |>
        eval(envir = data, enclos = parent.frame())
    )
  }

  strata <- combine_strata(x, strata)

  statistic <- NULL
  p.value <- NULL
  if(method == "score"){
    # Get the n and response for each by and strata
    by_strata <- interaction(by, strata) |>
      unique()
    if(length(by_strata) == 2){
      cli::cli_abort("{.arg by} and {.arg strata} are non-unique check inputs")
    }
    response_df <- get_counts(x = x, by = by, strata = strata)
    n_x <- response_df$n_1
    s_x <- response_df$response_1
    n_y <- response_df$n_2
    s_y <- response_df$response_2
    # Calculate weights and diff in weighted proportions
    weights <-(n_x * n_y) / (n_x + n_y)
    names(weights) <- response_df$strata
    tot_w <- sum(weights)
    diff <- sum(s_x/n_x*weights)/tot_w - sum(s_y/n_y*weights)/tot_w

    # Calculate confidence interval
    lower_ci <- stats::uniroot(z_distance, interval=c(-0.999,0.999),
                                      fx=test_score_mn_weighted,
                                      ref_z = stats::qnorm(1 - alpha / 2),
                                      s_x = s_x, n_x = n_x,
                                      s_y = s_y, n_y = n_y, w = weights, tol=1e-08)$root

    upper_ci <- stats::uniroot(z_distance, interval=c(-0.999,0.999),
                                      fx=test_score_mn_weighted,
                                      ref_z = stats::qnorm(alpha / 2),
                                      s_x = s_x, n_x = n_x,
                                      s_y = s_y, n_y = n_y, w = weights, tol=1e-08)$root

    if(!is.null(delta)){
      statistic <- purrr::map_dbl(delta, \(d){
        test_score_mn_weighted(s_x = s_x, n_x = n_x,
                               s_y = s_y, n_y = n_y, w = weights, delta = d)
      })
      p.value <- (1 - stats::pnorm(abs(statistic)))
    }
  } else if(method == "summary score") {

    #SAS PROC FREQ Summary Score Estimate of the Common Risk Difference
    #https://support.sas.com/documentation/cdl/en/procstat/67528/HTML/default/viewer.htm#procstat_freq_details63.htm
    estimate_df <- dplyr::tibble(
      x = x,
      by = as.numeric(as.factor(by)),
      strata = strata
    ) |>
      dplyr::group_by(strata) |>
      dplyr::summarise(mn = list(ci_prop_diff_mn(x, by, conf.level =conf.level))) |>
      dplyr::mutate(
        low = purrr::map_dbl(.data$mn, "conf.low"),
        high = purrr::map_dbl(.data$mn, "conf.high"),
        width = .data$high - .data$low,
        dh = .data$low + .data$width/2,
        sh = .data$width/(2*stats::qnorm(1-alpha/2)),
        w = (1/.data$sh^2)/sum(1/.data$sh^2)
      )
    weights <- estimate_df$w
    names(weights) <- estimate_df$strata

    estimate <- estimate_df |>
      dplyr::summarise(dS = sum(.data$dh*.data$w),
                       var_ds = 1/sum(1/.data$sh^2))


    # Calculate confidence interval
    lower_ci <- estimate$dS - stats::qnorm(1-alpha/2)*sqrt(estimate$var_ds)
    upper_ci <- estimate$dS + stats::qnorm(1-alpha/2)*sqrt(estimate$var_ds)
    diff <- estimate$dS
    if(!is.null(delta)){
      response_df <- get_counts(x = x, by = by, strata = strata)
      n_x <- response_df$n_1
      s_x <- response_df$response_1

      n_y <- response_df$n_2
      s_y <- response_df$response_2
      # Calculate weights and diff in weighted proportions
      w <-(n_x * n_y) / (n_x + n_y)

      statistic <- test_score_mn_weighted(s_x = s_x, n_x = n_x,
                                          s_y = s_y, n_y = n_y, w = w, delta = delta)
      p.value <- (1 - stats::pnorm(abs(statistic)))
    }

  }
  df <- get_counts(x = x, by = by)
  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      weights = weights,
      estimate = diff,
      conf.low = lower_ci,
      conf.high = upper_ci,
      conf.level = conf.level,
      delta = delta,
      statistic = statistic,
      p.value = p.value,
      method =
        glue::glue("Stratified {stringr::str_to_title(method)} Miettinen-Nurminen Confidence Interval")
    ),
    class = c("stratified_miettinen_nurminen", "prop_ci_bi", "cicada")
  )

}

#' Calculate Stratified Miettinen-Nurminen Test Statistic
#'
#' The stratified Miettinen-Nurminen z test statistic
#' for a given delta (difference in proportions) using the method described in
#' Miettinen and Nurminen (1985), adapted for stratified data.
#'
#' @param s_x A numeric vector of successes in the first group, one per stratum.
#' @param n_x A numeric vector of sample sizes in the first group, one per stratum.
#' @param s_y A numeric vector of successes in the second group, one per stratum.
#' @param n_y A numeric vector of sample sizes in the second group, one per stratum.
#' @param delta A numeric value representing the hypothesized difference in proportions (p_x - p_y).
#'        Must be between -1 and 1, inclusive.
#'
#' @return A numeric value representing the stratified Miettinen-Nurminen Z-test statistic.
#'
#' @details
#' The function implements the stratified version of the Miettinen-Nurminen (MN) score test
#' for the difference between two proportions. The stratified test combines information across
#' multiple strata using appropriate weighting.
#'
#'
#' @references
#' Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#' @keywords internal
#' @noRd
test_score_mn_weighted<-function(s_x, n_x, s_y, n_y, w, delta){
  tot_w <- sum(w)
  diff <- sum(s_x/n_x*w)/tot_w - sum(s_y/n_y*w)/tot_w

  mV <- variance_mn(s_x, n_x, s_y, n_y, delta)

  #equation 15
  den <- ((w/tot_w)^2)*mV
  tot_den <- sum(den)

  zstat <- (diff-delta)/sqrt(tot_den)
  zstat
}



#' @keywords internal
#'
variance_mn <- function(s_x, n_x, s_y, n_y, delta){
  # The implementation follows equations described in Miettinen and Nurminen's
  # paper, specifically equations 8, 15, 27, and 28 for handling stratified
  # data. Variable names have been changed to be more consistent with the rest
  # of the package
  N <- n_x + n_y
  tot_s <- s_x + s_y

  #equation 27
  L3 <- N
  L2 <- (n_x + 2*n_y)*delta - N - tot_s
  L1 <- (n_y*delta - N - 2*s_y)*delta + tot_s
  L0 <- s_y*delta*(1-delta)

  #equation 28
  q <- (L2^3)/((3*L3)^3) - (L1*L2)/(6*(L3^2)) + L0/(2*L3)

  p <- sign(q)*sqrt((L2^2)/((3*L3)^2) - L1/(3*L3))

  temp <- pmax(pmin(q/(p^3),1),-1)
  a <- (1/3)*(pi+acos(temp))

  # The MLE of p_x and p_y
  mle_p_x <- 2*p*cos(a) - L2/(3*L3)
  mle_p_y <-  mle_p_x+delta

  #equation 8
  var_delta <- (mle_p_y*(1-mle_p_y)/n_x + mle_p_x*(1-mle_p_x)/n_y) * (N/(N-1))
  var_delta
}


#' @keywords internal
z_distance <- function(delta, fx, ref_z, ...){
  z_delta <- fx(delta = delta, ...)
  ref_z - z_delta
}


