#' Miettinen-Nurminen Confidence Interval for Difference in Proportions
#'
#' Calculates the Miettinen-Nurminen (MN) confidence interval for the difference
#' between two proportions. This method can be more accurate than traditional
#' methods, especially with small sample sizes or proportions close to 0 or 1.
#'
#' @param x A binary vector representing all responses. Can also be a column
#'   name if a data frame provided in the `data` argument.
#' @param by A character or factor vector with exactly two unique levels
#'   identifying the two groups to compare. Can also be a column name if a data
#'   frame provided in the `data` argument.
#' @param conf.level A numeric value between 0 and 1 specifying the confidence
#'   level. Default is 0.95 (95\% confidence).
#' @param delta Optional numeric value(s) for hypothesis testing. If provided, the
#'   function returns the test statistic and p-value under the `delta`
#'   hypothesis.
#' @param data Optional data frame containing the variables specified in `x` and `by`.
#
#'
#' @return A list containing the following components:
#'
#'   \item{estimate}{The point estimate of the difference in proportions (p_x - p_y)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Description of the method used ("Miettinen-Nurminen Confidence Interval")}
#' If `delta` is provided the list will have two additional compnents:
#'   \item{statistic}{Z-Statistic under the given `delta` null hypothesis}
#'   \item{p.value}{p-value under the given `delta` null hypothesis}
#'
#'
#' @details The function implements the Miettinen-Nurminen method to compute
#' confidence intervals for the difference between two proportions. This
#' approach:
#'
#' 1. Computes the test statistic across a range of possible difference values
#' (delta) 2. Identifies values of delta where the test statistic falls within
#' the critical region determined by the confidence level 3. Returns the minimum
#' and maximum acceptable values as the confidence interval bounds
#'
#' The method uses a score test with a small-sample correction factor, making it
#' more accurate than normal approximation methods, especially for small samples
#' or extreme proportions. The equation for the test statistics is as follows:
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

  lower_ci <- ifelse( df$response_1 > 0,
                      stats::uniroot(z_distance, interval=c(-0.999,0.999),
                      fx=test_score_mn,
                      ref_z = stats::qnorm(1 - alpha / 2),
                      s_x = df$response_1, n_x = df$n_1,
                      s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root,
                      -1 )

  upper_ci <- ifelse( df$response_2 > 0,
    stats::uniroot(z_distance, interval=c(lower_ci,0.999999),
                      fx=test_score_mn,
                      ref_z = stats::qnorm(alpha / 2),
                      s_x = df$response_1, n_x = df$n_1,
                      s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root,
    1)

  statistic = NULL
  p.value = NULL

  if(!is.null(delta)){
    check_not_missing(delta)
    statistic <- test_score_mn(s_x = df$response_1, n_x = df$n_1,
                               s_y = df$response_2, n_y = df$n_2, delta = delta)
    p.value <- (1 - stats::pnorm(abs(statistic)))
  }

  # Output
  list(
    estimate = df$response_1/df$n_1 - df$response_2/ df$n_2,
    conf.low = lower_ci,
    conf.high = upper_ci,
    conf.level = conf.level,
    delta = delta,
    statistic = statistic,
    p.value = p.value,
    method =
      glue::glue("Miettinen-Nurminen Confidence Interval")
  )

}

#' Calculate Miettinen-Nurminen Test Statistic
#'
#' @description This function calculates the Miettinen-Nurminen test statistic for a given
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
#' @details
#' The function implements the Miettinen-Nurminen (MN) score test for the
#' difference between two proportions. The MN test statistic is calculated as:
#'
#'
#' The restricted MLEs are found by solving a cubic equation as implemented in
#' the function. The test statistic follows approximately a standard normal
#' distribution under the null hypothesis.
#'
#' @references
#' Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#' Chen, Y., & Zhou, X. (2016). Interval Estimation for the Difference Between
#' Independent Proportions. Western Users of SAS Software Conference Proceedings 2016.
#' https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
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

ci_prop_diff_mn_strata <- function(x, by, strata, method = c("score", "summary score"),
                                   conf.level = 0.95, delta = NULL, data = NULL){
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)
  if(is.data.frame(data)){
    with(data, ci_prop_diff_mn_strata(x = x, by = by, strata = strata,
                                      conf.level = conf.level, delta = delta))
  }

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_not_missing(by)
  check_not_missing(strata)
  check_binary(x)
  check_n_levels(by, n_levels = 2)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_identical_length(x, by)
  check_identical_length(x, strata)
  check_numeric(delta, allow_empty = TRUE)
  check_range(delta,allow_empty = TRUE,
              range = c(-1, 1), include_bounds = c(FALSE, FALSE))

  method <- match.arg(method, c("score", "summary score"))
  alpha <- 1 - conf.level

  statistic <- NULL
  p.value <- NULL
  if(method == "score"){
    # Get the n and response for each by and strata
    response_df <- get_counts(x = x, by = by, strata = strata)
    n_x <- response_df$n_1
    s_x <- response_df$response_1

    n_y <- response_df$n_2
    s_y <- response_df$response_2
    # Calculate weights and diff in weighted proportions
    w <-(n_x * n_y) / (n_x + n_y)
    tot_w <- sum(w)
    diff <- sum(s_x/n_x*w)/tot_w - sum(s_y/n_y*w)/tot_w

    # Calculate confidence interval
    lower_ci <- ifelse(s_x > 0,
      stats::uniroot(z_distance, interval=c(-0.999,0.999),
                        fx=test_score_mn_weighted,
                        ref_z = stats::qnorm(1 - alpha / 2),
                        s_x = s_x, n_x = n_x,
                        s_y = s_y, n_y = n_y, w = w, tol=1e-08)$root,
      -1)

    upper_ci <- ifelse(s_y > 0,
      stats::uniroot(z_distance, interval=c(-0.999,0.999),
                        fx=test_score_mn_weighted,
                        ref_z = stats::qnorm(alpha / 2),
                        s_x = s_x, n_x = n_x,
                        s_y = s_y, n_y = n_y, w = w, tol=1e-08)$root,
      1)

    if(!is.null(delta)){
      statistic <- test_score_mn_strata(s_x = s_x, n_x = n_x,
                                        s_y = s_y, n_y = n_y, w = w, delta = delta)
      p.value <- (1 - stats::pnorm(abs(statistic)))
    }
  } else if(method == "summary score") {
    #SAS PROC FREQ Summary Score Estimate of the Common Risk Difference
    #https://support.sas.com/documentation/cdl/en/procstat/67528/HTML/default/viewer.htm#procstat_freq_details63.htm
    estimate <- dplyr::tibble(
      x = x,
      by = as.numeric(as.factor(by)),
      strata = strata
    ) |>
      dplyr::group_by(strata) |>
      dplyr::summarise(mn = list(ci_prop_diff_mn(x, by, conf.level =conf.level))) |>
      dplyr::mutate(
             low = purrr::map_dbl(mn, "conf.low"),
             high = purrr::map_dbl(mn, "conf.high"),
             width = high - low,
             dh = low + width/2,
             sh = width/(2*stats::qnorm(1-alpha/2)),
             w = (1/sh^2)/sum(1/sh^2)
        ) |>
      dplyr::summarise(dS = sum(dh*w),
                       var_ds = 1/sum(1/sh^2))


    # Calculate confidence interval
    lower_ci <- estimate$dS - stats::qnorm(1-alpha/2)*sqrt(estimate$var_ds)
    upper_ci <- estimate$dS + stats::qnorm(1-alpha/2)*sqrt(estimate$var_ds)
    diff <- estimate$dS
    if(!is.null(delta)){
      statistic <- test_score_mn_strata(s_x = s_x, n_x = n_x,
                                        s_y = s_y, n_y = n_y, w = w, delta = delta)
      p.value <- (1 - stats::pnorm(abs(statistic)))
    }

  }

  list(
    estimate = diff,
    conf.low = lower_ci,
    conf.high = upper_ci,
    conf.level = conf.level,
    delta = delta,
    statistic = statistic,
    p.value = p.value,
    method =
      glue::glue("Stratified Miettinen-Nurminen Confidence Interval")
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


