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
#' \deqn{T_{MN} = \frac{\hat{p}_x - \hat{p}_y - \delta}{\sqrt{V_{\delta}}}}
#'
#' where:
#' \itemize{
#'   \item \eqn{\hat{p}_x} and \eqn{\hat{p}_y} are the observed proportions in the two groups
#'   \item \eqn{\delta} is the hypothesized difference
#'   \item \eqn{V_{\delta}} is the variance estimate under the null hypothesis
#'         \eqn{V_{\delta} = \lambda(\tilde{p}_x(1-\tilde{p}_x)/n_x + \tilde{p}_y(1-\tilde{p}_y)/n_y)}
#'   \item \eqn{\lambda = N/(N+1)} is a small sample correction factor
#'   \item \eqn{\tilde{p}_x} and \eqn{\tilde{p}_y} are the restricted maximum likelihood estimates
#'         under the constraint that \eqn{\tilde{p}_x - \tilde{p}_y = \delta}
#'   \item \eqn{\tilde{p}_x = 2ucos(w) - b/3a} and  \eqn{\tilde{p}_y = \tilde{p}_x - \delta}
#'   \item \eqn{w = \frac{\pi + cos^{-1}(v/u^3)}{3}}
#'   \item \eqn{v = \frac{b^3}{27a^3}-\frac{bc}{6a^2}+\frac{d}{2a}}
#'   \item \eqn{u = \frac{v}{|v|}\sqrt{\frac{b^2}{9a^2}+\frac{c}{3a}}}
#'   \item \eqn{a = 1 + \theta}
#'   \item \eqn{b = -(1 + \theta + \tilde{p}_x + \theta\tilde{p}_y + \delta(\theta + 2))}
#'   \item \eqn{c = \delta^2 + \delta(2\tilde{p}_x + \theta + 1) + \tilde{p}_x + \theta\tilde{p}_y}
#'   \item \eqn{d = -\tilde{p}_x\delta(1+\delta)}
#'   \item \eqn{\theta = n_y / n_x}
#'
#' }
#'
#' @references Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of
#' two rates. Statistics in Medicine, 4(2), 213-226.
#'
#' Chen, Y., & Zhou, X. (2016). Interval Estimation for the Difference Between
#' Independent Proportions. Western Users of SAS Software Conference Proceedings
#' 2016. https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
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

  # check x and by are equal length if data is null
  by_vals <- unique(by)
  y1 <- x[which(by == by_vals[1])]
  y2 <- x[which(by == by_vals[2])]
  r_1 = sum(y1); n_1 = length(y1)
  r_2 = sum(y2); n_2 = length(y2)

  z <- stats::qnorm((1 + conf.level) / 2)
  delta_vec <- seq(-0.99999, 0.99999, length.out = 1000000)
  T_scores <- test_score_mn(r_x = r_1, n_x = n_1,
                            r_y = r_2, n_y = n_2, delta = delta_vec)
  potential_vals <- delta_vec[which(-z < T_scores & T_scores < z)]

  if(!is.null(delta)){
    check_not_missing(delta)
    statistic = test_score_mn(r_x = r_1, n_x = n_1,
                              r_y = r_2, n_y = n_2, delta = delta)
    out <- list(
      estimate = sum(y1)/length(y1) - sum(y1) / length(y2),
      conf.low = min(potential_vals),
      conf.high = max(potential_vals),
      conf.level = conf.level,
      delta = delta,
      statistic = statistic,
      p.value = 2 * (1 - pnorm(abs(statistic))),
      method =
        glue::glue("Miettinen-Nurminen Confidence Interval")
    )
  } else {
    list(
      estimate = sum(y1)/length(y1) - sum(y1) / length(y2),
      conf.low = min(potential_vals),
      conf.high = max(potential_vals),
      conf.level = conf.level,
      method =
        glue::glue("Miettinen-Nurminen Confidence Interval")
    )
  }

}

#' Calculate Miettinen-Nurminen Test Statistic
#'
#' @description This function calculates the Miettinen-Nurminen test statistic for a given
#' delta (difference in proportions) using the method described in Miettinen and
#' Nurminen (1985).
#'
#' @param r_x A numeric vector of successes in the first group.
#' @param n_x A numeric vector of sample sizes in the first group.
#' @param r_y A numeric vector of successes in the second group.
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
#' \deqn{T_{MN} = \frac{\hat{p}_x - \hat{p}_y - \delta}{\sqrt{V_{\delta}}}}
#'
#' where:
#' \itemize{
#'   \item \eqn{\hat{p}_x} and \eqn{\hat{p}_y} are the observed proportions in the two groups
#'   \item \eqn{\delta} is the hypothesized difference
#'   \item \eqn{V_{\delta}} is the variance estimate under the null hypothesis
#'         \eqn{V_{\delta} = \lambda(\tilde{p}_x(1-\tilde{p}_x)/n_x + \tilde{p}_y(1-\tilde{p}_y)/n_y)}
#'   \item \eqn{\lambda = N/(N+1)} is a small sample correction factor
#'   \item \eqn{\tilde{p}_x} and \eqn{\tilde{p}_y} are the restricted maximum likelihood estimates
#'         under the constraint that \eqn{\tilde{p}_x - \tilde{p}_y = \delta}
#'   \item \eqn{\tilde{p}_x = 2ucos(w) - b/3a} and  \eqn{\tilde{p}_y = \tilde{p}_x - \delta}
#'   \item \eqn{w = \frac{\pi + cos^{-1}(v/u^3)}{3}}
#'   \item \eqn{v = \frac{b^3}{27a^3}-\frac{bc}{6a^2}+\frac{d}{2a}}
#'   \item \eqn{u = \frac{v}{|v|}\sqrt{\frac{b^2}{9a^2}+\frac{c}{3a}}}
#'   \item \eqn{a = 1 + \theta}
#'   \item \eqn{b = -(1 + \theta + \tilde{p}_x + \theta\tilde{p}_y + \delta(\theta + 2))}
#'   \item \eqn{c = \delta^2 + \delta(2\tilde{p}_x + \theta + 1) + \tilde{p}_x + \theta\tilde{p}_y}
#'   \item \eqn{d = -\tilde{p}_x\delta(1+\delta)}
#'   \item \eqn{\theta = n_y / n_x}
#'
#' }
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
test_score_mn <- function(r_x, n_x, r_y, n_y, delta){
  N <- n_x + n_y

  p_hat_x <- r_x / n_x
  p_hat_y <- r_y / n_y
  p_hat <- p_hat_x - p_hat_y

  theta = n_y / n_x
  lambda <- N/(N+1)

  d <- -p_hat_x*delta * (1+delta)
  c <- delta^2 + delta * (2* p_hat_x + theta + 1) + p_hat_x + theta * p_hat_y
  b <- -1*(1 + theta + p_hat_x + theta * p_hat_y + delta * (theta + 2))
  a <- 1 + theta
  v <- b^3/(27 * a^3) - b*c / (6* a^2) + d/(2*a)
  u <- v/abs(v) * sqrt(b^2 / (9 * a^2) - c / (3 * a))
  w <- (pi + acos(v/u^3))/3
  p_tilda_x <- 2 * u * cos(w) - b / (3*a)
  p_tilda_y <- p_tilda_x - delta
  var_delta <- lambda * (p_tilda_x * (1-p_tilda_x) / n_x + p_tilda_y * (1-p_tilda_y) / n_y)
  T_stat <- (p_hat_x - p_hat_y - delta) / sqrt(var_delta)
  T_stat
}


#' Calculate Stratified Miettinen-Nurminen Test Statistic
#'
#' The stratified Miettinen-Nurminen z test statistic
#' for a given delta (difference in proportions) using the method described in
#' Miettinen and Nurminen (1985), adapted for stratified data.
#'
#' @param r_x A numeric vector of successes in the first group, one per stratum.
#' @param n_x A numeric vector of sample sizes in the first group, one per stratum.
#' @param r_y A numeric vector of successes in the second group, one per stratum.
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
test_score_mn_strata<-function(r_x, n_x, r_y, n_y, delta){
  N <- n_x + n_y
  tot_r <- r_x + r_y
  w <- (n_x * n_y) / N
  tot_w <- sum(w)

  # The implementation follows equations described in Miettinen and Nurminen's
  # paper, specifically equations 8, 15, 27, and 28 for handling stratified
  # data. Variable names have been changed to be more consistent with the rest
  # of the package

  #equation 27
  L3 <- N
  L2 <- (n_x + 2*n_y)*delta - N - tot_r
  L1 <- (n_y*delta - N - 2*r_y)*delta + tot_r
  L0 <- r_y*delta*(1-delta)

  #equation 28
  q <- (L2^3)/((3*L3)^3) - (L1*L2)/(6*(L3^2)) + L0/(2*L3)

  p <- abs(sqrt((L2^2)/((3*L3)^2) - L1/(3*L3)))

  temp <- pmax(pmin(q/(p^3),1),-1)
  a <- (1/3)*(pi+acos(temp))

  mR0 <- 2*p*cos(a) - L2/(3*L3)
  mR1 <- mR0+delta

  #equation 8
  mV <- (mR1*(1-mR1)/n_x + mR0*(1-mR0)/n_y) * (N/(N-1))

  #equation 15
  den <- ((w/tot_w)^2)*mV

  tot_den <- sum(den)

  zstat <- (diff-delta)/sqrt(tot_den)
  zstat
}
