#' Miettinen-Nurminen Confidence Interval for Difference in Proportions
#'
#' Calculates the Miettinen-Nurminen (MN) confidence interval for the difference
#' between two proportions. This method can be more accurate than traditional methods,
#' especially with small sample sizes or proportions close to 0 or 1.
#'
#' @param x A binary vector representing the first sample.
#' @param y A binary vector representing the second sample.
#' @param conf.level A numeric value between 0 and 1 specifying the confidence level.
#'        Default is 0.95 (95\% confidence).
#'
#' @return A list containing the following components:
#'   \item{estimate}{The point estimate of the difference in proportions (p_x - p_y)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Description of the method used ("Miettinen-Nurminen Confidence Interval")}
#'
#' @details
#' The function implements the Miettinen-Nurminen method to compute confidence intervals
#' for the difference between two proportions. This approach:
#'
#' 1. Computes the test statistic across a range of possible difference values (delta)
#' 2. Identifies values of delta where the test statistic falls within the critical region
#'    determined by the confidence level
#' 3. Returns the minimum and maximum acceptable values as the confidence interval bounds
#'
#' The method uses a score test with a small-sample correction factor, making it more
#' accurate than normal approximation methods, especially for small samples or extreme proportions.
#' The equation for the test statistics is as follows:
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
#' @references
#' Miettinen, O. S., & Nurminen, M. (1985). Comparative analysis of two rates.
#' Statistics in Medicine, 4(2), 213-226.
#'
#' Chen, Y., & Zhou, X. (2016). Interval Estimation for the Difference Between
#' Independent Proportions. Western Users of SAS Software Conference Proceedings 2016.
#' https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
#'
#' @examples
#' # Generate two binary samples
#' x <- rbinom(100, 1, 0.6)
#' y <- rbinom(80, 1, 0.5)
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_mn(x, y)
#'
#' # Calculate 99% confidence interval
#' ci_prop_diff_mn(x, y, conf.level = 0.99)
#'
#' @export
ci_prop_diff_mn <- function(x, y, conf.level = 0.95){
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_not_missing(y)
  check_binary(y)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))

  z <- stats::qnorm((1 + conf.level) / 2)
  delta_vec <- seq(-0.99999, 0.99999, length.out = 1000000)
  T_scores <- test_score_mn(x, y, delta = delta_vec)
  potential_vals <- delta_vec[which(-z < T_scores & T_scores < z)]

  list(
    estimate = sum(x)/length(x) - sum(y) / length(y),
    conf.low = min(potential_vals),
    conf.high = max(potential_vals),
    conf.level = conf.level,
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
#' @param x A binary vector representing the first sample.
#' @param y A binary vector representing the second sample.
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
test_score_mn <- function(x, y, delta){
  set_cli_abort_call()

  # check inputs ---------------------------------------------------------------
  check_not_missing(x)
  check_binary(x)
  check_not_missing(y)
  check_binary(y)
  check_range(delta, range = c(-1, 1), include_bounds = c(TRUE, TRUE))

  n_x <- length(x)
  n_y <- length(y)
  N <- n_x + n_y

  p_hat_x <- sum(x) / n_x
  p_hat_y <- sum(y) / n_y
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

