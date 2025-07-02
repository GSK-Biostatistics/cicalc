#' Wald Confidence Interval for Difference in Proportions
#'
#' Calculates the Wald interval by following the usual textbook definition
#'   for a difference in proportions confidence interval using the normal approximation.
#'
#' @details
#'
#' \deqn{(\hat{p}_1 - \hat{p}_2) \pm z_{\alpha/2}
#' \sqrt{\frac{\hat{p}_1(1 - \hat{p}_1)}{n_1}+\frac{\hat{p}_2(1 - \hat{p}_2)}{n_2}}}
#'
#' @inheritParams ci_prop_diff_mn
#'
#' @return A list containing the following components:
#'
#'   \item{estimate}{The point estimate of the difference in proportions (p_x - p_y)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'
#' @export
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_wald(x = responses, by = arm)
ci_prop_diff_wald <- function(x, by, conf.level = 0.95, correct = FALSE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_wald(
        x = x , by = by,
        conf.level = conf.level,
        correct = correct
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
  check_scalar(conf.level)
  check_class(x = correct, "logical")
  check_scalar(correct)


  # convert vectors to count data
  df <- get_counts(x = x, by = by)

  p_1_hat <- df$response_1/df$n_1
  p_2_hat <- df$response_2/df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  correction_factor <- ifelse(correct, 0.5*(1/df$n_1 + 1/df$n_2), 0)

  err <- z * sqrt(p_1_hat*(1- p_1_hat)/df$n_1 + p_2_hat*(1 - p_2_hat)/ df$n_2) + correction_factor
  l_ci <- max(-1, p_hat_diff - err)
  u_ci <- min(1, p_hat_diff + err)

  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = p_hat_diff,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Wald Confidence Interval {ifelse(correct, 'with', 'without')} Continuity Correction")
    ),
    class = c("wald", "prop_ci_bi", "cicada")
  )
}

#' Haldane Confidence Interval for Difference in Proportions
#'
#'
#' @inheritParams ci_prop_diff_mn
#'
#' @details
#' The confidence interval is calculated by \eqn{\theta^* \pm w} where:
#'
#' \deqn{\theta^* = \frac{(\hat{p}_1 - \hat{p}_2) + z^2v(1-2\hat{\psi})}{1+z^2u}}
#' where
#' \deqn{w = \frac{z}{1+z^2u}\sqrt{u\{4\hat{\psi}(1-\hat{\psi})-(\hat{p}_1 - \hat{p}_2)^2\}+2v(1-2\hat{\psi})(\hat{p}_1-\hat{p}_2)
#' +4z^2v^2(1-2\hat{\psi})^2
#' }}
#' \deqn{\hat{\psi} = \frac{\hat{p}_1 + \hat{p}_2}{2}}
#' \deqn{u = \frac{1/n_1 + 1/n_2}{4}}
#' \deqn{v = \frac{1/n_1 - 1/n_2}{4}}
#'
#' @return A list containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (theta*)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'
#' @export
#'
#' @references
#'
#' \href{https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf}{Constructing Confidence Intervals for the Differences of Binomial Proportions in SAS}
#'
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_haldane(x = responses, by = arm)
ci_prop_diff_haldane <- function(x, by, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_haldane(
        x = x , by = by,
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
  check_scalar(conf.level)


  # convert vectors to count data
  df <- get_counts(x = x, by = by)

  p_1_hat <- df$response_1/df$n_1
  p_2_hat <- df$response_2/df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  psi_hat = (p_1_hat + p_2_hat)/2

  u = (1/df$n_1 + 1/df$n_2)/4
  v = (1/df$n_1 - 1/df$n_2)/4
  theta_star = (p_hat_diff + z^2*v*(1-2*psi_hat)) / (1+z^2*u)

  a = u*(4 * psi_hat*(1-psi_hat) - p_hat_diff^2)
  b = 2 * v * (1 - 2 *psi_hat) * p_hat_diff
  c = 4 * z^2 * u^2*(1-psi_hat) * psi_hat
  d = z^2*v^2*(1-2*psi_hat)^2
  w = z/(1+z^2*u)*sqrt((a+b+c+d))

  l_ci <- max(-1, theta_star - w)
  u_ci <- min(1, theta_star + w)

  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = theta_star,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Haldane Confidence Interval")
    ),
    class = c("haldane", "prop_ci_bi", "cicada")
  )

}

#' Jeffreys-Perks Confidence Interval for Difference in Proportions
#'
#'
#' @inheritParams ci_prop_diff_mn
#'
#' @details
#' The confidence interval is calculated by \eqn{\theta^* \pm w} where:
#'
#' \deqn{\theta^* = \frac{(\hat{p}_1 - \hat{p}_2) + z^2v(1-2\hat{\psi})}{1+z^2u}}
#' where
#' \deqn{w = \frac{z}{1+z^2u}\sqrt{u\{4\hat{\psi}(1-\hat{\psi})-(\hat{p}_1 - \hat{p}_2)^2\}+2v(1-2\hat{\psi})(\hat{p}_1-\hat{p}_2)
#' +4z^2v^2(1-2\hat{\psi})^2
#' }}
#' \deqn{\hat{\psi} = \frac{1}{2}\left(\frac{x_1 + 1/2}{n_1+1}+\frac{x_2 + 1/2}{n_2+1}\right)}
#' \deqn{u = \frac{1/n_1 + 1/n_2}{4}}
#' \deqn{v = \frac{1/n_1 - 1/n_2}{4}}
#'
#' @return A list containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (theta*)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'
#' @export
#'
#' @references
#'
#' \href{https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf}{Constructing Confidence Intervals for the Differences of Binomial Proportions in SAS}
#'
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_jp(x = responses, by = arm)
ci_prop_diff_jp <- function(x, by, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_jp(
        x = x , by = by,
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
  check_scalar(conf.level)


  # convert vectors to count data
  df <- get_counts(x = x, by = by)

  p_1_hat <- df$response_1/df$n_1
  p_2_hat <- df$response_2/df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  psi_hat = 0.5*((df$response_1 + 0.5)/(df$n_1 + 1)+(df$response_2 + 0.5)/(df$n_2 + 1))

  u = (1/df$n_1 + 1/df$n_2)/4
  v = (1/df$n_1 - 1/df$n_2)/4
  theta_star = (p_hat_diff + z^2*v*(1-2*psi_hat)) / (1+z^2*u)

  a = u*(4 * psi_hat*(1-psi_hat) - p_hat_diff^2)
  b = 2 * v * (1 - 2 *psi_hat) * p_hat_diff
  c = 4 * z^2 * u^2*(1-psi_hat) * psi_hat
  d = z^2*v^2*(1-2*psi_hat)^2
  w = z/(1+z^2*u)*sqrt((a+b+c+d))

  l_ci <- max(-1, theta_star - w)
  u_ci <- min(1, theta_star + w)

  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = theta_star,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Jeffreys-Perks Confidence Interval")
    ),
    class = c("jeffreys_perks", "prop_ci_bi", "cicada")
  )

}


#' Mee Confidence Interval for Difference in Proportions
#'
#'
#'
#' @inheritParams ci_prop_diff_mn
#'
#' @details
#' The confidence interval is calculated by \eqn{\theta^* \pm w} where:
#'
#' \deqn{\theta^* = \frac{(\hat{p}_1 - \hat{p}_2) + z^2v(1-2\hat{\psi})}{1+z^2u}}
#' where
#' \deqn{w = \frac{z}{1+z^2u}\sqrt{u\{4\hat{\psi}(1-\hat{\psi})-(\hat{p}_1 - \hat{p}_2)^2\}+2v(1-2\hat{\psi})(\hat{p}_1-\hat{p}_2)
#' +4z^2v^2(1-2\hat{\psi})^2
#' }}
#' \deqn{\hat{\psi} = \frac{1}{2}\left(\frac{x_1 + 1/2}{n_1+1}+\frac{x_2 + 1/2}{n_2+1}\right)}
#' \deqn{u = \frac{1/n_1 + 1/n_2}{4}}
#' \deqn{v = \frac{1/n_1 - 1/n_2}{4}}
#'
#' @return A list containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (theta*)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'
#' @export
#'
#' @references
#'
#' \href{https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf}{Constructing Confidence Intervals for the Differences of Binomial Proportions in SAS}
#'
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_mee(x = responses, by = arm)
ci_prop_diff_mee <- function(x, by, conf.level = 0.95, delta = NULL, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_mee(
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
                                     fx=test_score_mee,
                                     ref_z = stats::qnorm(1 - alpha / 2),
                                     s_x = df$response_1, n_x = df$n_1,
                                     s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root,
                      -1 )

  upper_ci <- ifelse( df$response_2 > 0,
                      stats::uniroot(z_distance, interval=c(lower_ci,0.999999),
                                     fx=test_score_mee,
                                     ref_z = stats::qnorm(alpha / 2),
                                     s_x = df$response_1, n_x = df$n_1,
                                     s_y = df$response_2, n_y = df$n_2, tol=1e-08)$root,
                      1)

  statistic = NULL
  p.value = NULL

  if(!is.null(delta)){
    check_not_missing(delta)
    statistic <- test_score_mee(s_x = df$response_1, n_x = df$n_1,
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
        glue::glue("Mee Confidence Interval")
    ),
    class = c("mee", "prop_ci_bi", "cicada")
  )

}

#' Helper Function for the Estimation of Weights for `ci_prop_diff_mee()`
#'
#' This function is a modified mn test statistic
#'
#'
#' @keywords internal
#' @noRd
test_score_mee <- function(s_x, n_x, s_y, n_y, delta){
  p_hat_x <- s_x / n_x
  p_hat_y <- s_y / n_y

  var_delta <- variance_mn(s_x, n_x, s_y, n_y, delta)*((n_x+n_y-1)/(n_x+n_y))

  T_stat <- (p_hat_x - p_hat_y - delta) / sqrt(var_delta)
  T_stat
}
