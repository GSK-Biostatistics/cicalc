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
#' @param correct (`logical`)\cr apply continuity correction.
#'
#' @return An object containing the following components:
#'   \item{n}{Number of responses in each by group}
#'   \item{N}{Total number in each by group}
#'   \item{estimate}{The point estimate of the difference in proportions (p_1 - p_2)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Type of method used}
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
        x = x, by = by,
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

  p_1_hat <- df$response_1 / df$n_1
  p_2_hat <- df$response_2 / df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  correction_factor <- ifelse(correct, 0.5 * (1 / df$n_1 + 1 / df$n_2), 0)

  err <- z * sqrt(p_1_hat * (1 - p_1_hat) / df$n_1 + p_2_hat * (1 - p_2_hat) / df$n_2) + correction_factor
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
#' @return An object containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (theta*)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Haldane Confidence Interval}
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
        x = x, by = by,
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

  p_1_hat <- df$response_1 / df$n_1
  p_2_hat <- df$response_2 / df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  psi_hat <- (p_1_hat + p_2_hat) / 2

  u <- (1 / df$n_1 + 1 / df$n_2) / 4
  v <- (1 / df$n_1 - 1 / df$n_2) / 4
  theta_star <- (p_hat_diff + z^2 * v * (1 - 2 * psi_hat)) / (1 + z^2 * u)

  a <- u * (4 * psi_hat * (1 - psi_hat) - p_hat_diff^2)
  b <- 2 * v * (1 - 2 * psi_hat) * p_hat_diff
  c <- 4 * z^2 * u^2 * (1 - psi_hat) * psi_hat
  d <- z^2 * v^2 * (1 - 2 * psi_hat)^2
  w <- z / (1 + z^2 * u) * sqrt((a + b + c + d))

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
#' @return An object containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (theta*)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Jeffreys-Perks Confidence Interval}
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
        x = x, by = by,
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

  p_1_hat <- df$response_1 / df$n_1
  p_2_hat <- df$response_2 / df$n_2
  p_hat_diff <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)
  psi_hat <- 0.5 * ((df$response_1 + 0.5) / (df$n_1 + 1) + (df$response_2 + 0.5) / (df$n_2 + 1))

  u <- (1 / df$n_1 + 1 / df$n_2) / 4
  v <- (1 / df$n_1 - 1 / df$n_2) / 4
  theta_star <- (p_hat_diff + z^2 * v * (1 - 2 * psi_hat)) / (1 + z^2 * u)

  a <- u * (4 * psi_hat * (1 - psi_hat) - p_hat_diff^2)
  b <- 2 * v * (1 - 2 * psi_hat) * p_hat_diff
  c <- 4 * z^2 * u^2 * (1 - psi_hat) * psi_hat
  d <- z^2 * v^2 * (1 - 2 * psi_hat)^2
  w <- z / (1 + z^2 * u) * sqrt((a + b + c + d))

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
#' @return An object containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions (p1-p2)}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Mee Confidence Interval}
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
        x = x,
        by = by,
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
  check_range(delta,
    allow_empty = TRUE,
    range = c(-1, 1), include_bounds = c(FALSE, FALSE)
  )

  # convert vectors to count data
  df <- get_counts(x = x, by = by)

  alpha <- 1 - conf.level

  lower_ci <- ifelse(df$response_1 > 0,
    stats::uniroot(z_distance,
      interval = c(-0.999, 0.999),
      fx = test_score_mee,
      ref_z = stats::qnorm(1 - alpha / 2),
      s_x = df$response_1, n_x = df$n_1,
      s_y = df$response_2, n_y = df$n_2, tol = 1e-08
    )$root,
    -1
  )

  upper_ci <- ifelse(df$response_2 > 0,
    stats::uniroot(z_distance,
      interval = c(lower_ci, 0.999999),
      fx = test_score_mee,
      ref_z = stats::qnorm(alpha / 2),
      s_x = df$response_1, n_x = df$n_1,
      s_y = df$response_2, n_y = df$n_2, tol = 1e-08
    )$root,
    1
  )

  statistic <- NULL
  p.value <- NULL

  if (!is.null(delta)) {
    check_not_missing(delta)
    statistic <- test_score_mee(
      s_x = df$response_1, n_x = df$n_1,
      s_y = df$response_2, n_y = df$n_2, delta = delta
    )
    p.value <- (1 - stats::pnorm(abs(statistic)))
  }

  # Output
  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = df$response_1 / df$n_1 - df$response_2 / df$n_2,
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
test_score_mee <- function(s_x, n_x, s_y, n_y, delta) {
  p_hat_x <- s_x / n_x
  p_hat_y <- s_y / n_y

  var_delta <- variance_mn(s_x, n_x, s_y, n_y, delta) * ((n_x + n_y - 1) / (n_x + n_y))

  T_stat <- (p_hat_x - p_hat_y - delta) / sqrt(var_delta)
  T_stat
}

#' Anderson-Hauck Confidence Interval for Difference in Proportions
#'
#'
#' @inheritParams ci_prop_diff_mn
#'
#' @details
#' The confidence interval is given by:
#'
#' \deqn{(\hat{p}_1 - \hat{p}_2) \pm \left[ \frac{1}{2 \min(n_1, n_2)} + z \sqrt{ \frac{\hat{p}_1 (1 - \hat{p}_1)}{n_1 - 1} + \frac{\hat{p}_2 (1 - \hat{p}_2)}{n_2 - 1} } \right]}.
#'
#' @return An object containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Anderson-Hauck Confidence Interval}
#'
#' @export
#'
#' @references
#' Hauck WW, Anderson S. (1986) A comparison of large-sample confidence interval methods for the difference of two binomial probabilities The American Statistician 40(4). p.318-322.
#' \href{https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf}{Constructing Confidence Intervals for the Differences of Binomial Proportions in SAS}
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_ha(x = responses, by = arm)
ci_prop_diff_ha <- function(x, by, conf.level = 0.95, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_ha(
        x = x,
        by = by,
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

  p_1_hat <- df$response_1 / df$n_1
  p_2_hat <- df$response_2 / df$n_2
  est <- p_1_hat - p_2_hat

  z <- stats::qnorm((1 + conf.level) / 2)

  term2 <- 1 /
    (2 * min(df$n_1, df$n_2)) + z * sqrt(p_1_hat * (1 - p_1_hat) / (df$n_1 - 1) + p_2_hat * (1 - p_2_hat) / (df$n_2 - 1))

  l_ci <- max(-1, est - term2)
  u_ci <- min(1, est + term2)

  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = est,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Anderson-Hauck Confidence Interval")
    ),
    class = c("Anderson-Hauck", "prop_ci_bi", "cicada")
  )
}

#' Newcombe Confidence Interval for Difference in Proportions
#'
#'
#' @inheritParams ci_prop_diff_wald
#'
#'
#' @details
#' The **Wilson (Score)** confidence limits without continuity correction for each
#' individual binomial proportion, \eqn{p_i = x_i / n_i}, for \eqn{i = 1, 2}, are given by:
#'
#' \deqn{
#' \frac{
#'   (2 n_i \hat{p}_i + z^2)
#'   \pm
#'   z \sqrt{ 4 n_i \hat{p}_i (1 - \hat{p}_i) + z^2 }
#' }{
#'   2 (n_i + z^2)
#' }
#' }
#'
#' Denote the lower and upper Wilson (Score) confidence limits for \eqn{p_i} as
#' \eqn{L_i} and \eqn{U_i}, respectively.
#'
#' Then, the **Newcombe (Score)** confidence limits for the difference in proportions
#' (\eqn{p_1 - p_2}) are given by:
#'
#' \deqn{
#' \text{Lower limit: } (\hat{p}_1 - \hat{p}_2) -
#' \sqrt{ (\hat{p}_1 - L_1)^2 + (U_2 - \hat{p}_2)^2 }
#' }
#'
#' \deqn{
#' \text{Upper limit: } (\hat{p}_1 - \hat{p}_2) +
#' \sqrt{ (U_1 - \hat{p}_1)^2 + (\hat{p}_2 - L_2)^2 }
#' }
#'
#' The confidence intervals with continuity correction for each individual binomial proportion are
#' obtained using the **Wilson (Score) confidence limits with continuity correction**.
#'
#' For each binomial proportion \eqn{p_i = x_i / n_i}, where \eqn{i = 1, 2},
#' the confidence intervals are given by:
#'
#' \deqn{
#' \frac{ 2 n_i \hat{p}_i + z^2 }{ 2 (n_i + z^2) }
#' \; \pm \;
#' \frac{ z }{ 2 (n_i + z^2) }
#' \sqrt{
#'   z^2 - \frac{2}{n_i} + 4 \hat{p}_i
#'   \left[ n_i (1 - \hat{p}_i) + 1 \right]
#' }
#' }
#'
#' @return An object containing the following components:
#'
#'   \item{n}{The number of responses for each group}
#'   \item{N}{The total number in each group}
#'   \item{estimate}{The point estimate of the difference in proportions}
#'   \item{conf.low}{Lower bound of the confidence interval}
#'   \item{conf.high}{Upper bound of the confidence interval}
#'   \item{conf.level}{The confidence level used}
#'   \item{method}{Anderson-Hauck Confidence Interval}
#'
#' @export
#'
#' @references
#' Newcombe, R. G. (1998). Interval estimation for the difference between
#' independent proportions: Comparison of eleven methods. *Statistics in Medicine, 17*(8), 873–890.
#' \href{https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf}{Constructing Confidence Intervals for the Differences of Binomial Proportions in SAS}
#'
#' @examples
#' responses <- expand(c(9, 3), c(10, 10))
#' arm <- rep(c("treat", "control"), times = c(10, 10))
#'
#' # Calculate 95% confidence interval for difference in proportions
#' ci_prop_diff_nc(x = responses, by = arm)
ci_prop_diff_nc <- function(x, by, conf.level = 0.95, correct = FALSE, data = NULL) {
  set_cli_abort_call()
  check_data_frame(data, allow_empty = TRUE)

  # if data was passed, evaluate in the context of the data frame
  if (is.data.frame(data)) {
    return(
      ci_prop_diff_nc(
        x = x, by = by,
        conf.level = ,
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
  check_class(correct, "logical")
  check_scalar(correct)
  check_range(conf.level, range = c(0, 1), include_bounds = c(FALSE, FALSE))
  check_scalar(conf.level)


  # convert vectors to count data
  df <- get_counts(x = x, by = by)


  alpha <- 1 - conf.level
  kappa <- stats::qnorm(1 - alpha / 2)
  p1_hat <- df$response_1 / df$n_1
  p2_hat <- df$response_2 / df$n_2
  est <- p1_hat - p2_hat


  if (correct) {
    w1 <- nc_binom_ci_helper(x = df$response_1, n = df$n_1, conf.level = conf.level, correct = correct)
    w2 <- nc_binom_ci_helper(x = df$response_2, n = df$n_2, conf.level = conf.level, correct = correct)
    l1 <- w1$l_ci
    u1 <- w1$u_ci
    l2 <- w2$l_ci
    u2 <- w2$u_ci
    l_ci <- max(-1, est - sqrt((p1_hat - l1)^2 + (u2 - p2_hat)^2))
    u_ci <- min(1, est + sqrt((u1 - p1_hat)^2 + (p2_hat - l2)^2))
  } else {
    w1 <- nc_binom_ci_helper(x = df$response_1, n = df$n_1, conf.level = conf.level, correct = correct)
    w2 <- nc_binom_ci_helper(x = df$response_2, n = df$n_2, conf.level = conf.level, correct = correct)
    l1 <- w1$l_ci
    u1 <- w1$u_ci
    l2 <- w2$l_ci
    u2 <- w2$u_ci
    l_ci <- est - kappa * sqrt(l1 * (1 - l1) / df$n_1 + u2 * (1 - u2) / df$n_2)
    u_ci <- est + kappa * sqrt(u1 * (1 - u1) / df$n_1 + l2 * (1 - l2) / df$n_2)
  }


  structure(
    list(
      n = c(df$response_1, df$response_2),
      N = c(df$n_1, df$n_2),
      estimate = est,
      conf.low = l_ci,
      conf.high = u_ci,
      conf.level = conf.level,
      method =
        glue::glue("Newcombe Confidence Interval {ifelse(correct, 'with', 'without')} continuity correction")
    ),
    class = c("Newcombe", "prop_ci_bi", "cicada")
  )
}

#' Helper Function for the Estimation of Score Confidence Interval for `ci_prop_diff_nc()`
#'
#' @keywords internal
#' @noRd
nc_binom_ci_helper <- function(x, n, conf.level, correct = correct) {
  alpha <- 1 - conf.level
  kappa <- stats::qnorm(1 - alpha / 2)
  p_hat <- x / n
  q_hat <- 1 - p_hat
  est <- p_hat

  if (correct) {
    lci <- (
      2 * x + kappa^2 - 1 - kappa * sqrt(kappa^2 - 2 - 1 / n + 4 * p_hat * (n * q_hat + 1))
    ) / (2 * (n + kappa^2))
    uci <- (
      2 * x + kappa^2 + 1 + kappa * sqrt(kappa^2 + 2 - 1 / n + 4 * p_hat * (n * q_hat - 1))
    ) / (2 * (n + kappa^2))
    ci_lwr <- max(0, ifelse(p_hat == 0, 0, lci))
    ci_upr <- min(1, ifelse(p_hat == 1, 1, uci))
    list(l_ci = ci_lwr, u_ci = ci_upr)
  } else {
    term1 <- (x + kappa^2 / 2) / (n + kappa^2)
    term2 <- kappa * sqrt(n) / (n + kappa^2) * sqrt(p_hat * q_hat + kappa^2 / (4 * n))
    ci_lwr <- max(0, term1 - term2)
    ci_upr <- min(1, term1 + term2)
    list(l_ci = ci_lwr, u_ci = ci_upr)
  }
}
