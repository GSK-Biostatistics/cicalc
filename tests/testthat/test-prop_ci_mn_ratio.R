# tests for ci_rel_risk_mn -----

test_that("ci_rel_risk_mn matches known reference values for CI", {
  # Reference values from gsDesign package ciBinomial function
  # ciBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "RR", adj = 1) gives (0.1342639, 0.7890346)
  # ciBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "RR", adj = 0) gives (0.134995, 0.7851359)
  # Also matches SAS PROC FREQ RELRISK(CL=SCORE) (Miettinen-Nurminen corrected).

  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  resp  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))

  # score method
  score <- ci_rel_risk_mn(x = resp, by = by, conf.level = 0.95)
  expect_equal(score$conf.low, 0.1342639, tolerance = 1e-4)
  expect_equal(score$conf.high, 0.7890346, tolerance = 1e-4)

  # asymptotic method
  asymptotic <- ci_rel_risk_mn(x = resp, by = by, method = "asymptotic", conf.level = 0.95)
  expect_equal(asymptotic$conf.low, 0.134995, tolerance = 1e-4)
  expect_equal(asymptotic$conf.high, 0.7851359, tolerance = 1e-4)


  # testing with data input
  df <- data.frame(event = resp, group = by)

  res_df <- ci_rel_risk_mn(x = event, by = group, data = df)
  res_vec <- ci_rel_risk_mn(x = df$event, by = df$group)

  expect_equal(res_df$estimate, res_vec$estimate)
  expect_equal(res_df$conf.low, res_vec$conf.low)
  expect_equal(res_df$conf.high, res_vec$conf.high)

})

test_that("delta argument works for ci_rel_risk_mn", {
  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  x  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))

  single_del <- ci_rel_risk_mn(x = x, by = by, delta = 0.3, conf.level = 0.95)
  multi_del <- ci_rel_risk_mn(x = x, by = by, delta = c(0.3, 0.5), conf.level = 0.95)

  expect_equal(length(single_del$p.value), 1)
  expect_equal(length(multi_del$p.value), 2)
  expect_equal(single_del$p.value, multi_del$p.value[1])

  expect_true("statistic" %in% names(single_del))
  expect_true("p.value" %in% names(single_del))
  expect_true(is.numeric(single_del$statistic))
  expect_length(single_del$statistic, 1)
  expect_true(is.finite(single_del$statistic))
  expect_true(is.numeric(single_del$p.value))
  expect_length(single_del$p.value, 1)
  expect_gte(single_del$p.value, 0)
  expect_lte(single_del$p.value, 1)

})

test_that("ci_rel_risk_mn matches known reference values for statistic/p.value", {
  # Reference values from gsDesign package testBinomial function
  # testBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "RR", adj = 1, delta0 = log(0.8)) : -1.99423
  # testBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "RR", adj = 0, delta0 = log(0.8)) : -2.006812

  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  resp  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))
  df <- data.frame(event = resp, group = by)
  delta <- 0.8

  # score method
  score <- ci_rel_risk_mn(x = resp, by = by, method = "score", data = df, delta = delta)
  expect_equal(score$statistic, -1.99423, tolerance = 1e-4)
  expect_equal(score$p.value, 2 * pnorm(-abs(-1.99423)), tolerance = 1e-4)

  # asymptotic method
  asymptotic <- ci_rel_risk_mn(x = resp, by = by, method = "asymptotic", data = df, delta = delta)
  expect_equal(asymptotic$statistic, -2.006812, tolerance = 1e-4)
  expect_equal(asymptotic$p.value, 2 * pnorm(-abs(-2.006812)), tolerance = 1e-4)

})

test_that("ci_rel_risk_mn validates inputs correctly", {
  # Non-binary x
  expect_error(ci_rel_risk_mn(x = c(1, 2, 3), by = c("A", "B", "A")))

  # By with more than two levels
  expect_error(ci_rel_risk_mn(x = c(1, 0, 1), by = c("A", "B", "C")))

  # Mismatched lengths
  expect_error(ci_rel_risk_mn(x = c(1, 0, 1, 0), by = c("A", "B", "A")))

  # Missing arguments or columns in df
  expect_error(ci_rel_risk_mn(x = c(1, 0, 1, 0)))
  df <- data.frame(event = c(1, 0, 1, 0),group = c(1, 1, 0, 0))
  expect_error(ci_rel_risk_mn(x = event1, by = group, data = df))
  expect_error(ci_rel_risk_mn(x = event, by = group1, data = df))

  # Invalid confidence level
  expect_error(ci_rel_risk_mn(x = c(1, 0), by = c("A", "B"), conf.level = 2))
  expect_error(ci_rel_risk_mn(x = c(1, 0), by = c("A", "B"), conf.level = 0))

  # Invalid delta
  expect_error(ci_rel_risk_mn(x = c(1, 0), by = c("A", "B"), delta = -1))
})

test_that("ci_rel_risk_mn is invariant to factor level ordering", {
  df_num <- data.frame(
    event = c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35)),
    group = c(rep(1, 40), rep(0, 40))
  )

  df_fac <- data.frame(
    event = df_num$event,
    group = factor(df_num$group, levels = c(0, 1), labels = c("control", "treated"))
  )

  res_num <- ci_rel_risk_mn(x = df_num$event, by = df_num$group)
  res_fac <- ci_rel_risk_mn(x = event, by = group, data = df_fac)

  expect_equal(res_fac$estimate, res_num$estimate, tolerance = 1e-8)
  expect_equal(res_fac$conf.low, res_num$conf.low, tolerance = 1e-8)
  expect_equal(res_fac$conf.high, res_num$conf.high, tolerance = 1e-8)
})


test_that("ci_rel_risk_mn matches ciBinomial at zero-event boundaries", {
  # ciBinomial(x1 = 5, n1 = 40, x2 = 0, n2 = 40, scale = "RR", adj = 1): (1.357923, Inf)
  # ciBinomial(x1 = 0, n1 = 40, x2 = 15, n2 = 40, scale = "RR", adj = 1): (0, 0.2395367)
  by <- c(rep(1, 40), rep(0, 40))

  res1 <- ci_rel_risk_mn(x = c(rep(0, 40), rep(1, 5), rep(0, 35)), by = by)
  expect_equal(res1$conf.low, 1.357923, tolerance = 1e-4)
  expect_equal(res1$conf.high, Inf)
  expect_equal(res1$estimate, Inf)

  res2 <- ci_rel_risk_mn(x = c(rep(1, 15), rep(0, 25), rep(0, 40)), by = by)
  expect_equal(res2$conf.low, 0)
  expect_equal(res2$conf.high, 0.2395367, tolerance = 1e-4)
  expect_equal(res2$estimate, 0)
})

test_that("ci_rel_risk_mn is not estimable with no events in either group", {
  by <- c(rep(1, 40), rep(0, 40))
  expect_warning(
    res <- ci_rel_risk_mn(x = rep(0, 80), by = by),
    "not estimable"
  )
  expect_true(is.na(res$estimate))
  expect_equal(res$conf.low, 0)
  expect_equal(res$conf.high, Inf)
})

test_that("ci_rel_risk_mn matches ciBinomial when a group has all events", {
  # ciBinomial(x1 = 10, n1 = 10, x2 = 1, n2 = 10, scale = "RR", adj = 1) : (2.420628, 58.01582)
  # ciBinomial(x1 = 1, n1 = 10, x2 = 10, n2 = 10, scale = "RR", adj = 1) : (0.01723668, 0.413116)
  # ciBinomial(x1 = 10, n1 = 10, x2 = 10, n2 = 10, scale = "RR", adj = 1) : (0.7120664, 1.404363)
  by <- rep(0:1, each = 10)

  res1 <- ci_rel_risk_mn(x = c(rep(1, 10), 1, rep(0, 9)), by = by)
  expect_equal(res1$conf.low, 2.420628, tolerance = 1e-4)
  expect_equal(res1$conf.high, 58.01582, tolerance = 1e-4)

  res2 <- ci_rel_risk_mn(x = c(1, rep(0, 9), rep(1, 10)), by = by)
  expect_equal(res2$conf.low, 0.01723668, tolerance = 1e-4)
  expect_equal(res2$conf.high, 0.413116, tolerance = 1e-4)

  res3 <- ci_rel_risk_mn(x = rep(1, 20), by = by)
  expect_equal(res3$conf.low, 0.7120664, tolerance = 1e-4)
  expect_equal(res3$conf.high, 1.404363, tolerance = 1e-4)
})



# tests for ci_odds_ratio_mn -----

test_that("ci_odds_ratio_mn matches known reference values", {
  # Reference values from gsDesign package ciBinomial function
  # ciBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "OR", adj = 1) : (0.07900466, 0.7248658)
  # ciBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "OR", adj = 0) : (0.0795152, 0.7200354)
  # Also matches SAS PROC FREQ RELRISK(CL=SCORE) (Miettinen-Nurminen corrected).

  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  resp  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))

  # score method
  score <- ci_odds_ratio_mn(x = resp, by = by, conf.level = 0.95)
  expect_equal(score$conf.low, 0.07900466, tolerance = 1e-4)
  expect_equal(score$conf.high, 0.7248658, tolerance = 1e-4)

  # asymptotic method
  asymptotic <- ci_odds_ratio_mn(x = resp, by = by, method = "asymptotic", conf.level = 0.95)
  expect_equal(asymptotic$conf.low, 0.0795152, tolerance = 1e-4)
  expect_equal(asymptotic$conf.high, 0.7200354, tolerance = 1e-4)


  # testing with data input
  df <- data.frame(event = resp, group = by)

  res_df <- ci_odds_ratio_mn(x = event, by = group, data = df)
  res_vec <- ci_odds_ratio_mn(x = df$event, by = df$group)

  expect_equal(res_df$estimate, res_vec$estimate)
  expect_equal(res_df$conf.low, res_vec$conf.low)
  expect_equal(res_df$conf.high, res_vec$conf.high)

})

test_that("delta argument works for ci_odds_ratio_mn", {
  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  x  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))

  single_del <- ci_odds_ratio_mn(x = x, by = by, delta = 0.3, conf.level = 0.95)
  multi_del <- ci_odds_ratio_mn(x = x, by = by, delta = c(0.3, 0.5), conf.level = 0.95)

  expect_equal(length(single_del$p.value), 1)
  expect_equal(length(multi_del$p.value), 2)
  expect_equal(single_del$p.value, multi_del$p.value[1])

  expect_true("statistic" %in% names(single_del))
  expect_true("p.value" %in% names(single_del))
  expect_true(is.numeric(single_del$statistic))
  expect_length(single_del$statistic, 1)
  expect_true(is.finite(single_del$statistic))
  expect_true(is.numeric(single_del$p.value))
  expect_length(single_del$p.value, 1)
  expect_gte(single_del$p.value, 0)
  expect_lte(single_del$p.value, 1)

})

test_that("ci_odds_ratio_mn matches known reference values for statistic/p.value", {
  # Reference values from gsDesign package testBinomial function
  # testBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "OR", adj = 1, delta0 = log(0.8)) gives -2.143003
  # testBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, scale = "OR", adj = 0, delta0 = log(0.8)) gives -2.156523

  a <- 15; n1 <- 40; c_ <- 5; n2 <- 40
  resp  <- c(rep(1, a), rep(0, n1 - a), rep(1, c_), rep(0, n2 - c_))
  by <- c(rep(1, n1), rep(0, n2))
  df <- data.frame(event = resp, group = by)
  delta <- 0.8

  # score method
  score <- ci_odds_ratio_mn(x = resp, by = by, method = "score", data = df, delta = delta)
  expect_equal(score$statistic, -2.143003, tolerance = 1e-4)
  expect_equal(score$p.value, 2 * pnorm(-abs(-2.143003)), tolerance = 1e-4)

  # asymptotic method
  asymptotic <- ci_odds_ratio_mn(x = resp, by = by, method = "asymptotic", data = df, delta = delta)
  expect_equal(asymptotic$statistic, -2.156523, tolerance = 1e-4)
  expect_equal(asymptotic$p.value, 2 * pnorm(-abs(-2.156523)), tolerance = 1e-4)

})

test_that("ci_odds_ratio_mn validates inputs correctly", {
  # Non-binary x
  expect_error(ci_odds_ratio_mn(x = c(1, 2, 3), by = c("A", "B", "A")))

  # By with more than two levels
  expect_error(ci_odds_ratio_mn(x = c(1, 0, 1), by = c("A", "B", "C")))

  # Mismatched lengths
  expect_error(ci_odds_ratio_mn(x = c(1, 0, 1, 0), by = c("A", "B", "A")))

  # Missing arguments or columns in df
  expect_error(ci_odds_ratio_mn(x = c(1, 0, 1, 0)))
  df <- data.frame(event = c(1, 0, 1, 0),group = c(1, 1, 0, 0))
  expect_error(ci_odds_ratio_mn(x = event1, by = group, data = df))
  expect_error(ci_odds_ratio_mn(x = event, by = group1, data = df))

  # Invalid confidence level
  expect_error(ci_odds_ratio_mn(x = c(1, 0), by = c("A", "B"), conf.level = 2))
  expect_error(ci_odds_ratio_mn(x = c(1, 0), by = c("A", "B"), conf.level = 0))

  # Invalid delta
  expect_error(ci_odds_ratio_mn(x = c(1, 0), by = c("A", "B"), delta = -1))
})

test_that("ci_odds_ratio_mn is invariant to factor level ordering", {
  df_num <- data.frame(
    event = c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35)),
    group = c(rep(1, 40), rep(0, 40))
  )

  df_fac <- data.frame(
    event = df_num$event,
    group = factor(df_num$group, levels = c(0, 1), labels = c("control", "treated"))
  )

  res_num <- ci_odds_ratio_mn(x = df_num$event, by = df_num$group)
  res_fac <- ci_odds_ratio_mn(x = event, by = group, data = df_fac)

  expect_equal(res_fac$estimate, res_num$estimate, tolerance = 1e-8)
  expect_equal(res_fac$conf.low, res_num$conf.low, tolerance = 1e-8)
  expect_equal(res_fac$conf.high, res_num$conf.high, tolerance = 1e-8)
})


test_that("ci_odds_ratio_mn matches ciBinomial at zero-event boundaries", {
  # ciBinomial(x1 = 5, n1 = 40, x2 = 0, n2 = 40, scale = "OR", adj = 1): (1.39766, Inf)
  # ciBinomial(x1 = 0, n1 = 40, x2 = 15, n2 = 40, scale = "OR", adj = 1): (-Inf, 0.1694178)
  # Note: ciBinomial reports -Inf where the OR lower limit is 0.
  by <- c(rep(1, 40), rep(0, 40))

  res1 <- ci_odds_ratio_mn(x = c(rep(0, 40), rep(1, 5), rep(0, 35)), by = by)
  expect_equal(res1$conf.low, 1.39766, tolerance = 1e-4)
  expect_equal(res1$conf.high, Inf)
  expect_equal(res1$estimate, Inf)

  res2 <- ci_odds_ratio_mn(x = c(rep(1, 15), rep(0, 25), rep(0, 40)), by = by)
  expect_equal(res2$conf.low, 0)
  expect_equal(res2$conf.high, 0.1694178, tolerance = 1e-4)
  expect_equal(res2$estimate, 0)
})

test_that("ci_odds_ratio_mn is not estimable with no events in both groups", {
  by <- c(rep(1, 40), rep(0, 40))
  expect_warning(
    res <- ci_odds_ratio_mn(x = rep(0, 80), by = by),
    "not estimable"
  )
  expect_true(is.na(res$estimate))
  expect_equal(res$conf.low, 0)
  expect_equal(res$conf.high, Inf)
})

test_that("ci_odds_ratio_mn matches ciBinomial when a group has all events", {
  # ciBinomial(x1 = 10, n1 = 10, x2 = 1, n2 = 10, scale = "OR", adj = 1): (10.6586, Inf)
  # ciBinomial(x1 = 1, n1 = 10, x2 = 10, n2 = 10, scale = "OR", adj = 1): (-Inf, 0.09382089)
  # Note: ciBinomial reports -Inf where the OR lower limit is 0.

  by <- rep(0:1, each = 10)

  res1 <- ci_odds_ratio_mn(x = c(rep(1, 10), 1, rep(0, 9)), by = by)
  expect_equal(res1$conf.low, 10.658607, tolerance = 1e-4)
  expect_equal(res1$conf.high, Inf, tolerance = 1e-4)

  res2 <- ci_odds_ratio_mn(x = c(1, rep(0, 9), rep(1, 10)), by = by)
  expect_equal(res2$conf.low, 0, tolerance = 1e-4)
  expect_equal(res2$conf.high, 0.093820894, tolerance = 1e-4)
})

# printing and delta range -----

test_that("ci_rel_risk_mn accepts any positive delta", {
  x  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
  by <- c(rep(1, 40), rep(0, 40))
  expect_no_error(ci_rel_risk_mn(x = x, by = by, delta = 2))
  expect_error(ci_rel_risk_mn(x = x, by = by, delta = 0))
})

test_that("ci_rel_risk_mn stores delta and prints test results", {
  x  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
  by <- c(rep(1, 40), rep(0, 40))

  res <- ci_rel_risk_mn(x = x, by = by, delta = c(0.3, 2))
  expect_equal(res$delta, c(0.3, 2))
  expect_length(res$statistic, 2)

  out <- cli::cli_fmt(print(res))
  expect_true(any(grepl("Score test", out)))
  expect_true(any(grepl("statistic", out)))
  expect_true(any(grepl("p-value", out)))
  expect_equal(sum(grepl("H0: ratio", out)), 2)

  out_none <- cli::cli_fmt(print(ci_rel_risk_mn(x = x, by = by)))
  expect_false(any(grepl("p-value", out_none)))
})

test_that("ci_rel_risk_mn both-zero case with delta gives statistic 0 and p-value 1", {
  x  <- rep(0, 80)
  by <- c(rep(0, 40), rep(1, 40))
  expect_warning(res <- ci_rel_risk_mn(x = x, by = by, delta = c(0.5, 1)), "not estimable")
  expect_equal(res$delta, c(0.5, 1))
  expect_equal(res$statistic, c(0, 0))
  expect_equal(res$p.value, c(1, 1))
})

test_that("ci_odds_ratio_mn accepts any positive delta", {
  x  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
  by <- c(rep(1, 40), rep(0, 40))
  expect_no_error(ci_odds_ratio_mn(x = x, by = by, delta = 2))
  expect_error(ci_odds_ratio_mn(x = x, by = by, delta = 0))
})

test_that("ci_odds_ratio_mn stores delta and prints test results", {
  x  <- c(rep(1, 15), rep(0, 25), rep(1, 5), rep(0, 35))
  by <- c(rep(1, 40), rep(0, 40))

  res <- ci_odds_ratio_mn(x = x, by = by, delta = c(0.3, 2))
  expect_equal(res$delta, c(0.3, 2))
  expect_length(res$statistic, 2)

  out <- cli::cli_fmt(print(res))
  expect_true(any(grepl("Score test", out)))
  expect_true(any(grepl("statistic", out)))
  expect_true(any(grepl("p-value", out)))
  expect_equal(sum(grepl("H0: ratio", out)), 2)

  out_none <- cli::cli_fmt(print(ci_odds_ratio_mn(x = x, by = by)))
  expect_false(any(grepl("p-value", out_none)))
})

test_that("ci_odds_ratio_mn both-zero case with delta gives statistic 0 and p-value 1", {
  x  <- rep(0, 80)
  by <- c(rep(0, 40), rep(1, 40))
  expect_warning(res <- ci_odds_ratio_mn(x = x, by = by, delta = c(0.5, 1)), "not estimable")
  expect_equal(res$delta, c(0.5, 1))
  expect_equal(res$statistic, c(0, 0))
  expect_equal(res$p.value, c(1, 1))
})

# comparison with gsDesign::testBinomial -----
test_that("score statistics match gsDesign::testBinomial, including boundaries", {
  skip_if_not_installed("gsDesign")

  grid <- expand.grid(
    x1 = c(0, 1, 5, 10), x2 = c(0, 1, 8, 10), n = 10,
    scale = c("RR", "OR"), method = c("score", "asymptotic"),
    delta = c(0.5, 1, 2), stringsAsFactors = FALSE
  )

  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    fn <- if (g$scale == "RR") ci_rel_risk_mn else ci_odds_ratio_mn
    x <- c(rep(1, g$x1), rep(0, g$n - g$x1), rep(1, g$x2), rep(0, g$n - g$x2))
    by <- rep(0:1, each = g$n)

    res <- suppressWarnings(fn(x, by, delta = g$delta, method = g$method))
    ref <- gsDesign::testBinomial(
      g$x1, g$x2, g$n, g$n,
      delta0 = log(g$delta), adj = as.integer(g$method == "score"), scale = g$scale
    )
    expect_equal(res$statistic, ref, tolerance = 1e-4, label = paste(g, collapse = " "))
  }
})

test_that("confidence limits match gsDesign::ciBinomial, including boundaries", {
  skip_if_not_installed("gsDesign")

  grid <- expand.grid(
    x1 = c(0, 1, 5, 10), x2 = c(0, 1, 8, 10), n = 10,
    scale = c("RR", "OR"), method = c("score", "asymptotic"),
    stringsAsFactors = FALSE
  )
  # Neither group has events; ciBinomial returns (0, Inf) and we also warn
  grid <- grid[!(grid$x1 == 0 & grid$x2 == 0), ]

  for (i in seq_len(nrow(grid))) {
    g <- grid[i, ]
    fn <- if (g$scale == "RR") ci_rel_risk_mn else ci_odds_ratio_mn
    x <- c(rep(1, g$x1), rep(0, g$n - g$x1), rep(1, g$x2), rep(0, g$n - g$x2))
    by <- rep(0:1, each = g$n)

    res <- suppressWarnings(fn(x, by, method = g$method))
    ref <- gsDesign::ciBinomial(
      g$x1, g$x2, g$n, g$n, adj = as.integer(g$method == "score"), scale = g$scale
    )
    # ciBinomial reports a lower OR limit of 0 as -Inf
    ref_low <- if (is.infinite(ref$lower) && ref$lower < 0) 0 else ref$lower
    lab <- paste(g, collapse = " ")
    expect_equal(res$conf.low, ref_low, tolerance = 1e-4, label = lab)
    expect_equal(res$conf.high, ref$upper, tolerance = 1e-4, label = lab)
  }
})


# additional tests -----

test_that("ci_odds_ratio_mn is not estimable when both groups have all events", {
  # ciBinomial(x1 = 10, n1 = 10, x2 = 10, n2 = 10, scale = "OR", adj = 1): (-Inf, Inf)
  by <- rep(0:1, each = 10)
  expect_warning(res <- ci_odds_ratio_mn(x = rep(1, 20), by = by), "not estimable")
  expect_true(is.na(res$estimate))
  expect_equal(res$conf.low, 0)
  expect_equal(res$conf.high, Inf)

  expect_warning(
    res_d <- ci_odds_ratio_mn(x = rep(1, 20), by = by, delta = c(0.5, 2)),
    "not estimable"
  )
  expect_equal(res_d$statistic, c(0, 0))
  expect_equal(res_d$p.value, c(1, 1))
})

test_that("equal proportions give an estimate of exactly 1", {
  # ciBinomial(x1 = 10, n1 = 40, x2 = 10, n2 = 40, adj = 1):
  #   RR (0.47185454, 2.1192972); OR (0.36812403, 2.7164757)
  x  <- c(rep(1, 10), rep(0, 30), rep(1, 10), rep(0, 30))
  by <- rep(0:1, each = 40)

  rr <- ci_rel_risk_mn(x, by, delta = 1)
  expect_equal(rr$estimate, 1)
  expect_equal(rr$conf.low, 0.47185454, tolerance = 1e-4)
  expect_equal(rr$conf.high, 2.1192972, tolerance = 1e-4)
  expect_equal(rr$statistic, 0, tolerance = 1e-8)
  expect_equal(rr$p.value, 1, tolerance = 1e-8)

  or <- ci_odds_ratio_mn(x, by, delta = 1)
  expect_equal(or$estimate, 1)
  expect_equal(or$conf.low, 0.36812403, tolerance = 1e-4)
  expect_equal(or$conf.high, 2.7164757, tolerance = 1e-4)
  expect_equal(or$statistic, 0, tolerance = 1e-8)
  expect_equal(or$p.value, 1, tolerance = 1e-8)
})

test_that("conf.level other than 0.95 matches ciBinomial", {
  # ciBinomial(x1 = 5, n1 = 40, x2 = 15, n2 = 40, adj = 1, alpha = ...)
  #   RR 90%: (0.15458479, 0.6935376);  RR 99%: (0.1030754, 1.0038077)
  #   OR 90%: (0.093436011, 0.60999537); OR 99%: (0.057863653, 1.0052067)
  x  <- c(rep(1, 5), rep(0, 35), rep(1, 15), rep(0, 25))
  by <- rep(0:1, each = 40)

  rr90 <- ci_rel_risk_mn(x, by, conf.level = 0.90)
  expect_equal(c(rr90$conf.low, rr90$conf.high), c(0.15458479, 0.6935376), tolerance = 1e-4)
  rr99 <- ci_rel_risk_mn(x, by, conf.level = 0.99)
  expect_equal(c(rr99$conf.low, rr99$conf.high), c(0.1030754, 1.0038077), tolerance = 1e-4)

  or90 <- ci_odds_ratio_mn(x, by, conf.level = 0.90)
  expect_equal(c(or90$conf.low, or90$conf.high), c(0.093436011, 0.60999537), tolerance = 1e-4)
  or99 <- ci_odds_ratio_mn(x, by, conf.level = 0.99)
  expect_equal(c(or99$conf.low, or99$conf.high), c(0.057863653, 1.0052067), tolerance = 1e-4)

  # Wider interval at higher confidence
  expect_lt(rr99$conf.low, rr90$conf.low)
  expect_gt(rr99$conf.high, rr90$conf.high)
  expect_lt(or99$conf.low, or90$conf.low)
  expect_gt(or99$conf.high, or90$conf.high)
})

test_that("swapping groups inverts the estimate and interval", {
  x  <- c(rep(1, 5), rep(0, 35), rep(1, 15), rep(0, 25))
  by <- rep(0:1, each = 40)
  by_swapped <- 1 - by

  for (fn in list(ci_rel_risk_mn, ci_odds_ratio_mn)) {
    for (method in c("score", "asymptotic")) {
      a <- fn(x, by, method = method)
      b <- fn(x, by_swapped, method = method)
      expect_equal(a$estimate * b$estimate, 1, tolerance = 1e-8)
      expect_equal(a$conf.low * b$conf.high, 1, tolerance = 1e-5)
      expect_equal(a$conf.high * b$conf.low, 1, tolerance = 1e-5)
    }
  }

  # Boundary case: [lower, Inf) becomes [0, 1 / lower]
  x_b <- c(rep(1, 5), rep(0, 35), rep(0, 40))
  for (fn in list(ci_rel_risk_mn, ci_odds_ratio_mn)) {
    a <- fn(x_b, by)
    b <- fn(x_b, by_swapped)
    expect_equal(a$conf.high, Inf)
    expect_equal(b$conf.low, 0)
    expect_equal(b$conf.high, 1 / a$conf.low, tolerance = 1e-5)
  }
})
