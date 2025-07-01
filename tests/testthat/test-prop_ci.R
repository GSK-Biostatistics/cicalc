test_that("check the ci_prop_*() functions work", {
  # setting vectors to test
  x_dbl <- c(NA, mtcars$vs)
  x_lgl <- as.numeric(x_dbl)
  x_rsp <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  x_true <- rep_len(TRUE, 32)
  x_false <- rep_len(FALSE, 32)

  # check Wilson CIs ----------------------------------------------------------
  expect_snapshot(
    wilson_dbl <- ci_prop_wilson(x_dbl, conf.level = 0.9, correct = FALSE)
  )
  expect_equal(
    wilson_dbl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_dbl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = FALSE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(
    wilsoncc_dbl <- ci_prop_wilson(x_dbl, conf.level = 0.9, correct = TRUE)
  )
  expect_equal(
    wilsoncc_dbl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_dbl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = TRUE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(
    wilson_lgl <- ci_prop_wilson(x_lgl, conf.level = 0.9, correct = FALSE)
  )
  expect_equal(
    wilson_lgl[c("conf.low", "conf.high")],
    prop.test(x = sum(x_lgl, na.rm = TRUE), n = 32, conf.level = 0.9, correct = FALSE)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(ci_prop_wilson(x_rsp, conf.level = 0.9))
  expect_snapshot(ci_prop_wilson(x_true))
  expect_snapshot(ci_prop_wilson(x_false))

  # check Wald CIs ----------------------------------------------------------
  expect_snapshot(
    wald_dbl <- ci_prop_wald(x_dbl, conf.level = 0.9, correct = FALSE)
  )
  expect_snapshot(
    waldcc_dbl <- ci_prop_wald(x_dbl, conf.level = 0.9, correct = TRUE)
  )
  expect_snapshot(
    wald_lgl <- ci_prop_wald(x_lgl, conf.level = 0.9, correct = FALSE)
  )

  expect_snapshot(ci_prop_wald(x_rsp, conf.level = 0.95, correct = TRUE))
  expect_snapshot(ci_prop_wald(x_true))
  expect_snapshot(ci_prop_wald(x_false))

  # check Clopper-Pearson CIs ----------------------------------------------------------
  expect_snapshot(
    clopper_pearson_dbl <- ci_prop_clopper_pearson(x_dbl, conf.level = 0.9)
  )
  expect_equal(
    clopper_pearson_dbl[c("conf.low", "conf.high")],
    binom.test(x = sum(x_dbl, na.rm = TRUE), n = 32, conf.level = 0.9)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(
    clopper_pearson_lgl <- ci_prop_clopper_pearson(x_lgl, conf.level = 0.9)
  )
  expect_equal(
    clopper_pearson_lgl[c("conf.low", "conf.high")],
    binom.test(x = sum(x_lgl, na.rm = TRUE), n = 32, conf.level = 0.9)$conf.int |>
      as.list() |>
      setNames(c("conf.low", "conf.high"))
  )

  expect_snapshot(ci_prop_clopper_pearson(x_rsp, conf.level = 0.95))
  expect_snapshot(ci_prop_wilson(x_true))
  expect_snapshot(ci_prop_wilson(x_false))

  # check Agresti-Coull CIs ----------------------------------------------------------
  expect_snapshot(
    agresti_coull_dbl <- ci_prop_agresti_coull(x_dbl, conf.level = 0.9)
  )
  expect_snapshot(
    agresti_coull_lgl <- ci_prop_agresti_coull(x_lgl, conf.level = 0.9)
  )

  expect_snapshot(ci_prop_agresti_coull(x_rsp, conf.level = 0.95))
  expect_snapshot(ci_prop_agresti_coull(x_true))
  expect_snapshot(ci_prop_agresti_coull(x_false))

  # check Jeffreys CIs ----------------------------------------------------------
  expect_snapshot(
    jeffreys_dbl <- ci_prop_jeffreys(x_dbl, conf.level = 0.9)
  )
  expect_snapshot(
    jeffreys_lgl <- ci_prop_jeffreys(x_lgl, conf.level = 0.9)
  )

  expect_snapshot(ci_prop_jeffreys(x_rsp, conf.level = 0.95))
  expect_snapshot(ci_prop_jeffreys(x_true))
  expect_snapshot(ci_prop_jeffreys(x_false))

  # error messaging ------------------------------------------------------------
  expect_snapshot(
    ci_prop_wilson(x_dbl, conf.level = c(0.9, 0.9)),
    error = TRUE
  )

  expect_snapshot(
    error = TRUE,
    ci_prop_wilson(mtcars$cyl)
  )
})


test_that("check the ci_prop_strat_wilson() function works", {
  # check Stratified Wilson CIs ----------------------------------------------------------
  withr::local_seed(1)
  rsp <- c(
    sample(c(TRUE, FALSE), size = 40, prob = c(3 / 4, 1 / 4), replace = TRUE),
    sample(c(TRUE, FALSE), size = 40, prob = c(1 / 2, 1 / 2), replace = TRUE)
  )
  grp <- factor(rep(c("A", "B"), each = 40), levels = c("B", "A"))
  strata_data <- data.frame(
    "f1" = sample(c("a", "b"), 80, TRUE),
    "f2" = sample(c("x", "y", "z"), 80, TRUE),
    stringsAsFactors = TRUE
  )
  strata <- interaction(strata_data)
  weights <- 1:6 / sum(1:6)

  expect_snapshot(
    ci_prop_wilson_strata(x = rsp, strata = strata, weights = weights, correct = FALSE)
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = rsp, strata = strata, weights = weights, correct = TRUE)
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights)
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata)
  )


  # checking error messaging
  expect_snapshot(
    ci_prop_wilson_strata(x = rep_len(TRUE, length(rsp)), strata = strata, weights = weights),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = rep_len(FALSE, length(rsp)), strata = strata, weights = weights),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, max.iterations = -1),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, max.iterations = -1),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights + pi / 5),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_wilson_strata(x = as.numeric(rsp), strata = strata, weights = weights + pi),
    error = TRUE
  )
})
