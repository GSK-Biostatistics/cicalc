# Data from Agresti (2013) page 226
agresti <- dplyr::tribble(
  ~centre, ~treatment, ~success, ~failure,
  1, "Drug", 11, 25,
  1, "Control", 10, 27,
  2, "Drug", 16 , 4,
  2, "Control", 22, 10,
  3, "Drug", 14 , 5,
  3, "Control", 7, 12,
  4, "Drug", 2, 14,
  4, "Control" , 1, 16,
  5, "Drug", 6, 11,
  5, "Control" , 0, 12,
  6, "Drug", 1, 10,
  6, "Control" ,0 , 10,
  7, "Drug", 1, 4 ,
  7, "Control",1, 8,
  8, "Drug", 4, 2,
  8, "Control", 6, 1,
)
agresti_long <- agresti |>
  dplyr::mutate(total = success+failure,
                results = purrr::map2(success, total, \(.x, .y) expand(.x, .y)),
                centre = as.character(centre)) |>
  dplyr::select(-success, -failure, -total)|>
  tidyr::unnest(results)

test_that("Testing values match what is stated in Agresti on page 231",{
  results <- ci_prop_diff_mh_strata(x= results, by = treatment,
                                    strata =centre, sato_var = TRUE, data = agresti_long)
  expect_equal(round(results$estimate, 3), 0.130)
  expect_equal(round(sqrt(results$variance), 3), 0.050)

  # manual calculations base on the delta and SE from agresti
  agresti_d <- 0.130
  agresti_se <- 0.05
  z <- stats::qnorm((1 + 0.95) / 2)
  low_ci <- agresti_d - agresti_se*z
  upper_ci <- agresti_d + agresti_se*z
  expect_equal(round(results$conf.low, 3), low_ci, tolerance = 0.05)
  expect_equal(round(results$conf.high, 3), upper_ci, tolerance = 0.05)
})

test_that("Check print",{
  expect_snapshot(
    ci_prop_diff_mh_strata(x= results, by = treatment,
                           strata =centre, data = agresti_long)
  )
})

test_that("Testing Independant Binomial Variance Estimator",{
  results <- ci_prop_diff_mh_strata(x= results, by = treatment,
                                    strata = centre, sato_var = FALSE, data = agresti_long)
  results_sato <- ci_prop_diff_mh_strata(x= results, by = treatment,
                                         strata = centre, sato_var = TRUE, data = agresti_long)
  # estimates between the two methods should be the same
  expect_equal(results$estimate, results_sato$estimate)
  # variance between the two should not be the same
  expect_false(results$variance == results_sato$variance)

  # the results of the Independent Binomial Variance does not equal the Sato variance
  expect_false(results$conf.low == results_sato$conf.low)
  expect_false(results$conf.high == results_sato$conf.high)
})

test_that("Check print",{
  expect_snapshot(
    ci_prop_diff_mh_strata(x= results, by = treatment,
                           strata =centre, sato_var = FALSE, data = agresti_long)
  )
})

# Common Relative Risk ----------------------------------------------------

test_that("Test Common Rel Risk",{
  sas_example <- dplyr::tribble(
    ~gender, ~treatment, ~response, ~count,
    "female", "Active", "Better", 16 ,
    "female", "Active", "Same", 11,
    "female", "Placebo", "Better",  5 ,
    "female", "Placebo", "Same", 20,
    "male",   "Active", "Better", 12 ,
    "male",   "Active", "Same", 16,
    "male",   "Placebo", "Better",  7 ,
    "male",   "Placebo", "Same", 19
  ) |>
    dplyr::mutate(
      res_val = response == "Better",
      res_vec = purrr::map2(res_val, count, rep)
    )  |>
    dplyr::select(-count, -res_val, -response)|>
    tidyr::unnest(res_vec)

  result <- ci_rel_risk_cmh_strata(res_vec, by = treatment, strat =gender, data = sas_example)
  expect_equal(round(result$estimate, 4), 2.1636)
  expect_equal(round(result$conf.low, 4), 1.2336)
  expect_equal(round(result$conf.high, 4), 3.7948)
})

test_that("Check print",{
  expect_snapshot(
    ci_rel_risk_cmh_strata(x= results, by = treatment,
                           strata =centre, data = agresti_long)
  )
})

# Stratified Newcombe Common Risk Diffence ------------------------------------
test_that("check the ci_prop_diff_nc_strata() function works", {
  # check Stratified Newcombe CIs ----------------------------------------------------------
  set.seed(1)
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

  expect_snapshot(
    ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "wilson" , correct = FALSE)
  )
  expect_snapshot(
    ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "wilson", correct = TRUE)
  )

  expect_snapshot(
    ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "cmh", correct = FALSE)
  )
  expect_snapshot(
    ci_prop_diff_nc_strata(x = rsp, by = grp, strata = strata, weights_method = "cmh", correct = TRUE)
  )

  expect_snapshot(
    ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata, weights_method = "wilson")
  )

  expect_snapshot(
    ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata, weights_method = "cmh")
  )

  expect_snapshot(
    ci_prop_diff_nc_strata(x = as.numeric(rsp), by = grp, strata = strata)
  )


  # checking error messaging
  expect_snapshot(
    ci_prop_diff_nc_strata(x = rep_len(TRUE, length(rsp)), by = grp, strata = strata, weights_method = "wilson"),
    error = TRUE
  )
  expect_snapshot(
    ci_prop_diff_nc_strata(x = rep_len(FALSE, length(rsp)), by = grp, strata = strata, weights_method = "cmh"),
    error = TRUE
  )
})
