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
  results <- ci_prop_common_risk_diff_sato(x= results, by = treatment,
                                           strata =centre, data = agresti_long)
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
    ci_prop_common_risk_diff_sato(x= results, by = treatment,
                                  strata =centre, data = agresti_long)
  )
})


