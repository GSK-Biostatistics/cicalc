test_that("ci_prop_diff_* matches the values in the paper", {
  # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
  # 56/70-48/80
  resp <- expand(c(56, 48), c(70, 80))
  trt <- rep(c("a", "b"), times = c(70,80))

  # Wald no CC
  wald <- ci_prop_diff_wald(x = resp, by = trt)
  expect_equal(wald$conf.low, 0.0575, tolerance = 0.02)
  expect_equal(wald$conf.high, 0.3425, tolerance = 0.02)

  # Wald CC
  wald_cc <- ci_prop_diff_wald(x = resp, by = trt, correct = TRUE)
  expect_equal(wald_cc$conf.low, 0.0441, tolerance = 0.02)
  expect_equal(wald_cc$conf.high, 0.3559, tolerance = 0.02)

  # Haldane
  haldane <- ci_prop_diff_haldane(x = resp, by = trt)
  expect_equal(haldane$conf.low, 0.0535, tolerance = 0.02)
  expect_equal(haldane$conf.high, 0.3351, tolerance = 0.02)

  # Jeffreys-Perks
  jp <- ci_prop_diff_jp(x = resp, by = trt)
  expect_equal(jp$conf.low, 0.0531, tolerance = 0.02)
  expect_equal(jp$conf.high, 0.3355, tolerance = 0.02)

  # Mee
  mee <- ci_prop_diff_mee(x = resp, by = trt)
  expect_equal(mee$conf.low, 0.0533, tolerance = 0.02)
  expect_equal(mee$conf.high, 0.3377, tolerance = 0.02)

  # Anderson-Hauck
  ha <- ci_prop_diff_ha(x = resp, by = trt)
  expect_equal(ha$conf.low, 0.0494, tolerance = 0.02)
  expect_equal(ha$conf.high, 0.3506, tolerance = 0.02)

  # Newcombe (score, noCC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = FALSE)
  expect_equal(nc$conf.low, 0.0524, tolerance = 0.02)
  expect_equal(nc$conf.high, 0.3339, tolerance = 0.02)

  # Newcombe (score, CC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = TRUE)
  expect_equal(nc$conf.low, 0.0428, tolerance = 0.02)
  expect_equal(nc$conf.high, 0.3422, tolerance = 0.02)

  # 9/10-3/10
  resp <- expand(c(9, 3), c(10, 10))
  trt <- rep(c("a", "b"), times = c(10,10))

  # Wald no CC
  wald <- ci_prop_diff_wald(x = resp, by = trt)
  expect_equal(wald$conf.low, 0.2605, tolerance = 0.02)
  expect_equal(wald$conf.high, 0.9395, tolerance = 0.02)

  # Wald CC
  wald_cc <- ci_prop_diff_wald(x = resp, by = trt, correct = TRUE)
  expect_equal(wald_cc$conf.low, 0.1605, tolerance = 0.02)
  expect_equal(wald_cc$conf.high, 1.0, tolerance = 0.02)

  # Haldane
  haldane <- ci_prop_diff_haldane(x = resp, by = trt)
  expect_equal(haldane$conf.low, 0.1777, tolerance = 0.02)
  expect_equal(haldane$conf.high, 0.8289, tolerance = 0.02)

  # Jeffreys-Perks
  jp <- ci_prop_diff_jp(x = resp, by = trt)
  expect_equal(jp$conf.low, 0.1760, tolerance = 0.02)
  expect_equal(jp$conf.high, 0.8306, tolerance = 0.02)

  # Mee
  mee <- ci_prop_diff_mee(x = resp, by = trt)
  expect_equal(mee$conf.low, 0.1821, tolerance = 0.02)
  expect_equal(mee$conf.high, 0.8370, tolerance = 0.02)

  # Anderson-Hauck
  ha <- ci_prop_diff_ha(x = resp, by = trt)
  expect_equal(ha$conf.low, 0.1922, tolerance = 0.02)
  expect_equal(ha$conf.high, 1, tolerance = 0.02)

  # Newcombe (score, noCC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = FALSE)
  expect_equal(nc$conf.low, 0.1705, tolerance = 0.02)
  expect_equal(nc$conf.high, 0.8090, tolerance = 0.02)

  # Newcombe (score, CC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = TRUE)
  expect_equal(nc$conf.low, 0.1013, tolerance = 0.02)
  expect_equal(nc$conf.high, 0.8387, tolerance = 0.02)

  # 10/10 - 0/20
  resp <- expand(c(10, 0), c(10, 20))
  trt <- rep(c("a", "b"), times = c(10,20))

  # Wald no CC
  wald <- ci_prop_diff_wald(x = resp, by = trt)
  expect_equal(wald$conf.low, 1, tolerance = 0.02)
  expect_equal(wald$conf.high, 1, tolerance = 0.02)

  # Wald CC
  wald_cc <- ci_prop_diff_wald(x = resp, by = trt, correct = TRUE)
  expect_equal(wald_cc$conf.low, 0.9250, tolerance = 0.02)
  expect_equal(wald_cc$conf.high, 1, tolerance = 0.02)

  # Haldane
  haldane <- ci_prop_diff_haldane(x = resp, by = trt)
  expect_equal(haldane$conf.low, 0.7482, tolerance = 0.02)
  expect_equal(haldane$conf.high, 1.0, tolerance = 0.02)

  # Jeffreys-Perks
  jp <- ci_prop_diff_jp(x = resp, by = trt)
  expect_equal(jp$conf.low, 0.7431, tolerance = 0.02)
  expect_equal(jp$conf.high, 1, tolerance = 0.02)

  # Mee
  mee <- ci_prop_diff_mee(x = resp, by = trt)
  expect_equal(mee$conf.low, 0.7225, tolerance = 0.02)
  expect_equal(mee$conf.high, 1, tolerance = 0.02)

  # Anderson-Hauck
  ha <- ci_prop_diff_ha(x = resp, by = trt)
  expect_equal(ha$conf.low, 0.9500, tolerance = 0.02)
  expect_equal(ha$conf.high, 1, tolerance = 0.02)

  # Newcombe (score, noCC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = FALSE)
  expect_equal(nc$conf.low, 0.6791, tolerance = 0.02)
  expect_equal(nc$conf.high, 1, tolerance = 0.02)

  # Newcombe (score, CC)
  nc <- ci_prop_diff_nc(x = resp, by = trt, correct = TRUE)
  expect_equal(nc$conf.low, 0.6014, tolerance = 0.02)
  expect_equal(nc$conf.high, 1, tolerance = 0.02)
})

test_that("Test Data", {
  # testing with data input
  response = expand(c(56, 48), c(70, 80))
  treat = rep(c("a", "b"), times = c(70,80))
  test_df <- data.frame(response1 = response,
                      treat1 = treat
  )

  # Wald
  wald <- ci_prop_diff_wald(response, treat)
  wald_w_df <- ci_prop_diff_wald(response, treat, data = test_df)
  expect_equal(wald, wald_w_df)

  # Haldane
  haldane <- ci_prop_diff_haldane(response, treat)
  haldane_w_df <- ci_prop_diff_haldane(response, treat, data = test_df)
  expect_equal(haldane, haldane_w_df)

  # Jeffreys-Perks
  jp <- ci_prop_diff_jp(response, treat)
  jp_w_df <- ci_prop_diff_jp(response, treat, data = test_df)
  expect_equal(jp, jp_w_df)

  # Mee
  mee <- ci_prop_diff_mee(response, treat)
  mee_w_df <- ci_prop_diff_mee(response, treat, data = test_df)
  expect_equal(mee, mee_w_df)

  # Anderson-Hauck
  ha <- ci_prop_diff_ha(response, treat)
  ha_w_df <- ci_prop_diff_ha(response, treat, data = test_df)
  expect_equal(ha, ha_w_df)

  # Newcombe (no CC)
  nc <- ci_prop_diff_nc(response, treat, correct = FALSE)
  nc_w_df <- ci_prop_diff_nc(response, treat, data = test_df)
  expect_equal(nc, nc_w_df)

  # Newcombe (CC)
  nc <- ci_prop_diff_nc(response, treat, correct = TRUE)
  nc_w_df <- ci_prop_diff_nc(response, treat, correct = TRUE,data = test_df)
  expect_equal(nc, nc_w_df)

})


test_that("Test Print", {
  response = expand(c(56, 48), c(70, 80))
  treat = rep(c("a", "b"), times = c(70,80))

  # Wald no CC
  expect_snapshot(
    ci_prop_diff_wald(response, treat)
  )

  # Wald CC
  expect_snapshot(
    ci_prop_diff_wald(response, treat, correct = TRUE)
  )

  # Haldane
  expect_snapshot(
    ci_prop_diff_haldane(response, treat)
  )

  # Jeffreys-Perks
  expect_snapshot(
    ci_prop_diff_jp(response, treat)
  )

  # Mee
  expect_snapshot(
    ci_prop_diff_mee(response, treat)
  )

  # Mee with delta
  expect_snapshot(
    print(ci_prop_diff_mee(response, treat, delta = c(0, 0.1)))
  )

  # Anderson-Hauck
  expect_snapshot(
    ci_prop_diff_ha(response, treat)
  )

  # Newcombe (no CC)
  expect_snapshot(
    ci_prop_diff_nc(response, treat, correct = FALSE)
  )

  # Newcombe (CC)
  expect_snapshot(
    print(ci_prop_diff_nc(response, treat, correct = TRUE))
  )

})
