test_that("ci_prop_diff_mn matches the values in the paper", {
  # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
  # 56/70-48/80
  resp <- expand(c(56, 48), c(70, 80))
  trt <- rep(c("a", "b"), times = c(70,80))
  norm <- ci_prop_diff_mn(x = resp, by = trt)

  expect_equal(norm$conf.low, 0.0528, tolerance = 0.02)
  expect_equal(norm$conf.high, 0.3382, tolerance = 0.02)


  # 9/10-3/10
  resp <- expand(c(9, 3), c(10, 10))
  trt <- rep(c("a", "b"), times = c(10,10))
  small <- ci_prop_diff_mn(x = resp, by = trt)

  expect_equal(small$conf.low, 0.1700, tolerance = 0.3)
  expect_equal(small$conf.high, 0.8406, tolerance = 0.02)


  # 10/10 - 0/20
  resp <- expand(c(10, 0), c(10, 20))
  trt <- rep(c("a", "b"), times = c(10,20))
  extreme <- ci_prop_diff_mn(x = resp, by = trt)

  expect_equal(extreme$conf.low, 0.7156, tolerance = 0.02)
  expect_equal(extreme$conf.high, 1.0000, tolerance = 0.02)

  # testing with data input
  df <- dplyr::tibble(response = resp,
               treat = trt
               )
  extreme_w_df <- ci_prop_diff_mn(response, treat, data = df)
  expect_equal(extreme_w_df, extreme)

  # Add print test
  expect_snapshot(
    ci_prop_diff_mn(response, treat, data = df)
  )
})

test_that("delta argument works", {
  resp <- expand(c(56, 48), c(70, 80))
  trt <- rep(c("a", "b"), times = c(70,80))

  single_del <- ci_prop_diff_mn(x = resp, by = trt, delta = -0.1)
  multi_del <- ci_prop_diff_mn(x = resp, by = trt, delta = c(-0.1, 0, 0.1))

  expect_equal(length(single_del$p.value), 1)
  expect_equal(length(multi_del$p.value), 3)
  expect_equal(single_del$p.value, multi_del$p.value[1])
})


test_that("ci_prop_diff_mn validates inputs correctly", {
  # Non-binary x
  expect_error(ci_prop_diff_mn(x = c(1, 2, 3), by = c("A", "B", "A")))

  # By with more than two levels
  expect_error(ci_prop_diff_mn(x = c(1, 0, 1), by = c("A", "B", "C")))

  # Mismatched lengths
  expect_error(ci_prop_diff_mn(x = c(1, 0, 1, 0), by = c("A", "B", "A")))

  # Invalid confidence level
  expect_error(ci_prop_diff_mn(x = c(1, 0), by = c("A", "B"), conf.level = 2))
  expect_error(ci_prop_diff_mn(x = c(1, 0), by = c("A", "B"), conf.level = 0))

  # Invalid delta
  expect_error(ci_prop_diff_mn(x = c(1, 0), by = c("A", "B"), delta = 2))
})

test_that("ci_prop_diff_mn_strata", {
  set.seed(123)
  trt<-c(rep(1, 100),rep(2, 100))
  response<-rbinom(200,1,.6)
  region <-1+rbinom(200,1,.5)
  sex <-1+rbinom(200,1,.5)

  exData<-data.frame(trt,response,region,sex)

  # SCORE METHOD
  strata <- interaction(region,sex)
  strata_mn <- ci_prop_diff_mn_strata(x = response, by = trt,
                         strata = strata, method = "score",
                         conf.level = 0.95, delta = -0.1)
  # The values are from running this same analysis in SAS
  expect_equal(strata_mn$conf.low, -0.16116, tolerance = 0.001)
  expect_equal(strata_mn$conf.high, 0.10954, tolerance = 0.001)

  # Check the values based on the delta = -0.1
  expect_equal(strata_mn$statistic, 1.05697, tolerance = 0.01)
  expect_equal(strata_mn$p.value, 0.14526, tolerance = 0.01)

  # SUMMARY SCORE METHOD
  # proc freq data=exRdata;
  # table region * sex * trt * response/COMMONRISKDIFF(CL=(score) column=2 test=(score));
  # ods output CommonPdiff=CommonPdiff CommonPdiffTests=CommonPdiffTests;
  # run;
  #
  # data output_mn;
  # merge CommonPdiff(where=(Method="Summary Score")) CommonPdiffTests(where=(Method="Summary Score"));
  # riskdiff=RiskDifference;
  # Z_inf=(RiskDifference--0.1)/StdErr; /*NI margin of -10%*/
  #   p_inf_1tail=1-cdf("NORMAL", Z_inf);
  # LowerCI=LowerCL;
  # UpperCI=UpperCL;
  # keep riskdiff Z_inf p_inf_1tail LowerCI UpperCI;
  # run;
  #
  # proc print data=output_mn; run;

  ss_strata_mn <- ci_prop_diff_mn_strata(x = response, by = trt,
                                      strata = strata, method = "summary score",
                                      conf.level = 0.95, delta = -0.1)

  # The values are from running this same analysis in SAS
  expect_equal(ss_strata_mn$conf.low, -0.15440, tolerance = 0.001)
  expect_equal(ss_strata_mn$conf.high, 0.10902, tolerance = 0.001)


  null_delta <- ci_prop_diff_mn_strata(x = response, by = trt,
                                      strata = strata, method = "score",
                                      conf.level = 0.95)
  expect_null(null_delta$statistic)
  expect_null(null_delta$p.value)

  vector_strata <- ci_prop_diff_mn_strata(x = response, by = trt,
                                      strata = c(region, sex),
                                      method = "score",
                                      conf.level = 0.95, delta = -0.1, data = exData)

  expect_equal(strata_mn, vector_strata)

  # Test improper strata length
  expect_error(
    ci_prop_diff_mn_strata(x = response, by = trt,
                           strata = c(region[1:54], sex),
                           method = "score",
                           conf.level = 0.95, delta = -0.1, data = exData)

  )


  # Test when strata and by are teh same
  responses <- expand(c(9, 3, 7, 2), c(10, 10, 10, 10))
  arm <- rep(c("treat", "control"), each = 20)
  strata <- rep(c("stratum1", "stratum2"), each = 20)

  # Calculate stratified confidence interval for difference in proportions
  expect_error(
    ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata)
  )
})

test_that("Test 0 response", {
  responses <- expand(c(3, 0), c(83, 46))
  arm <- rep(c("treat", "control"), c(83, 46))
  non_1_zero <- ci_prop_diff_mn(x = responses, by = arm)
  # Expected valeus from SAS
  expect_equal(non_1_zero$conf.low, -0.043, tolerance = 0.02)
  expect_equal(non_1_zero$conf.high, 0.101, tolerance = 0.02)


  # responses <- expand(c(9, 3, 7, 0), c(10, 83, 10, 46))
  # arm <- rep(c("treat", "control"), c(93, 56))
  # strata <- c(rep(c("stratum1", "stratum2"), c(10, 83)),
  #             rep(c("stratum1", "stratum2"), c(10, 46))
  # )
  # ci_prop_diff_mn_strata(x = responses, by = arm, strata = strata)
  })

test_that("Data argument works", {
  set.seed(123)

  test_df <- data.frame(
    trt = c(rep(1, 100),rep(2, 100)),
    response = rbinom(200,1,.6),
    region = 1+rbinom(200,1,.5),
    sex = 1+rbinom(200,1,.5)
  )
  by_data <- ci_prop_diff_mn_strata(x = response,  by = trt, strata = c(region, sex),
                         method = "summary score", conf.level = 0.95, data=test_df)

  by_vector <- ci_prop_diff_mn_strata(x = test_df$response,  by = test_df$trt, strata = c(test_df$region, test_df$sex),
                                method = "summary score", conf.level = 0.95)


  expect_equal(by_data, by_vector)



})
