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
