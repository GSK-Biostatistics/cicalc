test_that("ci_prop_diff_mn matches the values in the paper", {

  # 56/70-48/80
  x <- c(rep(1, 56), rep(0, 14))
  y <- c(rep(1, 48), rep(0, 32))
  norm <- ci_prop_diff_mn(x, y)

  expect_equal(norm$conf.low, 0.0528, tolerance = 0.02)
  expect_equal(norm$conf.high, 0.3382, tolerance = 0.02)


  # 9/10-3/10
  x <- c(rep(TRUE, 9), rep(FALSE, 1))
  y <- c(rep(TRUE, 3), rep(FALSE, 7))
  small <- ci_prop_diff_mn(x, y)

  expect_equal(small$conf.low, 0.1700, tolerance = 0.3)
  expect_equal(small$conf.high, 0.8406, tolerance = 0.02)


  # 10/10 - 0/20
  x <- rep(1, 10)
  y <-  rep(0, 20)
  extreme <- ci_prop_diff_mn(x, y)

  expect_equal(extreme$conf.low, 0.7156, tolerance = 0.02)
  expect_equal(extreme$conf.high, 1.0000, tolerance = 0.02)

})
