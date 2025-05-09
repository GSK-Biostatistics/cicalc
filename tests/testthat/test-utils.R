test_that("expand converts single count to binary vector", {
  # Basic case - some successes, some failures
  expect_equal(expand(4, 10), c(rep(TRUE, 4), rep(FALSE, 6)))

  # Edge case - all successes
  expect_equal(expand(5, 5), rep(TRUE, 5))

  # Edge case - no successes
  expect_equal(expand(0, 5), rep(FALSE, 5))

  # Check that the result is a logical vector
  expect_type(expand(3, 7), "logical")
})

test_that("expand handles multiple groups correctly", {
  # Two groups with different success rates
  result <- expand(c(3, 7), c(10, 10))
  expect_equal(result, c(rep(TRUE, 3), rep(FALSE, 7), rep(TRUE, 7), rep(FALSE, 3)))
  expect_length(result, 20)

  # Three groups with different sample sizes
  result <- expand(c(2, 4, 1), c(5, 8, 3))
  expect_equal(
    result,
    c(rep(TRUE, 2), rep(FALSE, 3), rep(TRUE, 4), rep(FALSE, 4), rep(TRUE, 1), rep(FALSE, 2))
  )
  expect_length(result, 16)
})

test_that("expand handles edge cases", {

  # Zero trials
  expect_length(expand(0, 0), 0)

  # Single success in single trial
  expect_equal(expand(1, 1), TRUE)
})

test_that("expand validates inputs", {
  # Not integer-like values
  expect_error(expand("a", 5))
  expect_error(expand(5, "b"))
  expect_error(expand(1.5, 5))

  # Negative values should error
  expect_error(expand(-1, 5))
  expect_error(expand(5, -2))

  # More successes than trials should error
  expect_error(expand(6, 5))

  # Different length vectors without recycling
  expect_error(expand(c(1, 2, 3), c(5, 10)))
})

