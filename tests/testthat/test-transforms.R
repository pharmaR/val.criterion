test_that("percentile returns ecdf-based quantile positions", {
  expect_equal(percentile(c(10, 20, 30, 40)), c(0.25, 0.5, 0.75, 1))
})

test_that("percentile is order-independent per element", {
  x <- c(40, 10, 30, 20)
  expect_equal(percentile(x), c(1, 0.25, 0.75, 0.5))
})

test_that("percentile handles ties by sharing the higher rank", {
  expect_equal(percentile(c(1, 1, 2)), c(2 / 3, 2 / 3, 1))
})

test_that("percentile of a single value is 1", {
  expect_equal(percentile(42), 1)
})
