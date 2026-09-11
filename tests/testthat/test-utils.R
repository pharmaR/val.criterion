test_that("vlapply returns a logical vector", {
  expect_identical(vlapply(1:3, function(x) x > 1L), c(FALSE, TRUE, TRUE))
})

test_that("vcapply returns a character vector", {
  expect_identical(vcapply(1:2, as.character), c("1", "2"))
})

test_that("viapply returns an integer vector", {
  expect_identical(viapply(1:2, function(x) x + 1L), c(2L, 3L))
})

test_that("vnapply returns a numeric vector", {
  expect_identical(vnapply(1:2, function(x) x / 2), c(0.5, 1))
})

test_that("the vapply wrappers enforce their return type", {
  expect_error(vlapply(1:2, function(x) "not logical"))
  expect_error(viapply(1:2, function(x) "not integer"))
})
