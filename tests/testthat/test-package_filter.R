test_that("package_filter returns an add flag and a classed filter", {
  pf <- package_filter({ r_cmd_check_error_count == 0 })

  expect_type(pf, "list")
  expect_true(pf$add)

  filt <- pf[[2L]]
  expect_true(is.function(filt))
  expect_true(inherits(filt, filter_class()))
})

test_that("package_filter captures the condition unevaluated", {
  pf <- package_filter({ downloads_total > 100 })
  expect_identical(attr(pf[[2L]], "cond"), quote({ downloads_total > 100 }))
})

test_that("package_filter honours add = FALSE", {
  pf <- package_filter({ TRUE }, add = FALSE)
  expect_false(pf$add)
})

test_that("package_filter assigns unique, incrementing filter names", {
  n1 <- names(package_filter({ TRUE }))[2L]
  n2 <- names(package_filter({ TRUE }))[2L]

  expect_match(n1, "^val\\.criterion-filter-[0-9]+$")
  expect_match(n2, "^val\\.criterion-filter-[0-9]+$")
  expect_false(identical(n1, n2))
})
