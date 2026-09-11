test_that("filter_class is the package-namespaced filter string", {
  expect_identical(filter_class(), "val.criterion::filter")
})

test_that("discover_filter finds a filter in available_packages_filters", {
  filt <- package_filter({ TRUE })[[2L]]
  withr::local_options(available_packages_filters = list(filt))
  expect_identical(discover_filter(), filt)
})

test_that("discover_filter ignores non-filter entries", {
  filt <- package_filter({ TRUE })[[2L]]
  withr::local_options(
    available_packages_filters = list("not a filter", filt)
  )
  expect_identical(discover_filter(), filt)
})

test_that("discover_filter returns NULL when no filter is registered", {
  withr::local_options(available_packages_filters = list())
  expect_null(discover_filter())
})
