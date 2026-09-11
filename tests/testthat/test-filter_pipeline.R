# End-to-end regression test for the filtering pipeline.
#
# Guards against two coupled defects that broke `available.packages()` filtering
# whenever a populated metric database was present:
#   * `is.na()` guards on a matrix / list erroring "condition has length > 1"
#     (val.criterion #12, R/available_packages.R).
#   * `percentile()` not being exported, so the filter DSL could not resolve it
#     (val.criterion #13, R/transforms.R).
# It also depends on val.meter's `metric_coerce()` handling logical/double
# metrics (val.meter #58).

test_that("package_filter filters a metric repo end to end", {
  skip_if_not_installed("val.meter")
  skip_if_not_installed("withr")

  # attach val.meter so its lazy `pkg_words` dataset is reachable by random_repo
  withr::local_package("val.meter")

  set.seed(1)
  repo <- suppressWarnings(val.meter::random_repo(n = 8))
  withr::defer(unlink(sub("^file://", "", repo), recursive = TRUE))

  withr::local_options(
    repos = repo,
    val.criterion.repos = repo,
    available_packages_filters = package_filter({
      r_cmd_check_error_count == 0 & percentile(downloads_total) >= 0.25
    })
  )

  # previously errored with "the condition has length > 1" (#12) and
  # "could not find function 'percentile'" (#13)
  expect_no_error(pkgs <- available.packages())

  expect_true(nrow(pkgs) > 0)
  expect_true("Repository" %in% colnames(pkgs))

  # the filter should exclude some, but not all, packages
  n_filtered <- sum(pkgs[, "Repository"] == "<filtered>", na.rm = TRUE)
  expect_true(n_filtered > 0)
  expect_true(n_filtered < nrow(pkgs))
})
