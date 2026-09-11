test_that("repo_packages_url points at src/contrib/PACKAGES", {
  expect_identical(
    repo_packages_url("https://cran.r-project.org"),
    file.path("https://cran.r-project.org", "src", "contrib", "PACKAGES")
  )
})

test_that("repo_packages_url is vectorised over repositories", {
  urls <- repo_packages_url(c("https://a.example", "https://b.example"))
  expect_length(urls, 2L)
  expect_true(all(endsWith(urls, file.path("src", "contrib", "PACKAGES"))))
})

test_that("build_filter_envir exposes values, falling back to defaults", {
  values <- list(a = 1:3)
  defaults <- list(a = NA, b = NA)
  e <- build_filter_envir(
    values = values,
    defaults = defaults,
    envir = globalenv()
  )

  # a resolved value shadows its default
  expect_identical(get("a", envir = e), 1:3)
  # a missing value falls through to the default
  expect_identical(get("b", envir = e), NA)
})
