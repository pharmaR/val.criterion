test_that("default_actions describes install-style calls to intercept", {
  da <- default_actions()

  expect_s3_class(da, "data.frame")
  expect_named(da, c("fn", "arg", "action"))
  expect_gte(nrow(da), 5L)

  # `fn` and `action` are stored as unevaluated calls/symbols
  expect_true(all(vapply(da$fn, is.call, logical(1L))))
  expect_type(da$arg, "character")

  # the standard installers are covered
  fns <- vapply(da$fn, deparse, character(1L))
  expect_true("utils::install.packages" %in% fns)
  expect_true("renv::install" %in% fns)
})

test_that("set_last_rejected / last_rejected round-trip", {
  old <- last_rejected()
  withr::defer(set_last_rejected(old))

  set_last_rejected(c("pkgA", "pkgB"))
  expect_identical(last_rejected(), c("pkgA", "pkgB"))
})

test_that("last_rejected_permit promotes rejected packages to exceptions", {
  old_rejected <- last_rejected()
  old_exceptions <- opt("exceptions")
  withr::defer({
    set_last_rejected(old_rejected)
    opt_set("exceptions", old_exceptions)
  })

  opt_set("exceptions", character(0L))
  set_last_rejected(c("foo", "bar"))

  added <- last_rejected_permit(quiet = TRUE)
  expect_setequal(added, c("foo", "bar"))
  expect_setequal(opt("exceptions"), c("foo", "bar"))
})

test_that("last_rejected_permit only adds packages that are not yet exceptions", {
  old_rejected <- last_rejected()
  old_exceptions <- opt("exceptions")
  withr::defer({
    set_last_rejected(old_rejected)
    opt_set("exceptions", old_exceptions)
  })

  opt_set("exceptions", "foo")
  set_last_rejected(c("foo", "bar"))

  added <- last_rejected_permit(quiet = TRUE)
  expect_identical(added, "bar")
})

test_that("action_disallow aborts when a filtered package is required", {
  old_rejected <- last_rejected()
  withr::defer(set_last_rejected(old_rejected))

  db <- cbind(
    Package = "badpkg",
    Version = "1.0",
    Repository = "<filtered>",
    Depends = NA_character_,
    Imports = NA_character_,
    LinkingTo = NA_character_
  )
  rownames(db) <- "badpkg"

  expect_error(
    action_disallow("badpkg", db = db),
    "excluded due to package filters"
  )

  # the offending package is recorded for later inspection
  expect_identical(last_rejected(), "badpkg")
})

test_that("handle_actions is a no-op without an installer on the call stack", {
  db <- cbind(Package = "p", Repository = "https://example.com")
  rownames(db) <- "p"
  expect_identical(handle_actions(default_actions(), db), db)
})
