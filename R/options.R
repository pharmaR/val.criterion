#' @eval options::as_roxygen_docs()
NULL

#' Options As Parameters
#' @eval options::as_params()
#' @name options_params
#' @keywords internal
NULL

define_options(
  "Metrics repositories to use implicitly for package filters. Will be
  accessed using [`available.packages()`], so repository paths should follow
  similar conventions.",
  repos = character(0L),

  "Packages that should be treated as exceptions. Even if they would otherwise
  be filtered out using preset filters, they would still be discoverable and
  installable without issue.",
  exceptions = character(0L),

  "Wether filtering operations should be reported as they're being queried.
  Passed to the `quiet` parameter of individual functions where it's used.",
  quiet = FALSE,

  "A set of calls that should prompt actions. See [`default_actions`] for an
  example of the expected format.",
  actions = default_actions()
)
