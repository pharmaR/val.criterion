# Options As Parameters

Options As Parameters

## Arguments

- quiet:

  Wether filtering operations should be reported as they're being
  queried. Passed to the `quiet` parameter of individual functions where
  it's used. (Defaults to `FALSE`, overwritable using option
  'val.criterion.quiet' or environment variable 'R_VAL_CRITERION_QUIET')

- repos:

  Metrics repositories to use implicitly for package filters. Will be
  accessed using
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html),
  so repository paths should follow similar conventions. (Defaults to
  `character(0L)`, overwritable using option 'val.criterion.repos' or
  environment variable 'R_VAL_CRITERION_REPOS')

- exceptions:

  Packages that should be treated as exceptions. Even if they would
  otherwise be filtered out using preset filters, they would still be
  discoverable and installable without issue. (Defaults to
  `character(0L)`, overwritable using option 'val.criterion.exceptions'
  or environment variable 'R_VAL_CRITERION_EXCEPTIONS')

- actions:

  A set of calls that should prompt actions. See
  [`default_actions`](https://pharmar.github.io/val.criterion/reference/default_actions.md)
  for an example of the expected format. (Defaults to
  [`default_actions()`](https://pharmar.github.io/val.criterion/reference/default_actions.md),
  overwritable using option 'val.criterion.actions' or environment
  variable 'R_VAL_CRITERION_ACTIONS')
