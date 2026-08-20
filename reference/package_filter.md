# Create a Package Filter

Create a Package Filter

## Usage

``` r
package_filter(
  cond,
  repos = opt("repos"),
  exceptions = opt("exceptions"),
  actions = opt("actions"),
  quiet = opt("quiet"),
  db = function() available_metrics(repos = repos),
  add = TRUE,
  envir = parent.frame()
)
```

## Arguments

- cond:

  `expression` to use to filter packages. Package metrics are referred
  to by their name and are expected to be vectors of length equal to the
  number of packages. For details on metric names and types, see
  [`val.meter::metrics`](https://pharmar.github.io/val.meter/reference/metrics.html).

- repos:

  `character(n)` list of repositories to search for metrics. Defaults to
  package option, `"val.criterion.repos"`, which should direct to a
  metrics database.

- db:

  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)-style
  `matrix`, a `character` vector of repository urls, or a `function`
  which should return a the metric `matrix`. By default, will search
  through `options(repos)` for metrics databases.

- add:

  `logical(1)` used to add to existing filters or replace them entirely.
  Defaults to `TRUE`. See
  [`available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  for more details on the default filters. In most cases, you probably
  want to keep this as-is, as the default filters impose necessary
  constraints such as R version and platform consistency.

- envir:

  `environment` in which the filter expression should be evaluated.

## Examples

``` r
if (FALSE) { # \dontrun{
library(val.meter)

# simulate repository of metrics, mirroring existing available packages
repo <- val.meter::random_repo(n = 10)
options(
  repos = repo,  # for the sake of example
  val.criterion.repos = repo,
  available_packages_filters = package_filter({
    r_cmd_check_error_count == 0 &
      percentile(downloads_total) >= 0.25
  })
)

# grab the first package that was disallowed by our filter
pkgs <- available.packages()
filtered_pkg <- pkgs[pkgs$Repository == "<filtered>", ][[1L]]

# attempt to install it
install.packages(filtered_pkg)
} # }
```
