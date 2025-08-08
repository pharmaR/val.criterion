# `val.criterion`

## Installation

``` r
# install.packages("pak")
pak::pak("pharmaR/val.criterion")
```

## Overview

Configure, `package_filter()`s, drawing on a repository of package qualities
to automatically apply a use policy.

## Quick Start

For the sake of example, we'll simulate a tiny repository.

> [!NOTE]
> Typically you'd combine a repository of package metadata with a package source
> like `CRAN`. Since we're simulating fake packages, we'll use it for both our
> metric and package repositories.

```r
library(val.criterion)
library(val.meter)
repo <- val.meter::random_repo(n = 10)
```

Use `package_filter()` to limit ourselves to high quality packages. In this
example we enforce a trivial policy, requiring that all packages were
checked without error and that they're in the top 75% of packages by
number of downloads.

```r
options(
  repos = repo,  # for the sake of example
  val.criterion.repos = repo,
  available_packages_filters = package_filter({
    r_cmd_check_error_count == 0 &
      percentile(downloads_total) >= 0.25
  })
)
```

We can inspect our package database and see that they now point to a
`Repository` called `<filtered>`. They're still there, but they'd fail to
install because this is not a real repository url. Let's see what happens
if we try to install it anyways!

```r
# grab the first package that was disallowed by our filter
pkgs <- as.data.frame(available.packages())
filtered_pkg <- subset(pkgs, Repository == "<filtered>")$Package[[1L]]
#> ℹ excluding 3 packages according to package filtering policy
```


```r
install.packages(filtered_pkg)
#> Installing package into ‘/usr/local/lib/R/site-library’
#> (as ‘lib’ is unspecified)
#> Error in `install.packages()`:
#> ! required packages are excluded due to package filters
#> "admirr"
#> ℹ To explain, use `val.criterion::last_rejected()`
#> ℹ To permit exceptions, use `val.criterion::last_rejected_permit()`
#> Run `rlang::last_trace()` to see where the error occurred.
```
