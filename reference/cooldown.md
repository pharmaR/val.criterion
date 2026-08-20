# Filter by date

Implement a cooldown filter so that only packages that are older than a
given threshold are installed.

## Usage

``` r
cooldown(accepted = Sys.Date() - 2 * 7, ...)
```

## Arguments

- accepted:

  A date of some time in the past until which published packages are
  accepted.

- ...:

  Other arguments passed to
  [`package_filter`](https://pharmar.github.io/val.criterion/reference/package_filter.md).

## Value

A
[`package_filter`](https://pharmar.github.io/val.criterion/reference/package_filter.md)

## Details

This helps to prevent installing packages recently published with a bug
or an infiltration. For the same reason it prevents installing updates
and patches of recently fixed packages.

## Examples

``` r
# entire available packages set
ap_complete <- available.packages()
nrow(ap_complete)
#> [1] 24728

# available packages with cooldown filter applied
ap <- available.packages(fields = "Published", filters = cooldown())
#> ℹ excluding 103 packages according to package filtering policy
ap <- subset(as.data.frame(ap), as.logical(Include))
nrow(ap)
#> [1] 0
```
