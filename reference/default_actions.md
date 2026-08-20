# Default actions

Decide what to do when some functions are called.

## Usage

``` r
default_actions()
```

## Value

A `data.frame` with three columns: functions, arguments and actions.

## See also

Other actions:
[`action_disallow()`](https://pharmar.github.io/val.criterion/reference/action_disallow.md),
[`last_rejected()`](https://pharmar.github.io/val.criterion/reference/last_rejected.md),
[`last_rejected_permit()`](https://pharmar.github.io/val.criterion/reference/last_rejected_permit.md)

## Examples

``` r
df <- default_actions()
```
