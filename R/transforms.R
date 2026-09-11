#' Filter Transforms
#'
#' Helper transforms intended for use within a [`package_filter()`] expression,
#' where they are applied to metric fields to express relative criteria.
#'
#' @param x `numeric` vector of metric values, typically a metric field
#'   referenced by name from within a filter expression.
#'
#' @return `percentile()` returns a `numeric` vector the same length as `x`,
#'   giving each element's empirical cumulative percentile (its
#'   [`stats::ecdf()`] evaluated at `x`), between `0` and `1`.
#'
#' @examples
#' percentile(c(10, 20, 30, 40))
#'
#' # used within a filter to keep only relatively popular packages
#' \dontrun{
#' package_filter({ percentile(downloads_total) >= 0.25 })
#' }
#'
#' @importFrom stats ecdf
#' @export
percentile <- function(x) {
  ecdf(x)(x)
}
