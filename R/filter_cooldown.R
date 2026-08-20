#' Filter by date
#'
#' Implement a cooldown filter so that only packages that are older than a given
#' threshold are installed.
#'
#' This helps to prevent installing packages recently published with a bug or an
#' infiltration. For the same reason it prevents installing updates and patches
#' of recently fixed packages.
#'
#' @param accepted A date of some time in the past until which published
#'   packages are accepted.
#' @param ... Other arguments passed to [`package_filter`].
#'
#' @returns A [`package_filter`]
#'
#' @examples
#' # entire available packages set
#' ap_complete <- available.packages()
#' nrow(ap_complete)
#'
#' # available packages with cooldown filter applied
#' ap <- available.packages(fields = "Published", filters = cooldown())
#' ap <- subset(as.data.frame(ap), as.logical(Include))
#' nrow(ap)
#'
#' @export
cooldown <- function(accepted = Sys.Date() - 2 * 7, ...) {
  stopifnot(is(accepted, "Date"))
  stopifnot(accepted < Sys.Date())
  package_filter(as.Date(Published) <= accepted, ...)
}
