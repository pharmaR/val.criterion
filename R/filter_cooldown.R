#' Filter by date
#'
#' Implement a cooldown filter so that only packages that are older than a given threshold are installed.
#' 
#' This helps to prevent installing packages recently published with a bug or an infiltration. 
#' For the same reason it prevents installing updates and patches of recently fixed packages.
#' @param accepted A date of some time in the past until which published packages are accepted.
#' @param ... Other arguments passed to package_filter.
#'
#' @returns A filter 
#' @export
#'
#' @examples
#' ap_wo <- available.packages()
#' dim(ap_wo)
#' ap <- available.packages(filters = cooldown())
#' dim(ap)
cooldown <- function(accepted = Sys.Date() - 2*7, ...){
  stopifnot(is.Date(accepted))
  stopifnot(accepted < Sys.Date())
  package_filter(as.Date(Date) <= accepted, ...)
  }