#' @import S7
NULL


.onLoad <- function(lib, pkg) {
  S7::methods_register()
}
