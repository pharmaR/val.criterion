#' @importFrom stats ecdf
percentile <- function(x) {
  ecdf(x)(x)
}
