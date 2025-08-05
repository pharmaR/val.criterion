metric_defaults <- function(x = metrics()) {
  lapply(x, function(xi) metric_coerce(NA, xi@data_class))
}
