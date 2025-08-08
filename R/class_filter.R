filter_class <- function() {
  paste0(packageName(), "::filter")
}

discover_filter <- function() {
  filters <- getOption("available_packages_filters", list())
  Find(function(f) inherits(f, filter_class()), filters)
}
