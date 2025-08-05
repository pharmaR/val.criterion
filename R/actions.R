default_monitored_calls <- local({
  x <- list(
    list(
      type = "install",
      fn = quote(utils::install.packages),
      arg = "pkgs"
    ),
    list(
      type = "install",
      fn = quote(utils::download.packages),
      arg = "pkgs"
    ),
    list(
      type = "install",
      fn = quote(pak::pak),
      arg = "pkg"
    ),
    list(
      type = "install",
      fn = quote(pak::pkg_install),
      arg = "pkg"
    ),
    list(
      type = "install",
      fn = quote(renv::install),
      arg = "packages"
    )
  )

  df <- data.frame(type = sapply(x, `[[`, "type"))
  df$fn <- lapply(x, `[[`, "fn")
  df$arg <- sapply(x, `[[`, "arg")
  df
})




find_context <- function(monitor = opt("monitor"), exhaustive = FALSE) {
  # grab function objects, ignore functions for namespaces that aren't loaded
  monitor_fn <- Filter(Negate(is.null), lapply(
    monitor$fn,
    function(i) {
      ns <- as.character(i[[2L]])
      fn <- as.character(i[[3L]])
      if (isNamespaceLoaded(ns)) get0(fn, envir = getNamespace(ns))
    }
  ))

  # walk the call stack at time of packages access to take action if triggered
  # by monitored calls
  contexts <- list()
  for (which in seq_len(sys.nframe())) {
    fn <- sys.function(which)
    if (any(is_monitored <- vlapply(monitor_fn, identical, fn))) {
      i <- which(is_monitored)[[1L]]
      contexts[[length(contexts) + 1L]] <- list(
        obj = fn,
        type = monitor$type[[i]],
        fn = monitor$fn[[i]],
        arg = monitor$arg[[i]],
        call = match.call(
          fn,
          call = sys.call(which),
          expand.dots = TRUE,
          envir = sys.frame(which)
        )
      )
    }
  }

  contexts
}

warn_filter_exception <- function(
  context,
  db,
  envir = parent.frame()
) {
  excluded_pkgs <- character(0L)
  for (ctx in context) {
    # get packages from monitored call
    pkgs <- ctx$call[[ctx$arg]]
    permitted_pkgs <- db[, "Package"] %in% pkgs & db[, "Include"]
    excluded_pkgs <- c(excluded_pkgs, setdiff(pkgs, permitted_pkgs))
  }

  if (length(excluded_pkgs)) {
    cli::cli_warn(
      "Package(s) {.str {excluded_pkgs}} are being installed despite being 
       omitted by package filter. To permit this package as an exception and
       avoid further warnings, consider adding to 
       `options(val.criterion.exceptions)`"
    )
  }
}
