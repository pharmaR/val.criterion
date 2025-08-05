#' Create a Package Filter
#'
#' @param cond `expression` to use to filter packages. Package metrics are
#'   referred to by their name and are expected to be vectors of length equal
#'   to the number of packages. For details on metric names and types, see
#'   [`val.meter::metrics`].
#' @param repos `character(n)` list of repositories to search for metrics.
#'   Defaults to package option, `"val.criterion.repos"`, which should direct
#'   to a metrics database.
#' @param db `available.packages()`-style `matrix`, a `character`
#'   vector of repository urls, or a `function` which should return a the metric
#'   `matrix`. By default, will search through `options(repos)` for metrics
#'   databases.
#' @param add `logical(1)` used to add to existing filters or replace them
#'   entirely. Defaults to `TRUE`. See [`available.packages()`] for more
#'   details on the default filters. In most cases, you probably want to keep
#'   this as-is, as the default filters impose necessary constraints such as
#'   R version and platform consistency.
#' @param envir `environment` in which the filter expression should be
#'   evaluated.
#'
#' @examples
#' \dontrun{
#' library(val.meter)
#'
#' # simulate repository of metrics, mirroring existing available packages
#' repo <- val.meter::random_repo()
#' options(
#'   repos = repo,  # for the sake of example
#'   val.criterion.repos = repo,
#'   available_packages_filters = package_filter(
#'     r_cmd_check_error_count == 0 &
#'     percentile(downloads_total) >= 0.25
#'   )
#' )
#' }
#'
#' @export
package_filter <- local({
  filter_id <- 0L
  filter_name <- function() {
    filter_id <<- filter_id + 1L
    paste(sep = "-", packageName(), "filter", filter_id)
  }

  function(
    cond,
    repos = opt("repos"),
    exceptions = opt("exceptions"),
    actions = list(install = opt("install.action")),
    quiet = opt("quiet"),
    db = function() available_metrics(repos = repos),
    add = TRUE,
    envir = parent.frame()
  ) {
    metric_db <- db
    cond <- substitute(cond)

    # used for messaging only on first use with set of input conditions
    signal_state <- NULL

    filter <- function(db) {
      if (is.function(metric_db)) {
        metric_db <- metric_db()
      } else if (is.character(metric_db) && !is.matrix(metric_db)) {
        metric_db <- available_metrics(repos = metric_db)
      }

      # define NAs in case where metric_db is missing known metrics
      metric_defaults <- as.list(rep_len(NA, length.out = length(metrics())))
      names(metric_defaults) <- names(metrics())

      # build our evaluation environemnt and evaluate filter expression
      db <- db[!is.na(db[, "Package"]), ]
      db <- as.data.frame(db)
      df <- convert(class_package_matrix(metric_db), class_metric_data_frame)
      df$Metric <- TRUE
      df <- merge(db, df, by = c("Package", "Version", "MD5sum"), all = TRUE)
      rownames(df) <- df[, "Package"]

      # evaluate filter
      envir <- build_filter_envir(values = df, envir = envir)
      df$Include <- eval(cond, envir = envir)
      df$Include <- df$Include | df$Package %in% exceptions

      # memoise messaging using run state so we're not too noisy
      if ((n <- sum(!df$Include, na.rm = TRUE)) > 0) {
        signal_state <<- signal_filter_message(n, signal_state, quiet = quiet)
      }

      # trigger actions as needed
      actions$install(find_context(), db = df)

      # return expected available packages matrix format
      df <- as.matrix(df[df$Include == "TRUE", ])
      invisible(df)
    }

    out <- list()
    out[[filter_name()]] <- filter
    out$add <- TRUE

    out
  }
})

build_filter_envir <- function(
  values,
  defaults = metric_defaults(),
  envir = parent.frame()
) {
  defaults_envir <- with(defaults, environment())
  parent.env(defaults_envir) <- envir
  value_envir <- with(values, environment())
  parent.env(value_envir) <- defaults_envir
  value_envir
}

signal_filter_message <- function(
  n,
  state = options()[c(
    "repos",
    "val.criterion.repos",
    "val.criterion.quiet",
    "available_packages_filters"
  )],
  quiet = opt("quiet")
) {
  # re-evaluate default state argument
  expected_state <- eval(formals()[["state"]], envir = parent.frame())
  if (!quiet && !identical(state, expected_state)) {
    cli::cli_alert_info(
      "excluding {.val {n}} packages according to package filtering policy"
    )
  }

  expected_state
}

repo_packages_url <- function(repo) {
  file.path(repo, "src", "contrib", "PACKAGES")
}

repo_is_metric_db <- function(url) {
  url <- repo_packages_url(url)
  line <- readLines(url, n = 1L)
  identical(read.dcf(textConnection(line))[[1L, "Format"]], "Metrics")
}

#' @importFrom val.meter metrics
available_metric_fields <- function(repos = getOption("repos")) {
  n <- length(metrics())
  repos_pkgs <- repo_packages_url(repos)
  fields <- unique(unlist(lapply(repos_pkgs, function(url) {
    # read first 20 + N_metrics lines, enough to discover all metrics
    nlines <- paste0(collapse = "\n", readLines(url, n = 20 + n))
    colnames(read.dcf(textConnection(nlines)))
  })))

  is_metric <- startsWith(fields, "Metric/")
  fields[is_metric]
}

#' @importFrom val.meter class_package_matrix class_metric_data_frame
available_metrics <- function(repos = opt("repos")) {
  is_metric_db <- vlapply(repos, repo_is_metric_db)
  db <- available.packages(
    repos = repos[is_metric_db],
    fields = available_metric_fields(repos = repos),
    filters = list()
  )

  # drop rows with missing Package field (used to drop `Format: ` header)
  db[!is.na(db[, "Package"]), ]
}
