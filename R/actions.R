default_actions <- function() {
  x <- list(
    list(
      fn = quote(utils::install.packages),
      arg = "pkgs",
      action = quote(action_disallow)
    ),
    list(
      fn = quote(utils::download.packages),
      arg = "pkgs",
      action = quote(action_disallow)
    ),
    list(
      fn = quote(pak::pak),
      arg = "pkg",
      action = quote(action_disallow)
    ),
    list(
      fn = quote(pak::pkg_install),
      arg = "pkg",
      action = quote(action_disallow)
    ),
    list(
      fn = quote(renv::install),
      arg = "packages",
      action = quote(action_disallow)
    )
  )

  df <- data.frame()[seq_along(x), ]
  rownames(df) <- NULL

  df$fn <- lapply(x, `[[`, "fn")
  df$arg <- sapply(x, `[[`, "arg")
  df$action <- lapply(x, `[[`, "action")

  df
}

last <- new.env()

set_last_rejected <- function(pkgs) {
  last$rejected <- pkgs
}

#' @export
last_rejected <- function() {
  last$rejected
}

#' @export
last_rejected_permit <- function(quiet = FALSE) {
  new_exceptions <- setdiff(last_rejected(), opt("exceptions"))
  n <- length(new_exceptions)
  n

  if (!quiet) {
    cli::cli_alert_info("adding {.val {n}} packages as new filter exceptions")
  }

  opt_set("exceptions", new_exceptions)
  invisible(new_exceptions)
}

#' @export
action_disallow <- function(pkgs, db, envir = parent.frame()) {
  pkgs_deps <- unlist(tools::package_dependencies(
    packages = pkgs,
    db = db,
    which = "strong",
    recursive = TRUE
  ))

  all_pkgs <- unique(c(pkgs, pkgs_deps))
  is_filtered <- db[all_pkgs, "Repository"] == "<filtered>"
  excluded_pkgs <- all_pkgs[is_filtered]
  excluded_pkgs # appease lintr

  # register excluded packages for user-facing inspection
  set_last_rejected(excluded_pkgs)

  cli::cli_abort(
    "required packages are excluded due to package filters",
    body = cli::format_inline("{.str {excluded_pkgs}}"),
    footer = cli::cli_fmt({
      cli::cli_alert_info(paste0(
        "To ",
        cli::style_italic(cli::col_blue("explain")),
        ", use {.run {packageName()}::last_rejected()}"
      ))
      cli::cli_alert_info(paste0(
        "To ",
        cli::style_italic(cli::col_green("permit exceptions")),
        ", use {.run {packageName()}::last_rejected_permit()}"
      ))
    }),
    call = envir
  )

  db
}

handle_actions <- function(actions, db) {
  actions$fn <- lapply(actions$fn, function(expr) {
    ns <- as.character(expr[[2L]])
    name <- as.character(expr[[3L]])
    if (isNamespaceLoaded(ns)) getNamespace(ns)[[name]]
  })

  for (i in seq_len(sys.nframe())) {
    frame <- sys.frame(i)
    sys_fn <- sys.function(i)
    action_i <- Position(function(act_fn) identical(sys_fn, act_fn), actions$fn)

    if (is.na(action_i)) {
      next
    }

    action <- actions$action[[action_i]]
    arg <- actions$arg[[action_i]]
    pkgs <- get0(arg, envir = frame)

    if (!is.function(action)) {
      action <- eval(action, topenv())
    }

    db <- action(pkgs, db, envir = frame)
    break
  }

  db
}
