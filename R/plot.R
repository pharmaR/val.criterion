#' Plot the decision tree of a criterion filter
#'
#' @param x A [`filter`] object, created using [`package_filter`] for use with
#'   [`available.packages()`].
#' @param db Optionally, a [`available.packages()`] database to use when
#'   plotting package counts at each stage of the decision process. By default,
#'   will use the [`available.packages()`] output as though the filter had not
#'   been applied.
#'
#'
#' @importFrom DiagrammeR
#' @exportS3Method plot "val.criterion::filter"
`plot.val.criterion::filter` <- function(
  x,
  db = x(as = "data.frame"),
  ...
) {
  if (!requireNamespace("DiagrammeR", quietly = TRUE)) {
    stop("package 'DiagrammeR' is required to plot filter decision trees.")
  }

  expr <- filter_get_cond(x)
  eval_envir <- build_filter_envir(values = db, envir = environment(x))
  term_envir <- build_filter_term_envir(envir = environment(x))

  g <- DiagrammeR::create_graph()

  label <- paste0("Available Packages (", nrow(db), ")")
  g <- DiagrammeR::add_node(g, label = label)

  #' takes an expression, adds appropriate nodes to graph
  #'
  #' @returns either `integer` node ID if a node was added, or a
  #'   `character` label if the current expression cannot be added as a node.
  build_graph <- function(expr, from = 1L) {
    if (!is.call(expr)) {
      value <- tryCatch(
        eval(expr, envir = term_envir),
        error = function(e) expr
      )

      if (inherits(value, "term")) {
        return(value)
      } else {
        return(as.character(expr))
      }
    }

    switch(
      as.character(expr[[1L]]),
      "&" = {
        # add nodes in sequence
        from <- build_graph(expr[[2L]], from = from)
        label <- build_graph(expr[[3L]], from = from)
        if (is.character(label)) {
          label <- sprintf("%s (%d)", label, nrow(db))
          g <<- DiagrammeR::add_node(g, label = label, from = from)
        }
      },
      "|" = {
        # add nodes in parallel
        build_graph(expr[[2L]], from = from)
        build_graph(expr[[3L]], from = from)
        db <<- db[eval(expr, envir = eval_envir), ]
      },
      "==" = {
        sides <- list(
          build_graph(expr[[2L]], from = from),
          build_graph(expr[[3L]], from = from)
        )

        if (mode(sides[[1]]) == mode(sides[[2]])) {
          sides <- simplify2array(sides)
        }

        db <<- db[eval(expr, envir = eval_envir), ]
        if (is.integer(sides)) {
          label <- paste0("are equal (", nrow(db), ")")
          g <<- DiagrammeR::add_node(g, label = label, from = sides)
        } else if (is.list(sides)) {
          from <- if (is.integer(sides[[1]])) sides[[1]] else sides[[2]]
          label <- paste0(
            "is equal to ",
            if (is.character(sides[[2]])) sides[[2]] else sides[[1]],
            " (",
            nrow(db),
            ")"
          )
          g <<- DiagrammeR::add_node(g, label = label, from = from)
        } else {
          label <- I(sprintf(
            "<<B>%s</B> is equal to <B>%s</B> (%d)>",
            sides[[1]],
            sides[[2]],
            nrow(db)
          ))
          g <<- DiagrammeR::add_node(g, label = label, from = from)
        }
      },
      {
        return(paste(collapse = " ", deparse(expr)))
      }
    )

    DiagrammeR::count_nodes(g)
  }

  build_graph(expr)

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "layout",
    value = "dot",
    attr_type = "graph"
  )

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "shape",
    value = "rectangle",
    attr_type = "node"
  )

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "fixedsize",
    value = "false",
    attr_type = "node"
  )

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "fillcolor",
    value = "white",
    attr_type = "node"
  )

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "fillcolor",
    value = "white",
    attr_type = "node"
  )

  g <- DiagrammeR::add_global_graph_attrs(
    g,
    attr = "fontcolor",
    value = "black",
    attr_type = "node"
  )

  dot <- DiagrammeR::generate_dot(g)
  DiagrammeR::grViz(dot)
}

filter_get_rich_ast <- function(x, ...) {
  UseMethod("filter_get_rich_ast")
}

#' @exportS3Method filter_get_rich_ast val.criterion::filter
`filter_get_rich_ast.val.criterion::filter` <- function(x, ..., filter = x) {
  filter_get_rich_ast(filter_get_cond(x), ..., filter = filter)
}

#' @exportS3Method filter_get_rich_ast call
filter_get_rich_ast.call <- function(x, ..., filter) {
  to <- x[[1L]]
  desc <- filter_get_rich_call_desc(to, filter = filter)

  if (is.na(desc)) {
    return(x)
  }

  x[-1L] <- lapply(x[-1L], filter_get_rich_ast, filter = filter)
  attr(x, "desc") <- desc

  x
}

filter_get_rich_call_desc <- function(x, ..., filter) {
  UseMethod("filter_get_rich_call_desc")
}

#' @exportS3Method filter_get_rich_call_desc call
filter_get_rich_call_desc.call <- function(x, ..., filter) {
  sym_name <- if (is.call(x) && as.character(x[[1L]]) %in% c("::", ":::")) {
    as.character(x)
  } else if (!is.symbol(x[[1L]])) {
    NULL
  } else if (!missing(filter)) {
    name <- as.character(x)
    obj <- eval(x[[1L]], envir = environment(filter))
    ns <- environment(obj)
    paste0(
      if (isNamespace(ns)) {
        paste0(getNamespaceName(ns), "::")
      },
      name
    )
  } else {
    as.character(x)
  }

  dispatch <- structure(list(), class = paste0("call_to_", sym_name))
  UseMethod("filter_get_rich_call_desc", dispatch)
}

filter_get_cond <- function(f) {
  expr <- attr(f, "cond")

  # don't bother printing enclosing curly braces
  while (is.call(expr) && expr[[1L]] == "{") {
    expr <- expr[[2L]]
  }

  expr
}

#' @exportS3Method print val.criterion::filter
`print.val.criterion::filter` <- function(x, ...) {
  cat("<", filter_class(), ">\n", sep = "")
  print(filter_get_cond(x), ...)
}
