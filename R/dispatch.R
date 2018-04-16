
dispatch2 <- function(generic, x, y, env = caller_env(2L)) {
  fn <- get_method2(generic, x, y, env)
  check_dispatch(fn, generic, x, y)

  call <- sys.call(sys.parent(1L))
  node_poke_car(call, fn)
  eval_bare(call, env)
}
dispatch2_ <- function(.generic, .x, .y, ..., .env) {
  fn <- get_method2(.generic, .x, .y, .env)
  check_dispatch(fn, .generic, .x, .y)
  fn(.x, .y, ...)
}
check_dispatch <- function(fn, generic, x, y) {
  if (is_null(fn)) {
    c1 <- class(x)[[1]]
    c2 <- class(y)[[1]]
    abort(sprintf("Can't find a `%s()` method for `%s` and `%s`", generic, c1, c2))
  }
}

get_method2 <- function(generic, x, y, env = caller_env()) {
  c1 <- class(x)
  c2 <- class(y)
  info <- get_method2_info(generic, c1, c2, env)

  if (is_null(info)) {
    return(NULL)
  }

  dispatched <- new_list(2L, names = info$classes)

  if (c1 == class(x)[[1]]) {
    dispatched[[c1]] <- x
    dispatched[[c2]] <- y
  } else {
    dispatched[[c1]] <- y
    dispatched[[c2]] <- x
  }

  fn <- info$method
  environment(fn) <- env(environment(fn), .dispatched = dispatched)
  fn
}

get_method2_info <- function(generic, c1, c2, env = caller_env()) {
  if (!length(c1) || !length(c2)) {
    abort("Object class does not have length")
  }

  classes <- sort.int(c(c1[[1]], c2[[1]]), method = "radix")
  c1 <- classes[[1]]
  c2 <- classes[[2]]

  wildcards <- list()

  while (!is_empty_env(env)) {
    table <- binary_table(env)

    if (!is_null(table)) {
      gen_table <- table[[generic]]
      fn <- gen_table[[c1]][[c2]]

      if (!is_null(fn)) {
        return(new_method_info(classes, fn))
      }

      star <- gen_table[["*"]]
      if (!is_null(star)) {
        already_found <- names(star) %in% names(wildcards)
        wildcards <- c(wildcards, star[!already_found])
      }
    }

    env <- env_parent(env)
  }

  fn <- wildcards[["*"]]
  if (!is_null(fn)) {
    return(new_method_info(classes, fn))
  }

  NULL
}

new_method_info <- function(classes, method) {
  list(classes = classes, method = method)
}

def_method2 <- function(.class1, .class2, ..., .env = caller_env()) {
  stopifnot(
    is_string(.class1),
    is_string(.class2)
  )

  # Sort the classes (with radix method for C collation) to enforce
  # commutativity of binary methods. Should commutativity be a
  # property of the generic instead?
  classes <- sort.int(c(.class1, .class2), method = "radix")
  c1 <- classes[[1]]
  c2 <- classes[[2]]

  methods <- list2(...)
  stopifnot(
    length(methods) >= 1,
    is_named(methods),
    map_lgl(methods, is_function)
  )

  table <- binary_table(.env)
  if (is_null(table)) {
    table <- .env[[binary_table_name]] <- new_environment()
  }

  for (name in names(methods)) {
    mtd <- table[[name]]
    if (is_null(table[[name]])) {
      mtd <- table[[name]] <- new_environment()
    }

    if (is_null(mtd[[c1]])) {
      mtd[[c1]] <- list()
    } else if (!is_null(mtd[[c1]][[c2]])) {
      warn(sprintf("Overriding method `%s()` for classes `%s` and `%s`", name, c1, c2))
    }

    mtd[[c1]][[c2]] <- methods[[name]]
  }

  invisible(methods)
}

binary_table_name <- ".__rlang_binary_strict_methods__."

binary_table <- function(env) {
  env[[binary_table_name]]
}
