
dispatch2 <- function(generic, x, y, env = caller_env(2)) {
  fn <- get_method2(generic, x, y, env)

  if (is_null(fn)) {
    abort(sprintf("Can't find a `%s()` method for `%s` and `%s`", generic, c1, c2))
  }

  call <- sys.call(sys.parent(1L))
  node_poke_car(call, fn)

  # Isn't this the same as `env`?
  frame <- sys.frame(sys.parent(2L))
  eval_bare(call, frame)
}

get_method2 <- function(generic, x, y, env = caller_env(2)) {
  classes <- sort.int(c(class(x), class(y)), method = "radix")
  c1 <- classes[[1]]
  c2 <- classes[[2]]

  while (!is_empty_env(env)) {
    table <- binary_table(env)

    if (!is_null(table)) {
      fn <- table[[generic]][[c1]][[c2]]

      if (!is_null(fn)) {
        dispatched <- new_list(2L, names = classes)
        if (c1 == class(x)) {
          dispatched[[c1]] <- x
          dispatched[[c2]] <- y
        } else {
          dispatched[[c1]] <- y
          dispatched[[c2]] <- x
        }
        environment(fn) <- env(environment(fn), .dispatched = dispatched)
        return(fn)
      }
    }

    env <- env_parent(env)
  }
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
