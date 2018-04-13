
dispatch2 <- function(generic, x, y, env = caller_env(2)) {
  input_c1 <- class(x)
  input_c2 <- class(y)
  classes <- sort.int(c(input_c1, input_c2), method = "radix")
  c1 <- classes[[1]]
  c2 <- classes[[2]]

  while (!is_empty_env(env)) {
    table <- binary_table(env)

    if (!is_null(table)) {
      fn <- table[[generic]][[c1]][[c2]][["mtd"]]

      if (!is_null(fn)) {
        flipped_def <- table[[generic]][[c1]][[c2]][["flipped"]]
        flipped_args <- input_c1 != c1
        if (flipped_def + flipped_args == 1L) {
          fmls <- formals(fn)
          stopifnot(length(fmls) >= 2L)
          names(fmls)[1:2] <- names(fmls[2:1])
          formals(fn) <- fmls
        }

        call <- sys.call(sys.parent(1L))
        frame <- sys.frame(sys.parent(2L))
        node_poke_car(call, fn)
        return(eval_bare(call, frame))
      }
    }

    env <- env_parent(env)
  }

  abort(sprintf("Can't find a `%s()` method for `%s` and `%s`", generic, c1, c2))
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
    if (is_null(mtd[[c1]][[c2]])) {
      mtd[[c1]][[c2]] <- list()
    }

    mtd[[c1]][[c2]][["mtd"]] <- methods[[name]]
    mtd[[c1]][[c2]][["flipped"]] <- c1 != .class1
  }

  invisible(methods)
}

binary_table_name <- ".__rlang_binary_strict_methods__."

binary_table <- function(env) {
  env[[binary_table_name]]
}
