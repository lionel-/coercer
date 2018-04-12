
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
