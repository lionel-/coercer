#' Binary dispatch
#'
#' @description
#'
#' `dispatch2()` dispatches to binary methods defined with
#' `def_method2()` based on the classes of `x` and `y`.
#'
#' * `dispatch2()` is meant to be called within a generic wrapper and
#'   forwards arguments automatically.
#'
#' * `dispatch2_()` with an underscore requires manual forwarding of
#'   arguments.
#'
#' `dispatch2()` is a binary version of [base::UseMethod()].
#' `def_method2()` has no S3 equivalent but is similar to the S4
#' operator [methods::setMethod()].
#'
#'
#' @section Semantics:
#'
#' * Dispatch is symmetric but not necessarily commutative. The same
#'   method is always called for two given types (symmetric
#'   dispatch). However the return value might be different
#'   (non-commutativity).
#'
#' * Method definition is commutative: `def_method2(c1, c2, ...)` is
#'   equivalent to `def_method2(c2, c1, ...)`. The ordering of the
#'   method arguments is never changed (and they could be of class
#'   `c1` or `c2`) but you can access the arguments through the
#'   `.dispatched` pronoun, e.g. `.dispatched$c1`.
#'
#' * Dispatch is lexically scoped. Methods are only looked up in `env`
#'   and its ancestors. Calling `def_method2()` for a method that
#'   already exists in an environment gives a warning.
#'
#' * Binary dispatch enforces specialisation: no inheritance is
#'   allowed. However you can specify default methods by supplying the
#'   `"*"` wildcard as classes.
#'
#' @param generic,.generic The name of the generic as a string.
#' @param x,y,.x,.y Objects to dispatch on.
#' @param env,.env The environment in which to find or define methods.
#' @param ... For `def_method2()`, named methods to be defined in
#'   `.env`. For `dispatch2_()`, further arguments to be passed on to
#'   the selected method.
#' @param .class1,.class2 Classes to define methods on. Must be length
#'   1 strings.
#'
#' @seealso [get_method2()]
#' @export
#' @examples
#' is_congruent <- function(x, y) {
#'   dispatch2("is_congruent", x, y)
#' }
#'
#' # `x` and `y` are as supplied by the user and so could each be a
#' # factor or a character vector. However we can access these
#' # arguments through the `.dispatched` pronoun:
#' def_method2("factor", "character",
#'   is_congruent = function(x, y, ...) {
#'     fct <- .dispatched$factor
#'     chr <- .dispatched$character
#'     all(chr %in% levels(fct))
#'   }
#' )
#'
#' # You can also supply a method for objects of the same class:
#' def_method2("factor", "factor",
#'   is_congruent = function(x, y, ...) {
#'     all(levels(x) %in% levels(y))
#'   }
#' )
#'
#' f <- factor(c("foo", "bar"))
#' is_congruent(f, "foo")
#' is_congruent(f, "baz")
#'
#' is_congruent(f, factor("bar"))
#' is_congruent(f, factor(c("bar", "foo")))
#'
#'
#' # While there is no inheritance of methods with binary dispatch, you
#' # can define a default method with the whichever() wildcard:
#' def_method2(whichever(), whichever(),
#'   is_congruent = function(x, y, ...) {
#'     message("Don't know how to determine congruence")
#'     FALSE
#'   }
#' )
#' is_congruent("foo", 10)
#'
#'
#' # You can also define several methods at once:
#' equals <- function(x, y) {
#'   dispatch2("equals", x, y)
#' }
#'
#' def_method2("integer", "numeric",
#'   is_congruent = function(x, y, ...) {
#'     rlang::is_integerish(.dispatched$numeric)
#'   },
#'   equals = function(x, y, ...) {
#'     all(x == y)
#'   }
#' )
#'
#' is_congruent(1:3, c(4, 5))
dispatch2 <- function(generic, x, y, env = caller_env(2L)) {
  fn <- get_method2(generic, x, y, env)
  check_dispatch(fn, generic, x, y)

  call <- sys.call(sys.parent(1L))
  node_poke_car(call, fn)
  eval_bare(call, env)
}
#' @rdname dispatch2
#' @export
dispatch2_ <- function(.generic, .x, .y, ..., .env) {
  fn <- get_method2(.generic, .x, .y, .env)
  check_dispatch(fn, .generic, .x, .y)
  fn(.x, .y, ...)
}
check_dispatch <- function(fn, generic, x, y) {
  if (is_null(fn)) {
    c1 <- class(x)[[1]]
    c2 <- class(y)[[1]]
    abort(sprintf("Can't find a `%s` and `%s` method for `%s()`", c1, c2, generic))
  }
}

#' Select a binary method
#'
#' This retrieves the method selected by binary dispatch without
#' calling it. The arguments dispatched on are stored in the
#' `.dispatched` pronoun.
#'
#' @inheritParams dispatch2
#' @export
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

get_method2_info <- function(generic, class1, class2, env = caller_env()) {
  if (!length(class1) || !length(class2)) {
    abort("Object class does not have length")
  }

  classes <- sort.int(c(class1[[1]], class2[[1]]), method = "radix")
  class1 <- classes[[1]]
  class2 <- classes[[2]]

  wildcards <- list()
  was_namespace <- FALSE

  while (!is_empty_env(env)) {
    # Break search at the global env if lookup from a namespace
    if (is_namespace(env)) {
      was_namespace <- TRUE
    } else if (was_namespace && is_reference(env, global_env())) {
      break
    }

    table <- binary_table(env)

    if (!is_null(table)) {
      gen_table <- table[[generic]]
      fn <- gen_table[[class1]][[class2]]

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

  star_c1 <- wildcards[[class1]]
  star_c2 <- wildcards[[class2]]
  if (!is_null(star_c1)) {
    if (!is_null(star_c2)) {
      msg <- "Ambiguous `whichever()` methods for classes `%s` and `%s`"
      abort(sprintf(msg, class1, class2))
    }
    return(new_method_info(classes, star_c1))
  }
  if (!is_null(star_c2)) {
    return(new_method_info(classes, star_c1))
  }

  star_star <- wildcards[["*"]]
  if (!is_null(star_star)) {
    return(new_method_info(classes, star_star))
  }

  NULL
}

new_method_info <- function(classes, method) {
  list(classes = classes, method = method)
}

#' @rdname dispatch2
#' @export
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

#' Wildcard type
#'
#' Use `whichever()` when the type does not matter.
#'
#' @export
#' @examples
#' def_method2(whichever(), whichever(), fn = function(x, y) "foo")
whichever <- function() {
  "*"
}
