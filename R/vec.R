#' Concatenate objects in a single vector
#'
#' @description
#'
#' `vec()` is like [base::c()] but uses an extensible mechanism for
#' coercions of elements to concatenate.
#'
#' Note that only the binary case is guaranteed to be symmetric. The
#' ordering of elements may have an influence on the return type and
#' `vec()` may be sensitive on permutations of arguments.
#'
#'
#' @section Determination of return type:
#'
#' The coercion mechanism uses the binary generic [vec_coerce()]:
#'
#' * A return type is first decided by reducing `vec_coerce()` over
#'   empty versions of the vectors in `...`.
#'
#' * All elements in `...` are then coerced to that return type.
#'
#' @param ... Objects to concatenate.
#'
#' @export
#' @examples
#' # In simple cases vec() works like c():
#' vec(1, 2L, TRUE)
#' c(1, 2L, TRUE)
#'
#' # However it supports more types and is extensible:
#' foo <- factor("foo")
#' vec(foo, foo)
#' c(foo, foo)
#'
#' # Warnings are consistently thrown when information is lost because
#' # of coercions:
#' vec(foo, "bar")
#' c(foo, "bar")
#'
#' # Or when coercion is dubious:
#' foobar <- factor(c("foo", "bar"))
#' vec(foo, foobar, foo)
#' c(foo, foobar, foo)
#'
#' # NA is always promoted to whichever type
#' vec(foo, NA)
#' c(foo, NA)
#'
#' # Incompatible types are promoted to list in order to preserve
#' # information:
#' vec(1L, TRUE, "foo")
#' c(1L, TRUE, "foo")
#'
#' vec(foobar, 1L)
#' c(foobar, 1L)
vec <- function(...) {
  xs <- list2(...)
  n <- length(xs)

  if (n == 0L) {
    return(NULL)
  }
  if (n == 1L) {
    x <- xs[[1]]
     # FIXME: Need a generic here?
    if (!is_vector(x)) {
      x <- as.vector(x)
    }
    return(x)
  }

  # Dispatch environment
  env <- caller_env()

  # First get return type with warnings disabled, then coerce
  # everything to that.
  sentinel <- env()
  return_type <- reduce_type(xs, env, sentinel)

  # Check if whichever/whichever was invoked and warn only once
  if (is_reference(return_type, sentinel)) {
    warn("Coercing all elements to `list` because of incompatible types")
    return_type <- list()
    quiet <- TRUE
  } else {
    quiet <- FALSE
  }

  # Expand with `[` method
  ns <- map_int(xs, length)
  out <- return_type[seq_len(sum(ns))]

  maybe_muffle_vec_coerce(quiet, {
    end <- 0L
    for (i in seq_len(n)) {
      start <- end + 1L
      end <- start + ns[[i]] - 1L
      idx <- seq2(start, end)
      out[idx] <- dispatch2_("vec_coerce", xs[[i]], return_type, .env = env)
    }
  })

  out
}

reduce_type <- function(xs, env, sentinel) {
  here <- current_env()

  # Override the whichever/whichever method so we warn only once
  env <- env(env)
  def_method2(whichever(), whichever(), .env = env,
    vec_coerce = function(from, to, ...) {
      return_from(here, sentinel)
    }
  )

  muffle_vec_coerce(
    reduce(xs, function(x, y) {
      dispatch2_("vec_coerce", vec_empty(x), vec_empty(y), .env = env)
    })
  )
}
maybe_muffle_vec_coerce <- function(quiet, expr) {
  if (quiet) {
    muffle_vec_coerce(expr)
  } else {
    expr
  }
}

# Relies on `vec[0]` semantics
vec_empty <- function(vec) {
  if (identical(vec, NA)) {
    NULL
  } else {
    vec[0]
  }
}
