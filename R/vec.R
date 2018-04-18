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
#'
#' # Warnings are consistently thrown when information is lost because
#' # of coercions:
#' vec(foo, "bar")
#'
#' # Incompatible types are promoted to list in order to preserve
#' # information:
#' vec(1L, TRUE, "foo")
#' vec(factor(c("foo", "bar")), 1L)
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

  # Current strategy: first get return type with warnings disabled,
  # then coerce everything to that
  return_type <- reduce_type(xs, env)

  # Expand with `[` method
  ns <- map_int(xs, length)
  out <- return_type[seq_len(sum(ns))]

  end <- 0L
  for (i in seq_len(n)) {
    x <- dispatch2_("vec_coerce", xs[[i]], return_type, .env = env)
    start <- end + 1L
    end <- start + ns[[i]] - 1L
    idx <- seq2(start, end)
    out[idx] <- x
  }

  out
}

reduce_type <- function(xs, env) {
  muffle_vec_coerce(
    reduce(xs, function(x, y) dispatch2_("vec_coerce", x[0], y[0], .env = env))
  )
}
