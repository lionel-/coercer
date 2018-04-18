#' Vector coercion
#'
#' @description
#'
#' `vec_coerce()` is a [binary generic][dispatch2] meant to power
#' automatic coercions in tidyverse and r-lib packages. Methods should:
#'
#' * Have symmetric coercion behaviour. The resulting type should be
#'   identical on permutation of `from` and `to`.
#'
#' * Not necessarily be fully commutative: `vec_coerce()` is expected
#'   to return a vector as long as the first argument `from`.
#'
#' * Differentiate between empty and non-empty vectors. Most of the
#'   time all the type information should be contained in attributes
#'   and the return type can be determined on empty vectors.
#'
#'   In some cases however the contents of the vector are necessary to
#'   determine the appropriate coercion (e.g. the contents of a
#'   character vector are needed to determine congruence with the
#'   levels of a factor).
#'
#' @param from The vector to coerce. The return value should be a
#'   vector as long as `from`.
#' @param to The vector containing the type information to determine
#'   the coercion of `from`.
#'
#' @include dispatch.R compat-purrr.R
#' @export
vec_coerce <- function(from, to) {
  dispatch2("vec_coerce", from, to)
}

## Export all methods
#' @rawNamespace export(.__rlang_binary_strict_methods__.)
NULL


### Numeric coercions

def_method2("logical", "logical",
  vec_coerce = function(from, to, ...) {
    from
  }
)
def_method2("logical", "integer",
  vec_coerce = function(from, to, ...) {
    vec_coerce_bare(from, "integer")
  }
)
def_method2("logical", "numeric",
  vec_coerce = function(from, to, ...) {
    vec_coerce_bare(from, "numeric")
  }
)

def_method2("integer", "integer",
  vec_coerce = function(from, to, ...) {
    from
  }
)
def_method2("integer", "numeric",
  vec_coerce = function(from, to, ...) {
    vec_coerce_bare(from, "numeric")
  }
)

def_method2("numeric", "numeric",
  vec_coerce = function(from, to, ...) {
    from
  }
)
