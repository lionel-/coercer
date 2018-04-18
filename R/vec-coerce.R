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
#' * Should warn about dangerous coercions with the class
#'   `"rlang_vec_coerce_wng"` so that coercion warnings can be
#'   selectively muffled (see [muffle_vec_coerce()]).
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
def_method2("integer", "numeric",
  vec_coerce = function(from, to, ...) {
    vec_coerce_bare(from, "numeric")
  }
)

for (type in c("logical", "integer", "numeric")) {
  def_method2(type, type,
    vec_coerce = function(from, to, ...) {
      from
    }
  )
}


### Character coercions

def_method2("character", "character",
  vec_coerce = function(from, to, ...) {
    from
  }
)

def_method2("character", "factor",
  vec_coerce = function(from, to, ...) {
    chr <- .dispatched$character
    fct <- .dispatched$factor

    if (!length(chr) || !all(chr %in% levels(fct))) {
      msg <- "Coercion of `factor` to `character` loses levels information"
      warn(msg, "rlang_vec_coerce_wng")
      return(as.character(from))
    }

    factor(from, levels(fct))
  }
)

def_method2("factor", "factor",
  vec_coerce = function(from, to, ...) {
    lvls_from <- levels(from)
    lvls_to <- levels(to)

    union <- union(lvls_from, lvls_to)
    lengths <- c(length(lvls_from), length(lvls_to))
    congruent <- length(union) == lengths

    if (all(congruent)) {
      from
    } else if (any(congruent)) {
      msg <- "Factor levels are congruent but not the same length"
      warn(msg, "rlang_vec_coerce_wng")
      lvls <- if (lengths[[1]] > lengths[[2]]) lvls_from else lvls_to
      factor(from, lvls)
    } else {
      msg <- "Coercing `factor` to `character` because of incompatible levels"
      warn(msg, "rlang_vec_coerce_wng")
      as.character(from)
    }
  }
)


### Default coercion to list()

# Requires `[[` and `length()`method
def_method2(whichever(), whichever(),
  vec_coerce = function(from, to, ...) {
    if (!is_vector(from) || !is_vector(to)) {
      abort("Can't coerce a non-vector object to a list")
    }
    msg <- sprintf("Coercing `%s` to `list`", class(from)[[1]])
    warn(msg, "rlang_vec_coerce_wng")

    if (is_bare_vector(from)) {
      return(vec_coerce_bare(from, "list"))
    }

    n <- length(from)
    vec <- new_list(n, names = names(from))
    for (i in seq_len(n)) {
      vec[[i]] <- from[[i]]
    }

    vec
  }
)


#' Muffle vector coercion warnings
#'
#' Evaluate `expr` with all vector coercion warnings turned off. Other
#' warnings are not muffled.
#'
#' @param expr Expression to evaluate.
#' @export
#' @examples
#' # Coercing incompatible factors is lossy because it returns a
#' # character vector that has lost the levels information. Use
#' # muffle_vec_coerce() to silence this kind of warning:
#' f1 <- factor("foo")
#' f2 <- factor("bar")
#' muffle_vec_coerce(vec_coerce(f1, f2))
muffle_vec_coerce <- function(expr) {
  withCallingHandlers(expr,
    rlang_vec_coerce_wng = function(c) invokeRestart("muffleWarning")
  )
}
