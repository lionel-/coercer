
[![Coverage status](https://codecov.io/gh/lionel-/coercer/branch/master/graph/badge.svg)](https://codecov.io/github/lionel-/coercer?branch=master)


## Overview

This package provides an experimental scaffholding and interface for
principled automatic coercions in tidyverse and r-lib packages. These
coercions will be particularly helpful for combining results together
in split-apply-combine settings (dplyr operations on grouped tibbles,
`purrr::pmap()`, ...).

We first focus on features and extensibility but speed will also be
addressed eventually with fast concatenation for base types.

Install the development version from github with:

```r
# install.packages("devtools")
devtools::install_github("lionel-/coercer")
```


## vec()

`vec()` is a replacement for `base::c()` that uses a consistent and
extensible mechanism for automatic coercions.

In simple cases vec() works like c():

```
vec(1, 2L, TRUE)
#> [1] 1 2 1

c(1, 2L, TRUE)
#> [1] 1 2 1
```

However it supports more types and is extensible via a binary dispatch
mechanism. Factors are well supported by default:

```
foo <- factor("foo")

vec(foo, foo)
#> [1] foo foo
#> Levels: foo

c(foo, foo)
#> [1] 1 1
```

Warnings are consistently thrown when information is lost because of
coercions:

```
vec(foo, "bar")
#> [1] "foo" "bar"
#> Warning message:
#> Coercion of `factor` to `character` loses levels information

c(foo, "bar")
#> [1] "1"   "bar"
```

Or when coercion is dubious:

```
# A new factor whose levels are a superset of the levels of `foo`
foobar <- factor(c("foo", "bar"))

vec(foo, foobar, foo)
#> [1] foo foo bar foo
#> Levels: bar foo
#> Warning messages:
#> 1: Factor levels are congruent but not the same length
#> 2: Factor levels are congruent but not the same length

c(foo, foobar, foo)
#> [1] 1 2 1 1
```

`NA` is always promoted to whichever type

```
vec(foo, NA)
#> [1] foo  <NA>
#> Levels: foo

c(foo, NA)
#> [1]  1 NA
```

Incompatible types are promoted to list in order to preserve
information:

```
vec(1L, TRUE, "foo")
#> [[1]]
#> [1] 1
#>
#> [[2]]
#> [1] TRUE
#>
#> [[3]]
#> [1] "foo"
#>
#> Warning message:
#> Coercing all elements to `list` because of incompatible types

c(1L, TRUE, "foo")
#> [1] "1"    "TRUE" "foo"

vec(foobar, 1L)
#> [[1]]
#> [1] foo
#> Levels: bar foo
#>
#> [[2]]
#> [1] bar
#> Levels: bar foo
#>
#> [[3]]
#> [1] 1
#>
#> Warning message:
#> Coercing all elements to `list` because of incompatible types

c(foobar, 1L)
#> [1] 2 1 1
```


## Binary dispatch

`def_method2()` and `dispatch2()` provide a binary dispatch mechanism
that enforces symmetry (coercion behaviour should not change on
permutation of arguments) and specialisation (no inheritance). See
`?dispatch2` for an overview.
