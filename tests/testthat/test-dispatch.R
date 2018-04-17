context("dispatch")

test_that("def_method2() registers methods in current env", {
  m1 <- function(x, y) 1
  m2 <- function(x, y) 2

  def_method2("c1", "c2", mtd = m1)
  table <- env_get(, binary_table_name)
  expect_is(table, "environment")

  table_mtd <- table$mtd
  expect_is(table_mtd, "environment")
  expect_identical(table_mtd$c1$c2, m1)

  def_method2("c3", "c2", mtd = m2)
  expect_identical(table_mtd$c2$c3, m2)
})

test_that("redefining a method warns", {
  m1 <- function(x, y) 1
  m2 <- function(x, y) 2
  def_method2("c1", "c2", mtd = m1)
  def_method2("c1", "c3", mtd = m1)
  expect_warning(def_method2("c1", "c2", mtd = m2), "`c1` and `c2`")
  expect_warning(def_method2("c3", "c1", mtd = m2), "`c1` and `c3`")

  table <- env_get(, binary_table_name)
  expect_identical(table$mtd$c1$c2, m2)
  expect_identical(table$mtd$c1$c3, m2)
})

test_that("dispatch2() finds methods", {
  def_method2("character", "integer", fn = function(x, y) stop("wrong!"))
  def_method2("numeric", "integer", fn = function(x, y) "dispatched!")

  fn <- function(x, y) dispatch2("fn", x, y)
  expect_identical(fn(1, 2L), "dispatched!")
  expect_identical(local(fn(1L, 2)), "dispatched!")

  env <- env()
  with_env(env, def_method2("character", "integer", fn = function(x, y) "dispatched!"))
  expect_identical(with_env(env, fn("foo", 2L)), "dispatched!")
  expect_error(fn("foo", 2L), "wrong!")
})

test_that("dispatch2() passes all arguments", {
  def_method2("numeric", "numeric", fn = function(x, y) NULL)

  fn <- function(x, y, ..., z) dispatch2("fn", x, y)
  expect_error(fn(1, 2, 3, 4), "unused arguments")

  expect_warning(def_method2("numeric", "numeric", fn = function(x, y, ..., z) list(..., x = x, y = y, z = z)))
  expect_identical(fn(1, 2, 3, z = 4, foo = 5), list(3, foo = 5, x = 1, y = 2, z = 4))
})

test_that("dispatch2() defines `.dispatched` pronoun", {
  env <- local({
    def_method2("character", "integer", fn = function(x, y) {
      chr <- .dispatched$character
      int <- .dispatched$integer
      list(chr = chr, int = int)
    })
    current_env()
  })

  fn <- function(x, y) dispatch2("fn", x, y, env = env)

  expect_identical(fn("foo", 1L), list(chr = "foo", int = 1L))
  expect_identical(fn(1L, "foo"), list(chr = "foo", int = 1L))

  expect_false(env_has(current_env(), ".dispatched"))
})

test_that("caller environment of methods is the caller of the generic", {
  fn <- function(...) dispatch2("fn", ...)
  def_method2("character", "integer", fn = function(...) parent.frame())

  g <- function() list(fn(1L, "foo"), current_env())
  frames <- g()
  expect_identical(frames[[1]], frames[[2]])
})

test_that("can retrieve method with get_method2()", {
  exp <- function(...) "fn"
  def_method2("character", "integer", fn = exp)
  out <- get_method2("fn", 1L, "foo")

  expect_identical(set_env(out), exp)
  expect_identical(get_env(out)$.dispatched, list(character = "foo", integer = 1L))
})

test_that("can retrieve method with get_bare_method2()", {
  exp_mtd <- function(...) "fn"
  def_method2("integer", "character", fn = exp_mtd)
  exp <- list(classes = c("character", "integer"), method = exp_mtd)
  expect_identical(get_method2_info("fn", "integer", "character"), exp)
})

test_that("get_bare_method2() fails when class has length zero", {
  expect_error(get_method2_info("foo", "bar", chr()), "not have length")
  expect_error(get_method2_info("foo", NULL, "bar"), "not have length")
})

test_that("dispatch2() fails if no methods could be found", {
  expect_error(dispatch2("foo", 1, 2, env = current_env()),
    "Can't find a `numeric` and `numeric` method for `foo()`",
    fixed = TRUE
  )
})

test_that("can dispatch on wildcard as last resort", {
  def_method2("*", "*", fn = function(x, y, ...) "dispatched!")
  expect_identical(dispatch2("fn", chr(), int(), current_env()), "dispatched!")
})

test_that("dispatch2_() forwards arguments manually", {
  def_method2("numeric", "integer", fn = function(x, y, ...) list(x, y, ...))
  out <- dispatch2_("fn", 1, 2L, foo = "bar", .env = current_env())
  expect_identical(out, list(1, 2L, foo = "bar"))
})
