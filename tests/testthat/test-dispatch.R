context("dispatch")

test_that("def_method2() registers methods in current env", {
  m1 <- function(x, y) 1
  m2 <- function(x, y) 2

  def_method2("c1", "c2", mtd = m1)
  table <- env_get(, binary_table_name)
  expect_is(table, "environment")

  table_mtd <- table$mtd
  expect_is(table_mtd, "environment")
  expect_identical(table_mtd$c1$c2$mtd, m1)

  def_method2("c3", "c2", mtd = m2)
  expect_identical(table_mtd$c2$c3$mtd, m2)
})

test_that("redefining a method warns", {
  m1 <- function(x, y) 1
  m2 <- function(x, y) 2
  def_method2("c1", "c2", mtd = m1)
  def_method2("c1", "c3", mtd = m1)
  expect_warning(def_method2("c1", "c2", mtd = m2), "`c1` and `c2`")
  expect_warning(def_method2("c3", "c1", mtd = m2), "`c1` and `c3`")

  table <- env_get(, binary_table_name)
  expect_identical(table$mtd$c1$c2$mtd, m2)
  expect_identical(table$mtd$c1$c3$mtd, m2)
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

test_that("method arguments are flipped if necessary", {
  fn <- function(x, y) {
    dispatch2("fn", x, y)
  }

  def_method2("factor", "character", fn = function(fct, chr) {
    list(fct = fct, chr = chr)
  })
  exp <- list(fct = factor("foo"), chr = "foo")
  expect_identical(fn("foo", factor("foo")), exp)

  # Should produce warning:
  expect_warning(regex = "Overriding",
    def_method2("character", "factor", fn = function(chr, fct) {
      list(fct = fct, chr = chr)
    })
  )
  expect_identical(fn("foo", factor("foo")), exp)
})
