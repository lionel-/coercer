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
