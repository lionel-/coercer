context("vec-coerce")

test_that("numeric types are coerced", {
  expect_identical(vec_coerce(TRUE, FALSE), TRUE)
  expect_identical(vec_coerce(TRUE, 1L), 1L)
  expect_identical(vec_coerce(TRUE, 1), 1)

  expect_identical(vec_coerce(1L, TRUE), 1L)
  expect_identical(vec_coerce(1L, 2L), 1L)
  expect_identical(vec_coerce(1L, 2), 1)

  expect_identical(vec_coerce(1, TRUE), 1)
  expect_identical(vec_coerce(1, 2L), 1)
  expect_identical(vec_coerce(1, 2), 1)
})

test_that("character types are coerced", {
  foo <- factor("foo")
  foobar <- factor(c("foo", "bar"))

  expect_identical(vec_coerce("foo", "bar"), "foo")

  expect_identical(vec_coerce("foo", foo), foo)
  expect_identical(vec_coerce(foo, "foo"), foo)

  expect_identical(vec_coerce("foo", foobar), foobar[1])
  expect_identical(vec_coerce(foobar, "foo"), foobar)

  expect_warning(
    expect_identical(vec_coerce(c("foo", "bar"), foo), c("foo", "bar")),
    "loses levels information"
  )
  expect_warning(
    expect_identical(vec_coerce(foo, c("foo", "bar")), "foo"),
    "loses levels information"
  )

  expect_warning(
    expect_identical(vec_coerce(foo, foobar), foobar[1]),
    "congruent but not the same length"
  )
  expect_warning(
    expect_identical(vec_coerce(foobar, foo), foobar),
    "congruent but not the same length"
  )

  baz <- factor("baz")
  expect_warning(
    expect_identical(vec_coerce(baz, foo), "baz"),
    "incompatible levels"
  )
  expect_warning(
    expect_identical(vec_coerce(foo, baz), "foo"),
    "incompatible levels"
  )
})

test_that("incompatible vectors coerce to list by default", {
  expect_warning(expect_identical(vec_coerce(1:3, list()), as.list(1:3)), "`integer` to `list`")
  expect_warning(expect_identical(vec_coerce(1:3, chr()), as.list(1:3)), "`integer` to `list`")

  foobar <- factor(c("foo", "bar"))
  expect_warning(expect_identical(vec_coerce(foobar, int()), list(foobar[1], foobar[2])), "`factor` to `list`")
})

test_that("can muffle coercion warnings", {
  expect_warning(regexp = NA, muffle_vec_coerce({
    vec_coerce(1L, "foo")
    vec_coerce("foo", factor("bar"))
    vec_coerce(factor("foo"), factor("bar"))
    vec_coerce(factor("foo"), factor(c("foo", "bar")))
  }))

  expect_warning(muffle_vec_coerce(warn("foo")), "foo")
})

test_that("can coerce list to list", {
  expect_identical(vec(list(), list()), list())
  expect_identical(vec(list(1, 2), list(3L)), list(1, 2, 3L))
})

test_that("can coerce NULL to anything", {
  expect_null(vec_coerce(NULL, NULL))
  expect_identical(vec_coerce(NULL, 1L), int())
  expect_identical(vec_coerce(1L, NULL), 1L)
})
