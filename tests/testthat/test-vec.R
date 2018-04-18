context("vec")

test_that("vec() works in edge and simple cases", {
  expect_null(vec())
  expect_null(vec(NULL))
  expect_identical(vec(1L), 1L)
  expect_identical(vec(NA), NA)
  expect_identical(vec(list()), list())
  expect_identical(vec(chr()), chr())
})

test_that("vec() works with numeric types", {
  expect_identical(vec(TRUE, FALSE), c(TRUE, FALSE))
  expect_identical(vec(TRUE, 2L), 1:2)
  expect_identical(vec(TRUE, 2), c(1, 2))
  expect_identical(vec(TRUE, 2L, 3), c(1, 2, 3))
})

test_that("vec() works with character types", {
  foo <- factor("foo")
  foobar <- factor(c("foo", "bar"))

  expect_identical(vec("foo", "bar"), c("foo", "bar"))

  expect_warning(
    expect_identical(vec("foo", foo), c("foo", "foo")),
    "loses levels"
  )

  expect_identical(vec(foo, foo), foo[c(1, 1)])
  expect_identical(vec(foobar[1], foobar), foobar[c(1, 1, 2)])

  expect_warning(
    expect_identical(vec(foo, foobar), foobar[c(1, 1, 2)]),
    "congruent but not the same length"
  )

  msgs <- catch_warning_msgs(
    expect_identical(vec(foobar, foo, foo), foobar[c(1, 2, 1, 1)])
  )
  expect_length(msgs, 2L)
  for (msg in msgs) {
    expect_match(msg, "congruent but not the same length")
  }

  baz <- factor("baz")
  msgs <- catch_warning_msgs(
    expect_identical(vec(foo, foobar, baz, "bam"), c("foo", "foo", "bar", "baz", "bam"))
  )
  expect_length(msgs, 3L)
  for (msg in msgs) {
    expect_match(msg, "loses levels information")
  }
})

test_that("vec() upcoerces to list", {
  # TODO: Do we want n or 1 warnings here?
  msgs <- catch_warning_msgs(
    expect_identical(vec(1L, 2, "foo"), list(1L, 2, "foo"))
  )
  expect_length(msgs, 3L)
  expect_match(msgs[[1]], "`integer` to `list`")
  expect_match(msgs[[2]], "`numeric` to `list`")
  expect_match(msgs[[3]], "`character` to `list`")
})

test_that("vec() ignores NULL elements", {
  expect_identical(vec(NULL, 1L), 1L)
  expect_identical(vec(1L, NULL), 1L)
  expect_warning(
    expect_identical(vec(NULL, 1L, NULL, list()), list(1L)),
    "`integer` to `list`"
  )
  expect_identical(vec(NULL, 1L, NULL, 2, NULL, FALSE, NULL), c(1, 2, 0))
})
