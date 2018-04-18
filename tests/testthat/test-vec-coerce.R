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
