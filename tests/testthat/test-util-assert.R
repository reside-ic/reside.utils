test_that("assert_is", {
  x <- structure(list(), class = "foo")
  expect_silent(assert_is(x, "foo"))
  expect_error(assert_is(x, "bar"),
               "Expected 'x' to be a 'bar'")
})


test_that("assert_scalar", {
  expect_error(assert_scalar(NULL), "must be a scalar")
  expect_error(assert_scalar(numeric(0)), "must be a scalar")
  expect_error(assert_scalar(1:2), "must be a scalar")
})


test_that("assert_character", {
  expect_silent(assert_character("a"))
  expect_error(assert_character(1), "to be character")
  expect_error(assert_character(TRUE), "to be character")
})


test_that("assert_logical", {
  expect_silent(assert_logical(TRUE))
  expect_error(assert_logical(1), "to be logical")
  expect_error(assert_logical("TRUE"), "to be logical")
})


test_that("assert_nonmissing", {
  expect_silent(assert_nonmissing(TRUE))
  expect_error(assert_nonmissing(NA), "Expected 'NA' to be non-NA")
  x <- c(1, NA)
  expect_error(assert_nonmissing(x), "Expected 'x' to be non-NA")
})


test_that("assert_numeric", {
  expect_silent(assert_numeric(1))
  expect_error(assert_numeric("1"), "to be numeric")
  expect_error(assert_numeric(TRUE), "to be numeric")
})


test_that("assert_integer", {
  expect_silent(assert_integer(1))
  expect_error(assert_integer(1.1), "to be integer")
  expect_error(assert_integer("1"), "to be integer")
  expect_error(assert_integer(TRUE), "to be integer")
})


test_that("assert_scalar_size", {
  expect_silent(assert_scalar_size(10))
  x <- -4
  expect_error(assert_scalar_size(x),
               "'x' must be at least 0")
  expect_error(assert_scalar_size(x, allow_zero = FALSE),
               "'x' must be at least 1")
  x <- 0
  expect_silent(assert_scalar_size(x))
  expect_error(assert_scalar_size(x, allow_zero = FALSE),
               "'x' must be at least 1")
})


test_that("assert_length", {
  x <- 1:5
  expect_silent(assert_length(x, 5))
  expect_error(assert_length(x, 10),
               "Expected 'x' to have length 10, but was length 5")
})


test_that("assert_named", {
  expect_silent(assert_named(structure(1:5, names = letters[1:5])))
  x <- 1:5
  expect_error(assert_named(x), "'x' must be named")
  names(x)[1:2] <- c("a", "b")
  expect_error(assert_named(x), "All elements of 'x' must be named")
  names(x)[3:5] <- ""
  expect_error(assert_named(x), "All elements of 'x' must be named")
  names(x)[3:5] <- c("a", "b", "c")
  expect_error(assert_named(x, unique = TRUE),
               "'x' must have unique names")
  expect_no_error(assert_named(x))
})


test_that("match_value", {
  expect_error(match_value("foo", letters), "must be one of")
  expect_silent(match_value("a", letters))
})


test_that("assert_scalar_numeric", {
  expect_no_error(assert_scalar_numeric(1))
  expect_error(assert_scalar_numeric(1:2), "must be a scalar")
})


test_that("assert_scalar_logical", {
  expect_no_error(assert_scalar_logical(TRUE))
  expect_error(assert_scalar_logical(1), "to be logical")
})
