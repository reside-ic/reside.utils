test_that("Throws errors on invalid lookup", {
  l <- strict_list(x = 34)
  expect_error(l$y, "'y' is not found in 'list'")
  expect_error(l[["y"]], "'y' is not found in 'list'")
  expect_error(l[c("x", "y", "z")], "'y' and 'z' not found in 'list'")
})

test_that("List description is included in error messages", {
  l <- strict_list(x = 34, .name = "parameters")
  expect_error(l$y, "'y' is not found in 'parameters'")
  expect_error(l[["y"]], "'y' is not found in 'parameters'")
  expect_error(l[c("x", "y", "z")], "'y' and 'z' not found in 'parameters'")
})

test_that("Can add values to an existing strict_list", {
  l <- strict_list(x = 34)
  expect_error(l$y, "'y' is not found in 'list'")
  expect_no_error({
    l$y <- 42
  })
  expect_equal(l$y, 42)
})

test_that("Subsetting a strict_list returns a strict_list", {
  l <- strict_list(x = 34, y = 42, z = 12, .name = "foo")
  sub <- l[c("x", "z")]
  expect_s3_class(sub, "strict_list")
  expect_setequal(names(sub), c("x", "z"))

  expect_error(sub$y, "'y' is not found in 'foo'")
})

test_that("Can index a list by position", {
  l <- strict_list(x = 34, y = 42, z = 12, .name = "foo")

  expect_equal(l[[1]], 34)
  expect_equal(l[1], strict_list(x = 34, .name = "foo"))
  expect_equal(l[c(2, 1)], strict_list(y = 42, x = 34, .name = "foo"))
})

test_that("Can unwrap a strict list", {
  l <- strict_list(x = 34)
  expect_error(l$y, "'y' is not found in 'list'")

  regular <- as.list(l)
  expect_equal(regular, list(x = 34))
  expect_equal(regular$y, NULL)
})
