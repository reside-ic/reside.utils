test_that("assert_file_exists", {
  tmp <- withr::local_tempdir()
  expect_error(assert_file_exists(file.path(tmp, "a")),
               "File does not exist")
  expect_error(assert_file_exists(file.path(tmp, c("a", "b"), "File")),
               "Files do not exist")
  file.create(file.path(tmp, c("a", "b")))
  expect_no_error(assert_file_exists(file.path(tmp, "a")))
  expect_no_error(assert_file_exists(file.path(tmp, c("a", "b"))))
})


test_that("assert_file_exists_relative works checks if files exist", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "c"))
  expect_error(assert_file_exists_relative("a", tmp, "File"),
               "File does not exist: 'a'")
  expect_error(assert_file_exists_relative(c("a", "b"), tmp, "File"),
               "Files do not exist: 'a', 'b'")
  expect_error(assert_file_exists_relative(c("a", "b", "c", "d"), tmp, "File"),
               "Files do not exist: 'a', 'b', 'd'")
  expect_silent(assert_file_exists_relative("c", tmp, "File"))
})


test_that("assert_file_exists_relative informs about case mismatch", {
  testthat::skip_if_not_installed("mockery")
  mock_file_exists <- mockery::mock(TRUE, cycle = TRUE)
  mockery::stub(assert_file_exists_relative, "file_exists", mock_file_exists)

  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "a"))
  fs::dir_create(file.path(tmp, "b/c"))
  file.create(file.path(tmp, "b/c/d"))

  err <- expect_error(
    assert_file_exists_relative("A", tmp, "File"),
    "File does not exist: 'A'")
  expect_length(err$body, 3)
  expect_equal(names(err$body), c("i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_match(err$body[[2]], "If you don't use the canonical case for a file")
  expect_match(err$body[[3]], "Looked within directory '.+'")

  err <- expect_error(
    assert_file_exists_relative(c("A", "b/C/d"), tmp, "File"),
    "Files do not exist: 'A', 'b/C/d'")
  expect_length(err$body, 4)
  expect_equal(names(err$body), c("i", "i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_equal(err$body[[2]], "For 'b/C/d', did you mean 'b/c/d'?")
  expect_match(err$body[[3]], "If you don't use the canonical case for a file")
  expect_match(err$body[[4]], "Looked within directory '.+'")

  err <- expect_error(
    assert_file_exists_relative(c("A", "b/X/d"), tmp, "File"),
    "Files do not exist: 'A', 'b/X/d'")
  expect_length(err$body, 3)
  expect_equal(names(err$body), c("i", "i", "i"))
  expect_equal(err$body[[1]], "For 'A', did you mean 'a'?")
  expect_match(err$body[[2]], "If you don't use the canonical case for a file")
  expect_match(err$body[[3]], "Looked within directory '.+'")
})


test_that("assert_is_directory", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "foo")
  expect_error(assert_is_directory(path), "Directory does not exist")
  file.create(path)
  expect_error(assert_is_directory(path),
               "Path exists but is not a directory")
  expect_silent(assert_is_directory("."))
})


test_that("assert_relative_path", {
  workdir <- getwd()
  expect_error(assert_relative_path(getwd(), "File", workdir),
               "File must be a relative path",
               fixed = TRUE)
  expect_silent(assert_relative_path("relpath", "File", workdir))
  expect_silent(assert_relative_path("a/b/c", "File", workdir))

  expect_error(
    assert_relative_path("../my/path", "File", workdir),
    "must not contain '..' (parent directory) components",
    fixed = TRUE)
  expect_error(
    assert_relative_path("my/../../path", "File", workdir),
    "must not contain '..' (parent directory) components",
    fixed = TRUE)
})


test_that("can convert files to canonical case", {
  tmp <- withr::local_tempdir()
  p <- file.path(tmp, "a", "b", "c")
  fs::dir_create(dirname(p))
  file.create(p)
  expect_equal(file_canonical_case("a/b/c", tmp), "a/b/c")
  expect_equal(file_canonical_case("a//b//c", tmp), "a/b/c")
  expect_equal(file_canonical_case("a/B/c", tmp), "a/b/c")
  expect_equal(file_canonical_case("A/B/C", tmp), "a/b/c")
  expect_equal(file_canonical_case("A/win~1/C", tmp), NA_character_)
  expect_equal(file_canonical_case(c("a/b/c", "a/b/d"), tmp), c("a/b/c", NA))
})


test_that("can check directories do not exist", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "a")
  expect_no_error(assert_directory_does_not_exist(path))
  dir.create(path)
  expect_error(
    assert_directory_does_not_exist(path),
    "Directory already exists")
})
