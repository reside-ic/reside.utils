test_that("can use utils in standalone", {
  testthat::skip_if_offline()
  path <- withr::local_tempdir()
  writeLines(c(
    "Package: pkg",
    "Version: 1.0.0"),
    file.path(path, "DESCRIPTION"))
  file.create(file.path(path, "NAMESPACE"))
  dir.create(file.path(path, "R"))

  evaluate_promise(
    usethis::with_project(
      path,
      usethis::use_standalone("reside-ic/reside.utils",
                              "standalone-utils-assert.R",
                              ref = "prototype")))

  expect_true(
    file.exists(file.path(path, "R/import-standalone-utils-assert.R")))

  pkg <- pkgload::load_all(path, quiet = TRUE)
  expect_no_error(pkg$env$assert_scalar_logical(TRUE))
})
