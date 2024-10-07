test_that("validate default for installation", {
  skip_if_not_installed("mockery")
  mock_interactive <- mockery::mock(TRUE, FALSE, cycle = TRUE)
  mockery::stub(install_offer_install, "interactive", mock_interactive)

  expect_true(install_offer_install(TRUE))
  expect_false(install_offer_install(FALSE))
  mockery::expect_called(mock_interactive, 0)

  withr::with_envvar(c(NOT_CRAN = NA_character_), {
    expect_false(install_offer_install(NULL))
    expect_false(install_offer_install(NULL))
  })
  mockery::expect_called(mock_interactive, 2)

  withr::with_envvar(c(NOT_CRAN = "TRUE"), {
    expect_true(install_offer_install(NULL))
    expect_false(install_offer_install(NULL))
  })
  mockery::expect_called(mock_interactive, 4)
})


test_that("can read package requirements", {
  desc <- structure(list(Package = "foo",
                         "Config/Needs/this" = "a",
                         "Config/Needs/that" = "a, b, c"),
                    class = "packageDescription")
  mock_pkg_description <- mockery::mock(desc, cycle = TRUE)
  mockery::stub(install_needs_list, "utils::packageDescription",
                mock_pkg_description)
  expect_equal(install_needs_list("foo", "this"), "a")
  expect_equal(install_needs_list("foo", "that"), c("a", "b", "c"))
  expect_equal(install_needs_list("foo", "other"), character())
})


test_that("can install missing packages", {
  mock_list_missing_packages <- mockery::mock(c("a", "c"), character())
  mock_readline <- mockery::mock("y")
  mock_install_packages <- mockery::mock()

  mockery::stub(install_missing, "install_list_missing_packages",
                mock_list_missing_packages)
  mockery::stub(install_missing, "install_packages",
                mock_install_packages)
  mockery::stub(install_missing, "readline", mock_readline)

  res <- evaluate_promise(install_missing(c("a", "b", "c"), TRUE))
  expect_null(res$result)
  expect_length(res$messages, 2)
  expect_match(res$messages[[1]],
               "Packages missing for this functionality: a and c")
  expect_match(res$messages[[2]],
               "Installation successful")

  mockery::expect_called(mock_readline, 1)
  mockery::expect_called(mock_install_packages, 1)
  expect_equal(mockery::mock_args(mock_install_packages)[[1]],
               list(c("a", "c")))
})


test_that("can error if installation refused", {
  mock_list_missing_packages <- mockery::mock(c("a", "c"))
  mock_readline <- mockery::mock("n")
  mock_install_packages <- mockery::mock()

  mockery::stub(install_missing, "install_list_missing_packages",
                mock_list_missing_packages)
  mockery::stub(install_missing, "install_packages",
                mock_install_packages)
  mockery::stub(install_missing, "readline", mock_readline)

  expect_error(
    suppressMessages(install_missing(c("a", "b", "c"), TRUE)),
    "Packages missing for this functionality: a and c")

  mockery::expect_called(mock_readline, 1)
  mockery::expect_called(mock_install_packages, 0)
})


test_that("can error if installation fails", {
  mock_list_missing_packages <- mockery::mock(c("a", "c"))
  mock_readline <- mockery::mock("y")
  mock_install_packages <- mockery::mock("c")

  mockery::stub(install_missing, "install_list_missing_packages",
                mock_list_missing_packages)
  mockery::stub(install_missing, "install_packages",
                mock_install_packages)
  mockery::stub(install_missing, "readline", mock_readline)

  expect_error(
    suppressMessages(install_missing(c("a", "b", "c"), TRUE)),
    "Failed to install package")

  mockery::expect_called(mock_readline, 1)
  mockery::expect_called(mock_install_packages, 1)
  expect_equal(mockery::mock_args(mock_install_packages)[[1]],
               list(c("a", "c")))
})
