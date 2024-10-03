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
