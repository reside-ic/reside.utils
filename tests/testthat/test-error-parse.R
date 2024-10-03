test_that("can construct a friendly pattern", {
  expect_equal(pattern_to_hint("E[0-9]{3}"), "Exxx")
  expect_equal(pattern_to_hint("E[0-9]{4}"), "Exxxx")
  expect_equal(pattern_to_hint("E[0-9]{4}[a-z]"), "E[0-9]{4}[a-z]")
})


test_that("can construct a url to an RMD", {
  skip_if_not_installed("mockery")
  mock_as_pkgdown <- mockery::mock(
    list(meta = list(url = "https://example.com"),
         vignettes = data.frame(
           file_in = "vignettes/errors.Rmd",
           file_out = "path/errors.html")))
  mockery::stub(errors_url, "pkgdown::as_pkgdown", mock_as_pkgdown)
  res <- errors_url("vignettes/errors.Rmd")
  expect_equal(res, "https://example.com/path/errors.html")
  mockery::expect_called(mock_as_pkgdown, 1)
  expect_equal(mockery::mock_args(mock_as_pkgdown)[[1]], list("."))
})


test_that("can parse all errors into a structure", {
  skip_if_not_installed("mockery")
  mock_errors_url <- mockery::mock("https://example.com/path/errors.html",
                                   cycle = TRUE)
  mockery::stub(errors_parse, "errors_url", mock_errors_url)
  tmp <- withr::local_tempfile()
  err <- c("# `E0001`",
           "",
           "my error")
  writeLines(err, tmp)
  res <- evaluate_promise(errors_parse(tmp, "E[0-9]{4}", "pkg::explain"))
  expect_equal(res$result$url, "https://example.com/path/errors.html")
  expect_equal(res$result$pattern,
               list(local = "E[0-9]{4}",
                    complete = "^E[0-9]{4}$",
                    hint = "Exxxx"))
  expect_match(res$messages,
               "Checking errors render", all = FALSE)
  expect_named(res$result$errors, "E0001")
  expect_equal(res$result$errors$E0001,
               list(code = "E0001",
                    plain = "my error",
                    parsed = list(list(type = "paragraph",
                                       text = "my error"))))
  expect_match(res$messages,
               "all ok", all = FALSE)
})


test_that("can trim string vectors", {
  expect_equal(trim_blank(c("", "", "x", "", "y")), c("x", "", "y"))
  expect_equal(trim_blank(c("", "", "x", "", "y", "")), c("x", "", "y"))
  expect_equal(trim_blank(c("x", "", "y", "")), c("x", "", "y"))
})


test_that("can parse error into cli", {
  info <- list(cmd_explain = "pkg::explain")
  expect_equal(
    error_parse_md("simple", info),
    list(list(type = "paragraph", text = "simple")))
  expect_equal(
    error_parse_md("*simple* with **inline** markdown `styles`", info),
    list(
      list(
        type = "paragraph",
        text = "{.emph simple} with {.strong inline} markdown {.code styles}")))
  expect_equal(
    error_parse_md(c("a paragraph",
                     "",
                     "1. a list",
                     "1. that is numbered"),
                   info),
    list(
      list(type = "paragraph", text = "a paragraph"),
      list(type = "list",
           mode = "ordered",
           items = list(list(type = "paragraph", text = "a list"),
                        list(type = "paragraph", text = "that is numbered")))))
  expect_equal(
    error_parse_md(c("a paragraph",
                     "",
                     "* a list",
                     "* that is bullets"),
                   info),
    list(
      list(type = "paragraph", text = "a paragraph"),
      list(type = "list",
           mode = "bullet",
           items = list(list(type = "paragraph", text = "a list"),
                        list(type = "paragraph", text = "that is bullets")))))
  expect_equal(
    error_parse_md(c("a paragraph",
                     "",
                     "```r",
                     "f <- function(x) {",
                     "  x",
                     "}",
                     "```"),
                   info),
    list(
      list(type = "paragraph", text = "a paragraph"),
      list(type = "code_block", text = c("f <- function(x) {", "  x", "}"))))

  expect_equal(
    error_parse_md("foo [link](dest) bar", info),
    list(list(type = "paragraph", text = "foo {.href [dest](link)} bar")))
  expect_equal(
    error_parse_md("foo [E101](#e101) bar", info),
    list(list(type = "paragraph",
              text = 'foo {.run pkg::explain("E101")} bar')))
})


test_that("require errors have consistent pattern", {
  tmp <- withr::local_tempfile()
  writeLines(
    c("# `E0001`",
      "",
      "error1",
      "",
      "# `E002`",
      "",
      "error2"),
    tmp)
  expect_no_error(errors_read(tmp, "E[0-9]+"))
  expect_error(
    errors_read(tmp, "E[0-9]{4}"),
    "Some headings in")
})


test_that("error on unknown node", {
  expect_error(error_parse_md("---", NULL),
               "Unknown node in md: 'thematic_break'")
})


test_that("prevent duplicated errors", {
  tmp <- withr::local_tempfile()
  writeLines(
    c("# `E001`",
      "",
      "error1",
      "",
      "# `E001`",
      "",
      "error2"),
    tmp)
  expect_error(
    errors_read(tmp, "E[0-9]{3}"),
    "Some headings in '.+' are duplicated")
})
