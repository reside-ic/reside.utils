##' Parse errors from vignette
##'
##' @title Parse errors from vignette
##'
##' @param path_rmd Path to the .Rmd file containing the error
##'   descriptions
##'
##' @param pattern A regular expression matching each error message,
##'   e.g., `E[0-9]{4}`.  This should not include beginning or end of
##'   string markers.
##'
##' @param cmd_explain The name of the command to explain an error
##'
##' @param check Logical, indicating if we should check that we can
##'   render everything we produce
##'
##' @return A list, save this within the package
##'
##' @export
errors_parse <- function(path_rmd, pattern, cmd_explain, check = TRUE) {
  assert_scalar_character(cmd_explain)
  dat <- errors_read(path_rmd, pattern)
  info <- list(cmd_explain = cmd_explain)
  errors <- Map(error_parse, names(dat), dat, MoreArgs = list(info = info))
  if (check) {
    cli::cli_alert_info("Checking errors render")
    for (err in errors) {
      utils::capture.output(suppressMessages(error_render(err, TRUE)))
    }
    cli::cli_alert_success("...all ok")
  }
  list(
    url = errors_url(path_rmd),
    pattern = list(
      local = pattern,
      complete = sprintf("^%s$", pattern),
      hint = pattern_to_hint(pattern)),
    errors = errors)
}


errors_url <- function(path_rmd) {
  pkg <- pkgdown::as_pkgdown(dirname(dirname(path_rmd)))
  i <- match(basename(path_rmd), basename(pkg$vignettes$file_in))
  file.path(pkg$meta$url, pkg$vignettes$file_out[[i]])
}


pattern_to_hint <- function(pattern) {
  re <- "^(.+)\\[0-9\\]\\{([0-9])\\}$"
  if (grepl(re, pattern)) {
    paste0(sub(re, "\\1", pattern),
           strrep("x", as.integer(sub(re, "\\2", pattern))))
  } else {
    pattern
  }
}


errors_read <- function(path_rmd, pattern) {
  txt <- readLines(path_rmd)
  re <- sprintf("^# `(%s)`$", pattern)
  i <- grep(re, txt)

  if (length(setdiff(grep("^# ", txt), i)) > 0) {
    cli::cli_abort(
      "Some headings in '{path_rmd}' don't match expected pattern")
  }

  nms <- sub(re, "\\1", txt[i])
  if (anyDuplicated(nms)) {
    dups <- unique(nms[duplicated(nms)])
    cli::cli_abort(
      "Some headings in '{path_rmd}' are duplicated: {dups}")
  }

  ret <- Map(function(from, to) trim_blank(txt[from:to]),
             i + 1, c(i[-1] - 1, length(txt)))
  names(ret) <- nms

  ret
}


error_parse <- function(name, txt, info) {
  list(code = name,
       plain = txt,
       parsed = error_parse_md(txt, info))
}


error_parse_md <- function(txt, info) {
  xml <- xml2::read_xml(commonmark::markdown_xml(txt))
  lapply(xml2::xml_children(xml), error_parse_node, info)
}


error_parse_node <- function(x, info) {
  nm <- xml2::xml_name(x)
  switch(nm,
         paragraph = error_parse_paragraph(x, info),
         code_block = error_parse_code_block(x, info),
         list = error_parse_list(x, info),
         ## Hard, inline:
         link = error_parse_link(x, info),
         ## Easy, inline:
         code = sprintf("{.code %s}", xml2::xml_text(x)),
         emph = sprintf("{.emph %s}", xml2::xml_text(x)),
         strong = sprintf("{.strong %s}", xml2::xml_text(x)),
         text = xml2::xml_text(x),
         cli::cli_abort("Unknown node in md: '{nm}'"))
}


error_parse_list <- function(x, info) {
  items <- xml2::xml_children(x)
  stopifnot(all(vapply(items, xml2::xml_name, "") == "item"))
  items <- lapply(items, function(x) error_parse_node(xml2::xml_child(x), info))
  list(type = "list",
       mode = xml2::xml_attr(x, "type"),
       items = items)
}


error_parse_paragraph <- function(x, info) {
  txt <- vapply(xml2::xml_children(x), error_parse_node, "", info)
  list(type = "paragraph",
       text = paste(txt, collapse = ""))
}


error_parse_code_block <- function(x, info) {
  list(type = "code_block",
       text = strsplit(sub("\n$", "", xml2::xml_text(x)), "\n")[[1]])
}


error_parse_link <- function(x, info) {
  target <- xml2::xml_attr(x, "destination")
  if (grepl("^#(.+)$", target)) {
    code <- xml2::xml_text(x)
    sprintf('{.run %s("%s")}', info$cmd_explain, code)
  } else {
    txt <- paste(vapply(xml2::xml_children(x), error_parse_node, "", info),
                 collapse = "")
    sprintf("{.href [%s](%s)}", target, txt)
  }
}


trim_blank <- function(x) {
  i <- 1L
  j <- length(x)
  while (x[[i]] == "" && i < j) {
    i <- i + 1L
  }
  while (x[[j]] == "" && j > i) {
    j <- j - 1L
  }
  x[i:j]
}
