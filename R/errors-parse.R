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
##' @param check Logical, indicating if we should check that we can
##'   render everything we produce
##'
##' @return A list, save this within the package
##'
##' @export
errors_parse <- function(path_rmd, pattern, check = TRUE) {
  dat <- errors_read(path_rmd, pattern)
  errors <- Map(error_parse, names(dat), dat)
  if (check) {
    cli::cli_alert_info("Checking errors render")
    for (err in errors) {
      capture.output(suppressMessages(error_render(err, TRUE)))
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

  ret <- Map(function(from, to) trim_blank(txt[from:to]),
             i + 1, c(i[-1] - 1, length(txt)))
  names(ret) <- sub(re, "\\1", txt[i])
  ret
}


error_parse <- function(name, txt) {
  xml <- xml2::read_xml(commonmark::markdown_xml(txt))
  list(code = name,
       plain = txt,
       parsed = lapply(xml2::xml_children(xml), error_parse_node))
}


error_parse_node <- function(x) {
  nm <- xml2::xml_name(x)
  switch(nm,
         paragraph = error_parse_paragraph(x),
         code_block = error_parse_code_block(x),
         list = error_parse_list(x),
         ## Hard, inline:
         link = error_parse_link(x),
         ## Easy, inline:
         code = sprintf("{.code %s}", xml2::xml_text(x)),
         emph = sprintf("{.emph %s}", xml2::xml_text(x)),
         text = xml2::xml_text(x),
         stop(sprintf("Unknown node '%s'", nm)))
}


error_parse_list <- function(x) {
  items <- xml2::xml_children(x)
  stopifnot(all(vapply(items, xml2::xml_name, "") == "item"))
  items <- lapply(items, function(x) error_parse_node(xml2::xml_child(x)))
  list(type = "list",
       mode = xml2::xml_attr(x, "type"),
       items = items)
}


error_parse_paragraph <- function(x) {
  txt <- vapply(xml2::xml_children(x), error_parse_node, "")
  list(type = "paragraph",
       text = paste(txt, collapse = ""))
}


error_parse_code_block <- function(x) {
  list(type = "code_block",
       text = strsplit(sub("\n$", "", xml2::xml_text(x)), "\n")[[1]])
}


error_parse_link <- function(x) {
  target <- xml2::xml_attr(x, "destination")
  if (grepl("^#e[0-9]{4}$", target)) {
    code <- xml2::xml_text(x)
    stopifnot(tolower(code) == sub("^#", "", target))
    sprintf('{.run odin2::odin_error_explain("%s")}', code)
  } else {
    txt <- paste(vapply(xml2::xml_children(x), error_parse_node, ""),
                 collapse = "")
    sprintf("{.href [%s](%s)}", target, txt)
  }
}


trim_blank <- function(x) {
  i <- 1L
  j <- length(x)
  while (x[[i]] == "" && i > j) {
    i <- i + 1L
  }
  while (x[[j]] == "" && j < i) {
    j <- j - 1L
  }
  x[i:j]
}
