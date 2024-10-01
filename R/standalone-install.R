# ---
# repo: reside/reside.utils
# file: standalone-install.R
# imports: [cli, rlang]
# ---
install_needed <- function(package, need, offer_install = interactive(),
                           call = parent.frame()) {
  packages <- install_needs_list(package, need)
  install_missing(packages, offer_install, call)
}


install_needs_list <- function(package, need) {
  desc <- utils::packageDescription(package)
  key <- sprintf("Config/Needs/%s", need)
  strsplit(desc[[key]], ",\\s+")[[1]]
}


install_missing <- function(packages,
                            offer_install = interactive(),
                            call = parent.frame()) {
  err <- !vapply(packages, requireNamespace, TRUE, quietly = TRUE)
  if (!any(err)) {
    return()
  }

  msg <- packages[err]
  message <- "Package{?s} missing for this functionality: {.pkg {msg}}"

  if (offer_install) {
    cli::cli_alert_warning(message)
    ans <- readline("Try installing these packages? [y/N] ")
    if (tolower(ans) == "y") {
      utils::install.packages(msg)
      install_missing(msg, FALSE, call)
    }
  }

  cmd <- sprintf("utils::install.packages(c(%s))",
                 paste(sprintf('"%s"', msg), collapse = ", "))
  n <- length(msg)
  cli::cli_abort(
    c("Package{?s} missing for this functionality: {.pkg {msg}}",
      i = paste("You can install {cli::qty(n)}{?this package/these packages}",
                "using {.run {cmd}}")),
    call = call)
}
