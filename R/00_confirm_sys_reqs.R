# ------------------------------------------------------------------------------
# confirm the R version
# ------------------------------------------------------------------------------

r_version_required <- base::numeric_version("4.4.1")
r_version_found <- base::getRversion()

if (r_version_found < r_version_required) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("sys_reqs", "r_version", "error", "desc"),
      "i" = glue::glue(get_msg("sys_reqs", "r_version", "error", "details")),
      "!" = glue::glue(get_msg("sys_reqs", "r_version", "error", "action"))
    )
  )

} else {
  cli::cli_inform(
    message = c(
      "v" = get_msg("sys_reqs", "r_version", "success")
    )
  )
}

# ------------------------------------------------------------------------------
# confirmer que RTools est installé
# ------------------------------------------------------------------------------

if (pkgbuild:::is_windows() & !pkgbuild::has_rtools()) {

  url_rtools <- "https://cran.r-project.org/bin/windows/Rtools/"

  cli::cli_abort(
    message = c(
      "x" = get_msg("sys_reqs", "rtools", "error", "desc"),
      "i" = get_msg("sys_reqs", "rtools", "error", "details"),
      "!" = glue::glue(
        get_msg("sys_reqs", "rtools", "error", "action"),
        .open = "<",
        .close = ">",
      )
    )
  )

} else {
  cli::cli_inform(
    message = c(
      "v" = get_msg("sys_reqs", "rtools", "success")
    )
  )
}

# ------------------------------------------------------------------------------
# confirmer que Quarto est installé
# ------------------------------------------------------------------------------

if (is.null(quarto::quarto_path())) {

  url_quarto <- "https://quarto.org/docs/get-started/"

  cli::cli_abort(
    message = c(
      "x" = get_msg("sys_reqs", "quarto", "error", "desc"),
      "i" = get_msg("sys_reqs", "quarto", "error", "details"),
      "!" = glue::glue(
        get_msg("sys_reqs", "quarto", "error", "action"),
        .open = "<",
        .close = ">",
      )
    )
  )

} else {
  cli::cli_inform(
    message = c(
      "v" = get_msg("sys_reqs", "quarto", "success")
    )
  )
}
