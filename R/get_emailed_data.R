# ==============================================================================
# Table of contents
# - get_emailed_data: Process emailed zip files and prepare data for combine
# ==============================================================================

#' Process emailed zip data
#'
#' @description
#' Handles zip files placed in per-questionnaire inbox folders. Automatically
#' detects zip structure, normalizes contents into `01_downloaded/`, then runs
#' the existing `combine_and_save_all()` pipeline.
#'
#' @param type Character. Questionnaire type short name ("household" or "community").
#' @param dirs List. Named list of project paths from `construct_paths()`.
#'
#' @return Side-effect of extracting and combining data. Returns invisibly NULL.
#'
#' @details
#' **Zip structure handling:**
#' - **dta_files**: All .dta files at root level. Extracted into
#'   `01_downloaded/{zip_name}/`
#' - **folders**: Survey Solutions export structure with subfolders. Extracted
#'   directly into `01_downloaded/`
#' - **zips**: Nested zip files at root. Parent extracted to `01_downloaded/`,
#'   child zips extracted to `01_downloaded/{child_zip_name}/`, then child
#'   zips deleted.
#'
#' **Inbox handling:**
#' - If inbox is empty: informatively skips and returns invisibly
#' - If multiple zips found: aborts with informative error
#' - Original zip file is left in inbox (not deleted)
#'
#' @importFrom fs dir_ls path path_ext path_file dir_exists
#' @importFrom cli cli_inform cli_alert_info cli_abort
#' @importFrom susoflows delete_in_dir
#' @importFrom zip unzip
get_emailed_data <- function(type, dirs) {

  # ============================================================================
  # Setup: resolve paths
  # ============================================================================

  inbox <- dirs$data[[type]]$inbox
  dir_downloaded <- dirs$data[[type]]$downloaded
  dir_combined <- dirs$data[[type]]$combined

  # ============================================================================
  # Check for zip files in inbox
  # ============================================================================

  zip_paths <- fs::dir_ls(inbox, glob = "*.zip")

  # If no zips found, skip silently
  if (length(zip_paths) == 0) {
    tmpl <- get_msg("get_data", "emailed_inbox_empty")
    cli::cli_inform(glue::glue(tmpl, type = type))
    return(invisible(NULL))
  }

  # If multiple zips found, abort
  if (length(zip_paths) > 1) {
    cli::cli_abort(
      message = c(
        "x" = "Ambiguous inbox: found {length(zip_paths)} zip files.",
        "i" = "Please leave only one zip file in {inbox}.",
        "!" = "Found: {paste(fs::path_file(zip_paths), collapse = ', ')}"
      )
    )
  }

  # Single zip file found
  zip_path <- zip_paths[1]

  # ============================================================================
  # Purge stale data directories
  # ============================================================================

  cli::cli_alert_info(get_msg("get_data", "emailed_deleting"))
  susoflows::delete_in_dir(dir_downloaded)
  susoflows::delete_in_dir(dir_combined)

  # ============================================================================
  # Classify zip structure
  # ============================================================================

  cli::cli_alert_info(get_msg("get_data", "emailed_classifying"))
  structure <- classify_zip(zip_path)

  # ============================================================================
  # Extract and normalize based on zip structure
  # ============================================================================

  cli::cli_alert_info(glue::glue(get_msg("get_data", "emailed_extracting"), structure = structure))

  if (structure == "dta_files") {
    # Extract dta files into a named subfolder
    zip_name_no_ext <- fs::path_ext_remove(fs::path_file(zip_path))
    extract_dir <- fs::path(dir_downloaded, zip_name_no_ext)
    zip::unzip(zip_path, exdir = extract_dir)

  } else if (structure == "folders") {
    # Extract folders directly to downloaded directory
    zip::unzip(zip_path, exdir = dir_downloaded)

  } else if (structure == "zips") {
    # Extract parent zip to dir_downloaded
    zip::unzip(zip_path, exdir = dir_downloaded)

    # For each child zip, create subfolder and extract it
    child_zips <- fs::dir_ls(dir_downloaded, glob = "*.zip")

    for (child_zip in child_zips) {
      child_zip_name_no_ext <- fs::path_ext_remove(fs::path_file(child_zip))
      child_extract_dir <- fs::path(dir_downloaded, child_zip_name_no_ext)
      fs::dir_create(child_extract_dir)
      zip::unzip(child_zip, exdir = child_extract_dir)
      # Delete the child zip file
      fs::file_delete(child_zip)
    }

  }

  # ============================================================================
  # Combine and save data
  # ============================================================================

  cli::cli_alert_info(get_msg("get_data", "merging"))

  combine_and_save_all(
    dir_downloaded = dir_downloaded,
    dir_combined = dir_combined
  )

  invisible(NULL)

}
