# ==============================================================================
# Table of contents
# - all_dta_files: Check if zip contains only .dta files at root
# - all_folders: Check if zip contains only folders or .dta files in folders
# - all_zips: Check if zip contains only .zip files at root
# - classify_zip: Classify zip structure and return type
# ==============================================================================

# ==============================================================================
# Check if zip contains only .dta files at root
# ==============================================================================

#' Check if zip contains only .dta files at root
#'
#' @param zip_path Character. Path to zip file.
#'
#' @return Logical. TRUE if all root entries are .dta files, FALSE otherwise.
#'
#' @importFrom fs path_ext path_dir
#' @importFrom dplyr mutate
all_dta_files <- function(zip_path) {

  zip_meta_df <- zip::zip_list(zip_path) |>
    dplyr::mutate(
      is_dta = fs::path_ext(.data$filename) == "dta",
      in_dir = fs::path_dir(.data$filename) != "."
    )

  are_all_dta <- all(zip_meta_df$is_dta == TRUE)
  are_not_in_dir <- all(zip_meta_df$in_dir == FALSE)

  result <- (are_all_dta == TRUE && are_not_in_dir == TRUE)

  return(result)

}

# ==============================================================================
# Check if zip contains only folders or .dta files in folders
# ==============================================================================

#' Check if zip contains only folders or .dta files in folders
#'
#' Filters out known tag-alongs from Survey Solutions exports:
#' - `export__readme.txt`
#' - `Questionnaire/` directory
#'
#' @param zip_path Character. Path to zip file.
#'
#' @return Logical. TRUE if all filtered entries are dirs or .dta files in
#' directories, FALSE otherwise.
#'
#' @importFrom fs path_ext path_dir
#' @importFrom dplyr filter mutate if_else
all_folders <- function(zip_path) {

  zip_meta_df <- zip::zip_list(zip_path) |>
    # filter out tag-alongs of export
    # - `export__readme.txt` data manifest
    # - `Questionnaire` directory
    dplyr::filter(
      !grepl(
        x = .data$filename,
        pattern = "(export__readme|Questionnaire/)"
      )
    ) |>
    dplyr::mutate(
      is_dir = (.data$type == "directory"),
      is_file = (.data$type == "file"),
      in_dir = fs::path_dir(.data$filename) != ".",
      is_dta = fs::path_ext(.data$filename) == "dta",
      is_dir_or_dta = dplyr::if_else(
        condition = (.data$is_dir == TRUE | (.data$is_dta == TRUE & .data$in_dir == TRUE)),
        true = TRUE,
        false = FALSE
      )
    )

  result <- all(zip_meta_df$is_dir_or_dta == TRUE)

  return(result)

}

# ==============================================================================
# Check if zip contains only .zip files at root
# ==============================================================================

#' Check if zip contains only .zip files at root
#'
#' @param zip_path Character. Path to zip file.
#'
#' @return Logical. TRUE if all root entries are .zip files, FALSE otherwise.
#'
#' @importFrom fs path_ext path_dir
all_zips <- function(zip_path) {

  zip_meta_df <- zip::zip_list(zip_path) |>
    dplyr::mutate(
      is_zip = fs::path_ext(.data$filename) == "zip",
      in_dir = fs::path_dir(.data$filename) != "."
    )

  are_all_zip <- all(zip_meta_df$is_zip == TRUE)
  are_not_in_dir <- all(zip_meta_df$in_dir == FALSE)

  result <- (are_all_zip == TRUE && are_not_in_dir == TRUE)

  return(result)

}

# ==============================================================================
# Classify zip file structure
# ==============================================================================

#' Classify the structure of a zip file
#'
#' Determines whether a zip file contains dta files, folders, or nested zips.
#' Tests in order: dta_files, folders, zips. Returns the first match.
#'
#' @param zip_path Character. Path to zip file.
#'
#' @return Character. One of: "dta_files", "folders", "zips".
#'
#' @importFrom cli cli_abort
classify_zip <- function(zip_path) {

  if (all_dta_files(zip_path)) {
    return("dta_files")
  }

  if (all_folders(zip_path)) {
    return("folders")
  }

  if (all_zips(zip_path)) {
    return("zips")
  }

  # If none of the above, abort with informative message
  cli::cli_abort(
    message = c(
      "x" = "Unable to classify zip structure.",
      "i" = "Zip file must contain one of: .dta files, folders, or nested .zip files.",
      "!" = "Path: {zip_path}"
    )
  )

}
