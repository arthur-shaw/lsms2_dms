# ==============================================================================
# Table of contents
# - Check
# - Download
# - Combine
# ==============================================================================

# ==============================================================================
# Check
# ==============================================================================

#' Check that the survey type is valid for the project
#'
#' @param type Character. Questionnaire type short name. Must be a value
#' defined in `allowed_types`.
#' @param allowed_types Character vector. Values of allowed questionnaire types
#' @param get_msg Function for retrieving the right message
#'
#' @return Character. Same value as `type` if check passes.
#'
#' @importFrom rlang is_string
#' @importFrom cli cli_abort
check_type <- function(
  type,
  allowed_types,
  get_msg,
  call = rlang::caller_env()
) {

  if (!rlang::is_string(type)) {
    cli::cli_abort(
      message = c(
        "x" = get_msg("get_data", "type_is_string")
      ),
      call = call
    )
  }

  if (!type %in% allowed_types) {
    cli::cli_abort(
      message = c(
        "x" = paste0(
          get_msg("get_data", "type_is_allowed"),
          paste(allowed_types, collapse = ", ")
        )
      ),
      call = call
    )
  }

  invisible(type)

}

# ==============================================================================
# Download
# ==============================================================================

#' Get data for the target questionnaire
#' @description Télécharger et décomprimer les données du questionnaire cible
#'
#' @param qnr_expr Character. Regular expression that identifies questionnaires
#' whose data to download. The expression is matched against the questionnaire
#' title.
#' @param allowed_types Character vector. Values of allowed questionnaire types
#' @param type Character. Questionnaire type short name. Must be a value
#' defined in `allowed_types`.
#' @param dirs List. Named list containing project paths as elements.
#' @param server Character. URL of the target SuSo server.
#' @param workspace Character. Name (!= display name) of workspace.
#' @param user Character. Name of the admin or API users.
#' @param password Character. Password of the user above.
#' @param get_msg Function for retrieving the right message
#'
#' @importFrom susoflows delete_in_dir download_matching unzip_to_dir
#' @importFrom cli cli_alert_info
get_data <- function(
  qnr_expr,
  allowed_types = type,
  type,
  dirs,
  server,
  workspace,
  user,
  password,
  get_msg
) {

  # ----------------------------------------------------------------------------
  # Assorted setup
  # ----------------------------------------------------------------------------

  # check the value of the survey type provided
  check_type(type = type, allowed_types = allowed_types)

  # capture directory paths as objects for simplicity of code
  dir_downloaded <- dirs$data[[type]]$downloaded
  dir_combined <- dirs$data[[type]]$combined

  # ----------------------------------------------------------------------------
  # Purge stale data files
  # ----------------------------------------------------------------------------

  cli::cli_alert_info(get_msg("get_data", "deleting"))

  # downloaded
  susoflows::delete_in_dir(dir_downloaded)
  # combiend
  susoflows::delete_in_dir(dir_combined)

  # ----------------------------------------------------------------------------
  # Download data as zip archive(s)
  # ----------------------------------------------------------------------------

  cli::cli_alert_info(get_msg("get_data", "downloading"))

  susoflows::download_matching(
    matches = qnr_expr,
    export_type = "STATA",
    path = dir_downloaded,
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )

  # ----------------------------------------------------------------------------
  # Unzip zip archive(s)
  # ----------------------------------------------------------------------------

  cli::cli_alert_info(get_msg("get_data", "unzipping"))

  susoflows::unzip_to_dir(dir_downloaded)

  # ----------------------------------------------------------------------------
  # Bind together data files
  # ----------------------------------------------------------------------------

  cli::cli_alert_info(get_msg("get_data", "merging"))

  combine_and_save_all(
    dir_downloaded = dir_downloaded,
    dir_combined = dir_combined
  )

}

#' Get team composition
#'
#' First, fetch composition. Then, write to labelled Stata file.
#'
#' @param dir Character. Directory where data should be stored.
#' @inheritParams susoflows::download_matching
#'
#' @importFrom susoapi get_interviewers
#' @importFrom labelled var_label
#' @importFrom haven write_dta
#' @importFrom fs path
#'
#' @noRd
get_team_composition <- function(
  dir,
  server,
  workspace,
  user,
  password
) {

  # construct team composition
  team_composition <- base::suppressMessages(
    susoapi::get_interviewers(
      server = server,
      workspace = workspace,
      user = user,
      password = password
    )
  )

  # label columns for easier comprehension
  labelled::var_label(team_composition) <- list(
    UserId = "Interviewer GUID",
    UserName = "Interviewer user name",
    SupervisorId = "Supervisor user GUID",
    SupervisorName = "Supervisor user name",
    Role = "Role: Interviewer, Supervisor"
  )

  # write to disk
  haven::write_dta(
    data = team_composition,
    path = fs::path(dir, "team_composition.dta")
  )

}


# ==============================================================================
# Combine
# ==============================================================================

#' Inventory Stata data files in target directory
#'
#' @param dir Character. Path to parent directory to scan files in child
#' directories
#'
#' @return Data frame. Columns: `path`, path to the file; `file_name`,
#' file name without path.
#'
#' @importFrom fs dir_ls dir_info
#' @importFrom dplyr mutate select
inventory_files <- function(dir) {

  # obtain list of all directories of unpacked zip files
  sub_dirs <- fs::dir_ls(
    path = dir,
    type = "directory",
    recurse = FALSE
  )

  # compile list of all Stata files in all directories
  if (length(sub_dirs) > 0) {
    files_df <- sub_dirs |>
      purrr::map_dfr(
        .f = ~ fs::dir_info(
          path = .x,
          recurse = FALSE,
          type = "file",
          regexp = "\\.dta$"
        )
      ) |>
      dplyr::mutate(file_name = fs::path_file(.data$path)) |>
      dplyr::select(path, file_name)
  # assign a null value if one found
  } else {
    files_df <- NULL
  }

  return(files_df)

}

#' Combine and save Stata data files with the same name
#'
#' @param file_df Data frame. Return value of `inventory_files()`.
#' @param name Character. Name of the file (with extension) to ingest from
#' all folders where it is found.
#' @param dir Character. Directory where combined data will be saved.
#'
#' @return Side-effect of writing combined files to disk.
#'
#' @importFrom dplyr filter pull
#' @importFrom purrr map_dfr
#' @importFrom haven read_dta
#' @importFrom fs path
combine_and_save <- function(
  file_df,
  name,
  dir
) {

  # file paths
  # so that can locate same-named data files to combine
  file_paths <- file_df |>
    dplyr::filter(.data$file_name == name) |>
    dplyr::pull(.data$path)

  # variable labels
  # so that can assign labels where purrr drops them
  # returns named list of the form needed by `labelled::set_variable_labels()`
  lbls <- file_paths[1] |>
    haven::read_dta(n_max = 0) |>
    labelled::var_label()

  # data frame
  # so that can assign this value to a name
  df <- purrr::map_dfr(
    .x = file_paths,
    .f = ~ haven::read_dta(file = .x)
  )

  # apply variable labels
  df <- df |>
    labelled::set_variable_labels(.labels = lbls)

  # save to destination directory
  haven::write_dta(data = df, path = fs::path(dir, name))

}

#' Combine and save Stata data files, iterating over each file name
#'
#' @param dir_downloaded Character. Directory data are downloaded and unzipped.
#' @param dir_combined Charcter. Directory data are combined.
#'
#' @return Side-effect of writing combined files to disk
#'
#' @importFrom dplyr distinct pull
#' @importFrom purrr walk
combine_and_save_all <- function(
  dir_downloaded,
  dir_combined
) {

  # inventory all Stata data files in sub-directories below `dir`
  files_df <- inventory_files(dir = dir_downloaded)

  # if any files found
  if (!is.null(files_df)) {

    # create a list of unique file names
    # so that can iterate over all files names
    file_names <- files_df |>
      dplyr::distinct(file_name) |>
      dplyr::pull(file_name)

    # combine and save all same-named Stata files
    purrr::walk(
      .x = file_names,
      .f = ~ combine_and_save(
        file_df = files_df,
        name = .x,
        dir = dir_combined
      )
    )

  }

}
