#' Ingest all data
#'
#' @description
#' Ingest all Stata data in a directory into a list
#'
#' @param dir Character. Directory containing Stata files.
#' @param hhold_varname Character. Questionnaire variable name for main,
#' household-level data.
#'
#' @return List of data frames.
#' Names correspond to file names without extension.
#' Contents correspond to data in those files.
#'
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom purrr set_names map
#' @importFrom haven read_dta
ingest_dfs <- function(
  dir,
  hhold_varname
) {

  dfs_list <-
    # get the path of all dta files in the target directory
    fs::dir_ls(dir, glob = "*.dta") |>
    # remove `assignment__actions.dta`
    # because it is the only file where `interview__id` isn't a key
    (\(x) {
      grep(
        x = x,
        pattern = "assignment__actions.dta",
        fixed = TRUE,
        invert = TRUE,
        value = TRUE
      )
    })() |>
    # make the file name the name of each path
    purrr::set_names(nm = ~ fs::path_ext_remove(fs::path_file(.x))) |>
    # rename the list element corresponding to the main, household-level data
    purrr::set_names(nm = ~ ifelse(.x == hhold_varname, "households", .x)) |>
    # replace the path with the data
    purrr::map(haven::read_dta)

  return(dfs_list)

}

#' Filter all data frames to the observations of interest
#'
#' @param dfs_list List of data frames.
#' @param interviews Data frame of interviews of interest,
#' consisting of just `interview__id`.
#'
#' @return List of data frames
#'
#' @importFrom purrr map
#' @importFrom dplyr semi_join
filter_dfs <- function(
  dfs_list,
  interviews
) {

  dfs_filtered <- purrr::map(
    .x = dfs_list,
    .f = \(x) {
      dplyr::semi_join(
        x = x,
        y = interviews,
        by = "interview__id"
      )
    }
  )

  return(dfs_filtered)

}

#' Identify interviews that are completed; add metadata for the API
#'
#' @description
#' This function performs two operations:
#'
#' 1. Filters interviews to those complete
#' 2. Adds metadata
#'
#' ## Filters interviews to those complete
#'
#' Interview completion is determined, jointly, by two sources of information:
#'
#' 1. **Interview status.** Survey Solutions' interview status(es)
#' (e.g., `Completed`, `ApprovedBySupervisor`, etc.)
#' 2. **Interview content.** Condition under which an interview is judged
#' completed (e.g., interview result, administration process, etc.).
#'
#' ## Adds metadata
#'
#' There are two pieces of metadata added:
#'
#' 1. **interview_complete.** This marker, which has value of `1` for all
#' interviews in the returned data set, is for downstream processing by other
#' `susoreview` functions.
#' 2. **interview__status.** This is the Survey Solutions status variable
#' that is found in the microdata and needed so that observations
#' can be routed to the correct API endpoint (i.e., rejection by Supervisor,
#' rejection by Headquarters).
#'
#' @param main_df
#' @param statuses
#' @param is_complete_expr
#'
#' @return Data frame with the following columns:
#'
#' - interview__id
#' - interview__key,
#' - interview_complete
#' - interview__status
#'
#' @importFrom dplyr filter mutate select
#' @importFrom cli cli_abort
#' @importFrom rlang enquo
identify_completed <- function(
  main_df,
  statuses,
  is_complete_expr
) {

  # capture the expression as a quosure
  # to delay evaluation
  # to permit inspection
  completed_quo <- rlang::enquo(is_complete_expr)

  # check that all variables in the expression are present in the data
  expr_vars <- completed_quo |>
    # retrieve the expression component of the quosure
    rlang::quo_get_expr() |>
    # return the variable names in the expression as a character vector
    base::all.vars()

  missing_vars <- setdiff(expr_vars, names(main_df))
  if (length(missing_vars) > 0) {
    cli::cli_abort(
      message = c(
        "!" = "Variables in {.arg completed_expr} not found in {.arg main_df}:",
        "*" = "{.var {missing_vars}}"
      )
    )
  }

  df_w_metadata <- main_df |>
    # filter by ...
    # ... Survey Solutions interview status
    dplyr::filter(.data$interview__status %in% .env$statuses) |>
    # ... interview content
    dplyr::filter(!!completed_quo) |>
    # add interview compeltion attributed
    dplyr::mutate(interview_complete = 1L) |>
    # retain attributes needed for downstream operations
    dplyr::select(
      interview__id, interview__key,
      interview_complete, interview__status
    )

  return(df_w_metadata)

}

#' Prepare interview stats for API request
#'
#' @param diagnostics_df Data frame containing the `interview__diagnostics` file
#'
#' @return Data frame with column names renamed to match the API.
#'
#' @importFrom dplyr rename select
prepare_interview_stats <- function(diagnostics_df) {

  # extract number of questions unanswered
  # use `interview__diagnostics` file rather than request stats from API
  interview_stats <- diagnostics_df |>
    # rename to match column names from GET /api/v1/interviews/{id}/stats
    dplyr::rename(
      NotAnswered = n_questions_unanswered,
      WithComments = questions__comments,
      Invalid = entities__errors
    ) |>
    dplyr::select(
      interview__id, interview__key,
      NotAnswered, WithComments, Invalid
    )

  return(interview_stats)

}
