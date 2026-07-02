# ==============================================================================
# Workflow Selector — R data quality pipelines
# ==============================================================================

# ==============================================================================
# Helper Functions
# ==============================================================================

#' Check if rejection file exists
#'
#' The single predicate that gates all rejection logic, used in both the
#' standalone reject workflow and the post-validate chain.
#'
#' @param dirs Named list of paths.
#'
#' @return Logical. TRUE if the rejection file exists in rejection_dir.
#'
rejection_file_exists <- function(dirs) {
  fs::file_exists(dirs$files$rejection)
}

#' Get the most recent file modification time for data directory
#'
#' Returns the most recent file modification time across all files in dir,
#' or NULL if the directory is empty or does not exist.
#'
#' @param dirs Named list of project paths.
#'
#' @return POSIXct timestamp or NULL if no files found.
#'
get_data_timestamp <- function(dirs) {

  dir <- dirs$data$household$combined

  if (!fs::dir_exists(dir)) {
    return(NULL)
  }

  files <- list.files(dir, full.names = TRUE, recursive = TRUE)
  if (length(files) == 0L) {
    return(NULL)
  }

  mtimes <- file.info(files)$mtime
  max(mtimes, na.rm = TRUE)

}

#' Format timestamp into a translated status message
#'
#' Returns a fully rendered string (after glue interpolation), ready to pass
#' directly to cli::cli_inform().
#'
#' @param mtime POSIXct. Modification time to format.
#'
#' @return Rendered character string.
#'
#' @importFrom glue glue
format_timestamp <- function(mtime) {

  if (as.Date(mtime) == Sys.Date()) {
    time <- format(mtime, "%H:%M")
    tmpl <- get_msg("selector", "data_last_fetched_today")
    glue::glue(tmpl)
  } else {
    datetime <- format(mtime, "%Y-%m-%d %H:%M")
    tmpl <- get_msg("selector", "data_last_fetched_prior")
    glue::glue(tmpl)
  }
}

#' Data source sub-menu
#'
#' Reusable sub-menu used by Validate and Monitor workflows. Accepts a
#' translated workflow_label string for display. Returns one of "existing",
#' "fetch", or NULL (cancelled).
#'
#' @param dirs Named list of project paths.
#' @param workflow_label Character. Translated label for the workflow.
#'
#' @return Character "existing" or "fetch", or NULL if cancelled.
#'
#' @importFrom cli cli_rule cli_warn
#' @importFrom glue glue
data_source_menu <- function(
  dirs,
  workflow_label
) {

  mtime <- get_data_timestamp(dirs)

  cli::cli_rule(left = get_msg("selector", "header_data_source"))

  if (is.null(mtime)) {
    # No data on disk — present 2-choice menu (fetch / emailed)
    workflow <- workflow_label
    tmpl <- get_msg("selector", "prompt_data_source")
    prompt <- glue::glue(tmpl)

    choice <- utils::menu(
      choices = c(
        get_msg("selector", "choice_fetch_new"),
        get_msg("selector", "choice_use_emailed")
      ),
      title = prompt
    )

    switch(choice,
      `1` = "fetch",
      `2` = "emailed",
      {
        cli::cli_inform(get_msg("selector", "cancelled"))
        invisible(NULL)
      }
    )
  } else {
    # Data exists — show timestamp and present 3-choice menu
    cli::cli_inform(format_timestamp(mtime))

    workflow <- workflow_label
    tmpl <- get_msg("selector", "prompt_data_source")
    prompt <- glue::glue(tmpl)

    choice <- utils::menu(
      choices = c(
        get_msg("selector", "choice_use_existing"),
        get_msg("selector", "choice_fetch_new"),
        get_msg("selector", "choice_use_emailed")
      ),
      title = prompt
    )

    switch(choice,
      `1` = "existing",
      `2` = "fetch",
      `3` = "emailed",
      {
        cli::cli_inform(get_msg("selector", "cancelled"))
        invisible(NULL)
      }
    )
  }
}

# ==============================================================================
# Workflow Handlers
# ==============================================================================

#' Workflow A — Get data
#'
#' No sub-menu. Print confirmation and source the script immediately.
#'
#' @param dirs Named list of paths to workflow scripts
#'
#' @importFrom cli cli_alert_success
wf_get_data <- function(dirs) {
  workflow <- get_msg("selector", "choice_get_data")
  detail   <- get_msg("selector", "detail_no_data_needed")
  tmpl     <- get_msg("selector", "starting_workflow")
  cli::cli_alert_success(glue::glue(tmpl))
  source(dirs$scripts$get_data)
  invisible(NULL)
}

#' Workflow B — Validate interviews
#'
#' Step B1: Data source sub-menu. If fetch selected, sources script_get first.
#' Step B2: Post-validate rejection chain if rejection file exists.
#'
#' @param dirs Named list of paths to workflow scripts
#'
#' @importFrom cli cli_alert_success cli_rule cli_inform
#' @importFrom glue glue
wf_validate <- function(dirs) {

  # check questionnaire details
  # many of which are needed for validations

  check_qnr_details_provided(params = params)

  check_qnr_var_for_extension(
    params = params,
    qnr_var = "household_qnr_var"
  )

  check_qnr_var_is_dset(
    combined_dir = dirs$data$household$combined,
    params = params,
    qnr_var = "household_qnr_var"
  )

  check_var_in_dset(
    combined_dir = dirs$data$household$combined,
    params = params,
    qnr_var = household_qnr_var,
    param_var_name = "admin1_var"
  )

  # Step B1 — Data source sub-menu
  workflow_label <- get_msg("selector", "choice_validate")
  data_choice <- data_source_menu(
    dirs = dirs,
    workflow_label = workflow_label
  )

  if (is.null(data_choice)) {
    return(invisible(NULL))
  }

  # Determine detail string for confirmation message
  if (data_choice == "fetch") {
    workflow <- get_msg("selector", "choice_get_data")
    detail   <- get_msg("selector", "detail_fetching")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    source(dirs$scripts$get_data)

    # Now run validate
    workflow <- get_msg("selector", "choice_validate")
    detail   <- get_msg("selector", "detail_fetching")
    # Note: We already printed the confirmation above, so we just source
    source(dirs$scripts$validate)
  } else if (data_choice == "emailed") {
    workflow <- get_msg("selector", "choice_get_data")
    detail   <- get_msg("selector", "detail_emailed")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    # Process household emailed data
    get_emailed_data("household", dirs)
    # Process community emailed data if present
    get_emailed_data("community", dirs)

    # Now run validate
    workflow <- get_msg("selector", "choice_validate")
    detail   <- get_msg("selector", "detail_emailed")
    # Note: We already printed the confirmation above, so we just source
    source(dirs$scripts$validate)
  } else {
    # data_choice == "existing"
    mtime    <- get_data_timestamp(dir = dirs)
    timestamp <- format(mtime, "%Y-%m-%d %H:%M")
    detail   <- get_msg("selector", "detail_existing")
    detail   <- glue::glue(detail, timestamp = timestamp)
    workflow <- get_msg("selector", "choice_validate")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    source(dirs$scripts$validate)
  }

  # Step B2 — Post-validate rejection chain
  if (rejection_file_exists(dirs = dirs)) {
    cli::cli_rule(left = get_msg("selector", "header_post_validate"))
    cli::cli_inform(get_msg("selector", "prompt_post_validate"))

    choice <- utils::menu(
      choices = c(
        get_msg("selector", "choice_reject_now"),
        get_msg("selector", "choice_skip_rejection")
      ),
      title = ""
    )

    if (choice == 1) {
      workflow <- get_msg("selector", "choice_reject")
      detail   <- get_msg("selector", "detail_no_data_needed")
      tmpl     <- get_msg("selector", "starting_workflow")
      cli::cli_alert_success(glue::glue(tmpl))
      source(dirs$scripts$reject)
    }
  } else {
    cli::cli_rule(left = get_msg("selector", "header_post_validate"))
    cli::cli_inform(get_msg("selector", "prompt_post_validate_none"))
  }

  invisible(NULL)
}

#' Workflow C — Reject flagged interviews
#'
#' No data source sub-menu. Checks for rejection file and sources script_reject
#' if it exists.
#'
#' @param dirs Named list of project paths
#'
#' @importFrom cli cli_warn cli_alert_success
wf_reject <- function(dirs) {
  if (!rejection_file_exists(dirs = dirs)) {
    dir  <- dirs$validation$household$decisions
    tmpl <- get_msg("selector", "reject_none_found")
    cli::cli_warn(glue::glue(tmpl))
    return(invisible(NULL))
  }

  workflow <- get_msg("selector", "choice_reject")
  detail   <- get_msg("selector", "detail_no_data_needed")
  tmpl     <- get_msg("selector", "starting_workflow")
  cli::cli_alert_success(glue::glue(tmpl))
  source(dirs$scripts$reject)
  invisible(NULL)
}

#' Workflow D — Create monitoring report
#'
#' Identical in structure to Workflow B Step B1. No post-workflow chain.
#'
#' @param dirs Named list of project paths
wf_monitor <- function(dirs) {
  # Data source sub-menu
  workflow_label <- get_msg("selector", "choice_monitor")
  data_choice <- data_source_menu(
    dirs = dirs,
    workflow_label = workflow_label
  )

  if (is.null(data_choice)) {
    return(invisible(NULL))
  }

  # Determine detail string for confirmation message
  if (data_choice == "fetch") {
    workflow <- get_msg("selector", "choice_get_data")
    detail   <- get_msg("selector", "detail_fetching")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    source(dirs$scripts$get_data)

    # Now run monitor
    workflow <- get_msg("selector", "choice_monitor")
    detail   <- get_msg("selector", "detail_fetching")
    # Note: We already printed the confirmation above, so we just source
    source(dirs$scripts$monitor)
  } else if (data_choice == "emailed") {
    workflow <- get_msg("selector", "choice_get_data")
    detail   <- get_msg("selector", "detail_emailed")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    # Process household emailed data
    get_emailed_data("household", dirs)
    # Process community emailed data if present
    get_emailed_data("community", dirs)

    # Now run monitor
    workflow <- get_msg("selector", "choice_monitor")
    detail   <- get_msg("selector", "detail_emailed")
    # Note: We already printed the confirmation above, so we just source
    source(dirs$scripts$monitor)
  } else {
    # data_choice == "existing"
    mtime    <- get_data_timestamp(dirs = dirs)
    timestamp <- format(mtime, "%Y-%m-%d %H:%M")
    detail   <- get_msg("selector", "detail_existing")
    detail   <- glue::glue(detail, timestamp = timestamp)
    workflow <- get_msg("selector", "choice_monitor")
    tmpl     <- get_msg("selector", "starting_workflow")
    cli::cli_alert_success(glue::glue(tmpl))
    source(dirs$scripts$monitor)
  }

  invisible(NULL)
}

# ==============================================================================
# Main Entry Point
# ==============================================================================

#' Run the workflow selector
#'
#' Shows the primary menu and dispatches to the appropriate workflow handler.
#'
run_workflow <- function() {
  cli::cli_rule(left = get_msg("selector", "header_main"))

  choice <- utils::menu(
    choices = c(
      get_msg("selector", "choice_get_data"),
      get_msg("selector", "choice_validate"),
      get_msg("selector", "choice_reject"),
      get_msg("selector", "choice_monitor")
    ),
    title = get_msg("selector", "prompt_main")
  )

  switch(choice,
    `1` = wf_get_data(dirs = dirs),
    `2` = wf_validate(dirs = dirs),
    `3` = wf_reject(dirs = dirs),
    `4` = wf_monitor(dirs = dirs),
    {
      cli::cli_inform(get_msg("selector", "cancelled"))
      invisible(NULL)
    }
  )
}
