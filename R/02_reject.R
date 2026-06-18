# ==============================================================================
# check file of interviews to reject
# ==============================================================================

cli::cli_alert_success(get_msg("reject", "checking_inputs"))

# ------------------------------------------------------------------------------
# collect file names and paths
# ------------------------------------------------------------------------------

reject_file_name <- "to_reject_api.xlsx"
reject_file_path <- fs::path(
  dirs$validation$household$decisions,
  reject_file_name
)

# ------------------------------------------------------------------------------
# confirm that there are files to reject
# ------------------------------------------------------------------------------

# charger les entretiens à rejeter
interviews_to_reject <- readxl::read_xlsx(path = reject_file_path)

if (nrow(interviews_to_reject) == 0) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("reject", "no_interview", "summary"),
      "i" = get_msg("reject", "no_interview", "details")
    )
  )

}

# ------------------------------------------------------------------------------
# confirmer les colonnes du fichier
# ------------------------------------------------------------------------------

reject_file_columns_found <- names(interviews_to_reject)

reject_file_columns_expected <- c(
  "interview__id",
  "reject_comment",
  "interview__status"
)

# all expected columns are there
if (any(!reject_file_columns_expected %in% reject_file_columns_found)) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("reject", "wrong_columns", "summary"),
      "i" = get_msg("reject", "wrong_columns", "expected"),
      "i" = get_msg("reject", "wrong_columns", "found")
    )
  )

}

# check that the columns are in the expected order
# otherwise, the `pwalk()` function won't work
# since it relies on the column indices for arguments
if (!identical(reject_file_columns_expected, reject_file_columns_found)) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("reject", "column_order", "summary"),
      "i" = get_msg("reject", "column_order", "expected"),
      "i" = get_msg("reject", "column_order", "found")
    )
  )

}

# ------------------------------------------------------------------------------
# check column content
# ------------------------------------------------------------------------------

# data types of the columns
interviews_to_reject <- readxl::read_xlsx(
  path = reject_file_path,
  col_types = c(
    "text", # interview__id
    "text", # reject_comment
    "numeric" # interview__status
  )
)

# interview__id
if(!all(susoapi:::is_guid(interviews_to_reject$interview__id))) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("reject", "id_is_guid", "summary"),
      "i" = get_msg("reject", "id_is_guid", "details")
    )
  )

}

# interview__status

# list the valid status code values supported by {susoreview}
valid_reject_status_codes <- c(
  100, # Completed
  120, # ApprovedBySupervisor
  130 # ApprovedByHeadquarters
)

if (
  any(!interviews_to_reject$interview__status %in% valid_reject_status_codes)
) {

  cli::cli_abort(
    message = c(
      "x" = get_msg("reject", "status_codes", "summary"),
      "i" = get_msg("reject", "status_codes", "details")
    )
  )

}

# ==============================================================================
# reject interviews on the server
# ==============================================================================

cli::cli_alert_success(get_msg("reject", "rejecting_inteviews"))

# reject cases in the file
purrr::pwalk(
  .l = interviews_to_reject,
  .f = ~ susoreview::reject_interview(
    interview__id = ..1,
    interview__status = ..3,
    reject_comment = ..2,
    statuses_to_reject = suso_statuses_to_reject,
    server = server,
    workspace = workspace,
    user = user,
    password = password
  )
)
