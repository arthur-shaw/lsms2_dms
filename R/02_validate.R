# ==============================================================================
# purge stale outputs
# ==============================================================================

output_paths <-
  # take list of all directories under this node
  # descendends of `household`
  # descendents of `community`
  dirs$validation |>
  # convert from list to vector
  unlist() |>
  # remove names
	unname() |>
  # bind together in a single vector
	c()

purrr::walk(
  .x = output_paths,
  .f = ~ susoflows::delete_in_dir(.x)
)

# ==============================================================================
# ingest data
# ==============================================================================

combined_data_dir <- dirs$data$household$combined

dfs_full <- ingest_dfs(
  dir = combined_data_dir,
  hhold_varname = household_qnr_var,
  members_roster_var = members_roster_var
)

# ==============================================================================
# identify updated interviews
# ==============================================================================

# create hashes for new data
hashes_new <- create_interview_hash(
  actions_path = fs::path(combined_data_dir, "interview__actions.dta"),
  actions = 1
)

# load hashes for old data
hashes_old <- load_interview_tracker(dirs$data$household$tracked)

# compare hashes to identify new/updated cases
updated_interviews <- get_updated_interviews(
  old_hash_df = hashes_old,
  new_hash_df = hashes_new
)

# stop program if there are no updated interviews
if (nrow(updated_interviews) == 0) {

  cli::cli_abort(
    message = c(
      "i" = "No new or updated interviews to process",
      "Consider downloading current data."
    )
  )

}

# ==============================================================================
# filter to udpated interviews
# ==============================================================================

dfs_filtered <- filter_dfs(
  dfs_list = dfs_full,
  interviews = updated_interviews
)

# ==============================================================================
# identify interviews of interest
# - by SuSo status
# - by interview completion
# ==============================================================================

completed_interviews <- identify_completed(
  main_df = dfs_filtered[["households"]],
  statuses = suso_statuses_to_reject,
  is_complete_expr = interview_result == 1
)

# stop program if there are no completed interviews
if (nrow(completed_interviews) == 0) {

  cli::cli_abort(
    message = c(
      "i" = "No completed to process",
      "Consider downloading current data."
    )
  )

}

# ==============================================================================
# filter to interviews of interest
# ==============================================================================

dfs_filtered <- filter_dfs(
  dfs_list = dfs_filtered,
  interviews = completed_interviews
)

# ==============================================================================
# perform high-frequency checks
# ==============================================================================

attribs <- create_attributes(dfs_filtered = dfs_filtered)

issues <- create_issues(
  df_attribs = attribs,
  dfs_full = dfs_full,
  dfs_filtered = dfs_filtered,
  admin1_var = admin1_var,
  urb_rur_var = urb_rur_var,
  get_msg = get_msg
)

issues_w_unanswered <- add_issue_for_unanswered_q(
  dfs_filtered = dfs_filtered,
  interviews = completed_interviews,
  issues = issues
)

# ==============================================================================
# make decisions
# ==============================================================================

decisions <- create_decisions(
  dfs_filtered = dfs_filtered,
  interviews = completed_interviews,
  issues = issues_w_unanswered,
  issue_codes_to_reject = issue_codes_to_reject
)

# ===========================================================================
# write recommendations to disk
# ===========================================================================

# intermediate data
write_df_to_disk(
  df = updated_interviews,
  df_name = "interviews_validated",
  dir = dirs$validation$household$recommendations
)
write_df_to_disk(
  df = attribs,
  dir = dirs$validation$household$recommendations
)
write_df_to_disk(
  df = issues_w_unanswered,
  df_name = "issues",
  dir = dirs$validation$household$recommendations
)

# recommendation files
write_df_list_to_disk(
  df_list = decisions,
  dir = dirs$validation$household$recommendations
)

# ===========================================================================
# copy rejection recommendations to decisions
# ===========================================================================

fs::file_copy(
  path = fs::path(
    dirs$validation$household$recommendations,
    "to_reject_api.xlsx"
  ),
  new_path = fs::path(
    dirs$validation$household$decisions,
    "to_reject_api.xlsx"
  ),
  overwrite = TRUE
)

# ===========================================================================
# render reports
# ===========================================================================

# ---------------------------------------------------------------------------
# HQ
# ---------------------------------------------------------------------------

# signal that report rendering process is underway
hq_report_msg <- get_msg("validate", "hq_report")
cli::cli_alert_success(hq_report_msg)

# determine report start/end dates
report <- infer_report_period(start = report_start, report_end)

# collect parameters for rendering
report_params <- list(
  dir_proj = dirs$proj,
  household_qnr_var = household_qnr_var,
  community_qnr_var = community_qnr_var,
  report_start = report$start,
  report_end = report$end
)

# construct paths
hq_report_template_path <- fs::path(dirs$proj, "inst", "hq_report.qmd")
hq_report_temp_path <- fs::path(dirs$proj, "inst", "hq_report.html")
hq_report_final_path <- fs::path(
  dirs$validation$household$hq_report,
  "hq_report.html"
)

# render in template directory
quarto::quarto_render(
  input = hq_report_template_path,
  execute_params = report_params
)

# remove old output, if present
if (fs::file_exists(hq_report_final_path)) {
  fs::file_delete(hq_report_final_path)
}

# move report to output directory
fs::file_move(
  path = hq_report_temp_path,
  new_path = hq_report_final_path
)
