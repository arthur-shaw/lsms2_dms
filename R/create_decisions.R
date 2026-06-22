#' Create decisions--on rejection, review, etc--from issues
#'
#' @param dfs_filtered List of filtered data frames.
#' @param interviews Data frame. Interviews of interest.
#' @param issues Data frame. Issues.
#' @param issue_codes_to_reject Numeric vector. Code(s) of issues to reject.
#' @param get_msg Function for getting messages.
#'
#' @return List of data frames containing decisions.
#'
#' @importFrom susoreview check_for_comments decide_action add_rejection_msgs
#' flag_persistent_issues add_rejection_msgs
#' @importFrom dplyr select left_join filter starts_with
create_decisions <- function(
  dfs_filtered,
  interviews,
  issues,
  issue_codes_to_reject,
  get_msg
) {

  # ===========================================================================
  # make decisions
  # ===========================================================================

  # check for comments
  # returns a data frame of cases that contain comments
  interviews_with_comments <- susoreview::check_for_comments(
    df_comments = dfs_filtered$interview__comments,
    df_issues = issues,
    df_cases_to_review = interviews
  )

  # decide what action to take
  decisions <- susoreview::decide_action(
    df_cases_to_review = interviews,
    df_issues = issues,
    issue_types_to_reject = issue_codes_to_reject,
    df_has_comments = interviews_with_comments,
    df_interview_stats = dfs_filtered$interview__diagnostics |>
      prepare_interview_stats()
  )

  # add rejection messages
  to_reject <- decisions[["to_reject"]]

  to_reject <- susoreview::add_rejection_msgs(
    df_to_reject = to_reject,
    df_issues = issues
  )

  # flag persistent issues
  revised_decisions <- susoreview::flag_persistent_issues(
    df_comments = dfs_filtered$interview__comments,
    df_to_reject = to_reject
  )

  # ===========================================================================
  # Extract decisions into data representing them
  # ===========================================================================

  # ---------------------------------------------------------------------------
  # To reject
  # ---------------------------------------------------------------------------

  to_reject_ids <- revised_decisions[["to_reject"]] |>
    dplyr::select(interview__id) |>
    dplyr::left_join(interviews, by = "interview__id")

  to_reject_issues <- to_reject_ids |>
    dplyr::left_join(
      issues,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::filter(issue_type %in% c(issue_codes_to_reject, 2)) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_reject_api <- revised_decisions[["to_reject"]]

  # ---------------------------------------------------------------------------
  # To review
  # ---------------------------------------------------------------------------

  to_review_ids <- decisions[["to_review"]]

  to_review_issues <- to_review_ids |>
    dplyr::left_join(
      issues,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::filter(issue_type %in% c(issue_codes_to_reject, 4)) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_review_api <- susoreview::add_rejection_msgs(
    df_to_reject = decisions[["to_review"]],
    df_issues = issues
  )

  # ---------------------------------------------------------------------------
  # To follow up
  # ---------------------------------------------------------------------------

  to_follow_up_ids <- revised_decisions[["to_follow_up"]] |>
    dplyr::left_join(interviews, by = "interview__id") |>
    dplyr::select(interview__id, interview__key)

  to_follow_up_issues <- revised_decisions[["to_follow_up"]] |>
    dplyr::left_join(issues, by = "interview__id") |>
    dplyr::left_join(
      interviews,
      by = c("interview__id", "interview__key")
    ) |>
    dplyr::select(
      interview__id, interview__key, interview__status,
      dplyr::starts_with("issue_")
    )

  to_follow_up_api <- revised_decisions[["to_follow_up"]]

  # ===========================================================================
  # Collect decisions in a named list
  # ===========================================================================

  decisions_list <- list(
    # to reject
    to_reject_ids = to_reject_ids,
    to_reject_issues = to_reject_issues,
    to_reject_api = to_reject_api,
    # to review
    to_review_ids = to_review_ids,
    to_review_issues = to_review_issues,
    to_review_api = to_review_api,
    # to follow up
    to_follow_up_ids = to_follow_up_ids,
    to_follow_up_issues = to_follow_up_issues,
    to_follow_up_api = to_follow_up_api
  )

  return(decisions_list)

}
