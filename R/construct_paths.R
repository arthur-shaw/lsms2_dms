#' Construct paths for the project
#'
#' @return List. Collection of all project paths, available as named elements:
#'
#' ```yaml
#' proj: <character> # project root
#' r: <character> # R function directory
#' scripts:
#'  get_data: <character>
#'  validate: <character>
#'  reject: <character>
#'  monitor: <character>
#' files:
#'  rejection: <character>
#' i18n: <character> # translations
#' data:
#'  meta: <character>
#'  hhold:
#'    tracked: <character>
#'    downloaded: <character>
#'    combined: <character>
#'  community:
#'    tracked: <character>
#'    downloaded: <character>
#'    combined: <character>
#' validation:
#'  hhold: <character>
#'    recommendations: <character>
#'    decisions: <character>
#'    hq_reqorts: <character>
#'    team_reports: <character>
#'  community: <character>
#'    recommendations: <character>
#'    decisions: <character>
#'    hq_reqorts: <character>
#'    team_reports: <character>
#' monitoring: <character>
#' ```
#'
#' @importFrom here here
#' @importFrom fs path
#' @importFrom purrr map
construct_paths <- function() {

  # ============================================================================
  # top-level
  # ============================================================================

  dirs <- list()
  dirs$proj <- here::here()
  dirs$r <- here::here("R")
  dirs$i18n <- here::here("i18n")
  dir_data <- here::here("01_data")
  dir_validation <- here::here("02_validation")
  dirs$monitoring <- here::here("03_monitoring")

  # ============================================================================
  # data
  # ============================================================================

  # ----------------------------------------------------------------------------
  # add an entry for the metadata
  # ----------------------------------------------------------------------------

  dirs$data <- list(
    meta = fs::path(dir_data, "00_meta")
  )

  # ----------------------------------------------------------------------------
  # add sub-directory entries for all surveys
  # ----------------------------------------------------------------------------

  # use a named vector so that the list entries can be named
  # with the values being the directory name
  data_parents <- c(
    household    = "01_household",
    community    = "02_community"
  )

  # apply a function to each a list of sub-directory paths
  # to each list entry named after a survey
  dirs$data[names(data_parents)] <- data_parents |>
    purrr::map(
      .f = \(parent_dir_name) {

        # construct the full path to the parent directory
        parent_dir <- fs::path(dir_data, parent_dir_name)

        # construct sub-directories under the parent
        child_dirs <- list(
          tracked = fs::path(parent_dir, "00_tracked"),
          downloaded = fs::path(parent_dir, "01_downloaded"),
          combined = fs::path(parent_dir, "02_combined")
        )

        return(child_dirs)

      }
    )

  # ============================================================================
  # validation
  # ============================================================================

  dirs$validation <- list(
    household = fs::path(dir_validation, "01_household"),
    community = fs::path(dir_validation, "02_community")
  )

  # ----------------------------------------------------------------------------
  # add sub-directory entries for all surveys
  # ----------------------------------------------------------------------------

  dirs$validation[names(data_parents)] <- data_parents |>
    purrr::map(
      .f = \(parent_dir_name) {

        # construct the full path to the parent directory
        parent_dir <- fs::path(dir_validation, parent_dir_name)

        # construct sub-directories under the parent
        child_dirs <- list(
          recommendations = fs::path(parent_dir, "01_recommendations"),
          decisions = fs::path(parent_dir, "02_decisions"),
          hq_report = fs::path(parent_dir, "03_hq_report"),
          team_reports = fs::path(parent_dir, "04_team_reports")
        )

        return(child_dirs)

      }
    )

  # ============================================================================
  # workflow scripts
  # ============================================================================

  dirs$scripts <- list(
    get_data = fs::path(dirs$r, "01_get_data.R"),
    validate = fs::path(dirs$r, "02_validate.R"),
    reject = fs::path(dirs$r, "02_reject.R"),
    monitor = fs::path(dirs$r, "03_monitor.R")
  )

  # ============================================================================
  # files
  # ============================================================================

  dirs$files <- list(
    rejection = fs::path(
      dirs$validation$household$decisions,
      "to_reject_api.xlsx"
    )
  )

  return(dirs)

}
