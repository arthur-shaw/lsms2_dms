# core params
# needed for validation

# ==============================================================================
# Server connection details
# ==============================================================================

# ------------------------------------------------------------------------------
# Connection details provided
# ------------------------------------------------------------------------------

check_server_details_provided <- function(
  params,
  call = rlang::caller_env()
) {

  # identify which, if any, are missing (i.e., equal to empty string)
  # with TRUE/FALSE
  missing_params <- c(
    params$server,
    params$workspace,
    params$user,
    params$password
  ) == ""

  if (any(missing_params)) {

    server_details_expected <- c("server", "workspace", "user", "password")

    # recover the names of the missing parameters
    missing_params_names <- server_details_expected[missing_params]

    # construct a list of the missing parameters
    missing_param_text <- glue::glue_collapse(
      glue::backtick(missing_params_names),
      sep = ", ",
      last = ", "
    )

    cli::cli_abort(
      message = c(
        "x" = "Required server details not provided.",
        "!" = "Details missing in {.file _parameters.R} for {missing_param_text}"
      ),
      call = call
    )

  }

}

# ------------------------------------------------------------------------------
# Server exists at specified URL
# ------------------------------------------------------------------------------

#' Check that the server exists
#'
#' @param params List of parameters
#'
#' @return Logical. If the server returns a 200 code, `TRUE.`
#' Otherwise, `FALSE.`
#'
#' @importFrom httr status_code GET
#' @importFrom cli cli_abort
check_server_exists <- function(
  params,
  call = rlang::caller_env()
) {

  server_exists <- tryCatch(
    expr = httr::status_code(httr::GET(url = params$server)) == 200,
    error = function(e) {
      FALSE
    }
  )

  if (server_exists != TRUE) {
    cli::cli_abort(
      message = c(
        "x" = "The server doesn't exist at the provided URL: {params$server}!",
        "!" = "Please correct {.arg server} in {.file parameters.R}"
      ),
      call = call
    )
  }

}

# ------------------------------------------------------------------------------
# Credentials valid
# ------------------------------------------------------------------------------

#' Check the Survey Solutions server credentials
#'
#' @inheritParams check_server_exists
#'
#' @return Logical.
#' If the credentials are valid, TRUE.
#' Otherwise, FALSE.
#'
#' @importFrom susoapi check_credentials
#' @importFrom cli cli_abort
check_server_credentials <- function(
  params,
  call = rlang::caller_env()
) {

  credentials_valid <- suppressMessages(
    susoapi::check_credentials(
      server = params$serveur,
      workspace = params$workspace,
      user = params$user,
      password = params$password,
      verbose = TRUE
    )
  )

  if (credentials_valid == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = "The API user credentials are invalid",
        "i" = "This could be due to one of the following issues:",
        "*" = "Some details may be incorrect (e.g., user name, password, etc.)",
        "*" = "The user may be the wrong type (e.g., Headquarters instead of API).",
        "*" = "The user may not have access to the target workspace"
      ),
      call = call
    )
  }
}

# ------------------------------------------------------------------------------
# confirm that the target questionnaire(s) exist on the server
# ------------------------------------------------------------------------------

check_qnr_on_server <- function(
  qnr_type,
  qnr_expr,
  qnr_var,
  params,
  call = rlang::caller_env()
) {

  tryCatch(
    expr = susoflows::find_matching_qnrs(
      matches = qnr_expr,
      server = params$server,
      workspace = params$workspace,
      user = params$user,
      password = params$password
    ),
    warning = function(cnd) {

      qnrs <- susoapi::get_questionnaires(
        server = params$server,
        workspace = params$workspace,
        user = params$user,
        password = params$password
      ) |>
      dplyr::mutate(qnr_title = glue::glue("{title} (version {version})")) |>
      dplyr::pull(qnr_title) |>
      glue::glue_collapse(sep = ", ")

      cli::cli_abort(
        message = c(
          "x" = "Aucun questionnaire {qnr_type} correspondant retrouvé",
          "i" = "Veuillez reprendre la valeur de {.code {qnr_var}}",
          "i" = "Voici les questionnaires dans l'espace de travail cible : {qnrs}"
        ),
        call = call
      )

    }

  )
}

check_server_params <- function(params) {

  check_server_details_provided(params = params)
  cli::cli_inform(
    message = c(
      "v" = "Server details complete"
    )
  )

  check_server_exists(params = params)
  cli::cli_inform(
    message = c(
      "v" = "Server URL exists"
    )
  )

  check_server_credentials(params = params)
  cli::cli_inform(
    message = c(
      "v" = "Server credentials valid"
    )
  )

}

# ==============================================================================
# Questionnaire details
# ==============================================================================

# ------------------------------------------------------------------------------
# confirm that questionnaire details provided
# ------------------------------------------------------------------------------

check_qnr_details_provided <- function(
  params,
  call = rlang::caller_env()
) {

  # identify which, if any, are missing (i.e., equal to empty string)
  # with TRUE/FALSE
  missing_params <- c(
    params$household_qnr_expr,
    params$household_qnr_var,
    params$members_roster_var,
    params$admin1_var,
    params$urb_rur_var
  ) == ""


  if (any(missing_params)) {

    qnr_details_expected <- c(
      "household_qnr_expr",
      "household_qnr_var",
      "members_roster_var",
      "admin1_var",
      "urb_rur_var"
    )

    # recover the names of the missing parameters
    missing_params_names <- qnr_details_expected[missing_params]

    # construct a list of the missing parameters
    missing_param_text <- glue::glue_collapse(
      glue::backtick(missing_params_names),
      sep = ", ",
      last = ", "
    )

    cli::cli_abort(
      message = c(
        "x" = "Required questionnaire details not provided.",
        "!" = "Details missing in {.file _parameters.R} for {missing_param_text}"
      ),
      call = call
    )

  }

}

# ------------------------------------------------------------------------------
# confirm that questionnaire variable does not have file extension
# ------------------------------------------------------------------------------

check_qnr_var_for_extension <- function(
  params,
  qnr_var,
  call = rlang::caller_env()
) {

  has_dta_extension <- fs::path_ext(params[[qnr_var]]) == "dta"

  if (has_dta_extension == TRUE) {
    cli::cli_abort(
      message = c(
        "x" = "The questionnaire variable {.var {qnr_var}} has a {.file .dta} extension.",
        "!" = "Please use the questionnaire variable as it appears in Designer."
      ),
      call = call
    )
  }

}

# ------------------------------------------------------------------------------
# confirm that questionnaire variable maps to a data set
# ------------------------------------------------------------------------------

check_qnr_var_is_dset <- function(
  combined_dir,
  params,
  qnr_var,
  call = rlang::caller_env()
) {

  dset_names <- combined_data_dir |>
    fs::dir_ls(
      type = "file",
      glob = "*.dta"
    ) |>
    fs::path_file() |>
    fs::path_ext_remove()

  is_dset_name <- params[[qnr_var]] %in% dset_names

  if (is_dset_name == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = paste(
          "The questionnaire variable {.var {qnr_var}}",
          "should have a data set with the same name.",
          "Yet no matching data set was found."
        ),
        "!" = "Please correct {.arg qnr_var} in {.file _parameters.R}"
      ),
      call = call
    )
  }

}

# ------------------------------------------------------------------------------
# check var in data
# ------------------------------------------------------------------------------

check_var_in_dset <- function(
  combined_dir,
  params,
  qnr_var,
  param_var_name
) {

  var_names <- fs::path(combined_dir, paste0(qnr_var, ".dta")) |>
    haven::read_dta(n_max = 0) |>
    names()

  var_in_dset <- params[[param_var_name]] %in% var_names

  if (var_in_dset == FALSE) {
    cli::cli_abort(
      message = c(
        "x" = paste(
          "Outlier grouping variable {.var param_var_name}",
          "({.var {params[[param_var_name]]}})",
          "not found in the in {.file {paste0(qnr_var, '.dta')}}"
        ),
        "!" = "Please fix {.var {param_var_name}} in {.file _parameters.R}"
      )
    )
  }

}
