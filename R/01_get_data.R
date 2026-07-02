# ==============================================================================
# get data 💾
# ==============================================================================

qnr_types <- c("household", "community")

dl_specs <- tibble::tribble(
  ~ qnr_expr, ~ type,
  household_qnr_expr, "household",
  community_qnr_expr, "community",
)

# ------------------------------------------------------------------------------
# check server parameters
# ------------------------------------------------------------------------------

check_server_params(params = params)

# ------------------------------------------------------------------------------
# check target questionnaires exist
# ------------------------------------------------------------------------------

qnr_specs <- dl_specs |>
	dplyr::mutate(
    qnr_var = dplyr::case_when(
      type == "household" ~ "household_qnr_expr",
      type == "community" ~ "community_qnr_expr",
      .default = NA_character_
    )
  )

purrr::pwalk(
  .l = qnr_specs,
  .f = ~ check_qnr_on_server(
    qnr_expr = ..1,
    qnr_type = ..2,
    qnr_var = ..3,
    params = params
  )
)

# ------------------------------------------------------------------------------
# microdata
# ------------------------------------------------------------------------------


purrr::pwalk(
  .l = dl_specs,
  .f = ~ get_data(
    qnr_expr = ..1,
    allowed_types = qnr_types,
    type = ..2,
    dirs = dirs,
    server = server,
    workspace = workspace,
    user = user,
    password = password,
    get_msg = get_msg
  )
)

# ------------------------------------------------------------------------------
# team composition
# ------------------------------------------------------------------------------

cli::cli_alert_info(get_msg("get_data", "getting_metadata"))

get_team_composition(
  dir = dirs$data$meta,
  server = server,
  workspace = workspace,
  user = user,
  password = password
)
