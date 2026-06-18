# ==============================================================================
# get data 💾
# ==============================================================================

# ------------------------------------------------------------------------------
# microdata
# ------------------------------------------------------------------------------

qnr_types <- c("household", "community")

dl_specs <- tibble::tribble(
  ~ qnr_expr, ~ type,
  household_qnr_expr, "household",
  community_qnr_expr, "community",
)

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
