# ==============================================================================
# set up ⚙️
# ==============================================================================

# ------------------------------------------------------------------------------
# restore project environment
# ------------------------------------------------------------------------------

# TODO: uncomment 👇 once have project tracked with renv
# renv::restore(prompt = FALSE)

# ------------------------------------------------------------------------------
# load parameters
# ------------------------------------------------------------------------------

# load user's config
source(here::here("_parameters.R"))

# pack into an object to pass around between functions
params <- list(

	# server details
	server = server,
	workspace = workspace,
	user      = user,
	password  = password,
	# questionnaire
	# household
	household_qnr_var = household_qnr_var,
	household_qnr_expr = household_qnr_expr,
	members_roster_var = members_roster_var,
	admin1_var = admin1_var,
	urb_rur_var = urb_rur_var,
	# community
	community_qnr_expr = community_qnr_expr,
	community_qnr_var =community_qnr_var,
	# validation behavior	
	suso_statuses_to_reject = suso_statuses_to_reject,
	issue_codes_to_reject = issue_codes_to_reject,
	# report period
	report_start = report_start,
	report_end = report_end
	
)

# ------------------------------------------------------------------------------
# load functions
# ------------------------------------------------------------------------------

fun_dir <- here::here("R")

fun_dir |>
	# get the path of all functions by:
	# scanning the script directory and
	# excluding all workflow scripts whose file name starts with
	# - either a 2-digit number
	# - or an underscore
	fs::dir_ls(type = "file") |>
	purrr::keep(
		.p = ~ !grepl(
			x = fs::path_file(.x),
			pattern = "^([0-9]{2}_|_)", 
		)
	) |>
	purrr::walk(.f = ~ source(.x))

# ------------------------------------------------------------------------------
# construct file paths and create necessary directories
# ------------------------------------------------------------------------------

dirs <- construct_paths()
create_dirs(dirs = dirs)
