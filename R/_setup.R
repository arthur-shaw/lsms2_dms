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

source(here::here("_parameters.R"))

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
