# ==============================================================================
# confirm the installation of system requirements
# ==============================================================================

# ------------------------------------------------------------------------------
# first, install the packages required for confirming
# ------------------------------------------------------------------------------

pkgs_for_confirming <- c(
  "pkgbuild",
  "cli",
  "glue",
  "quarto",
  "here"
)

# install any missing packages
# for each package:
# - confirm whether absent
# - install if so
base::lapply(
  X = pkgs_for_confirming,
  FUN = function(x) {
    if (
      !base::require(
        x,
        quietly = TRUE,
        warn.conflicts = FALSE,
        character.only = TRUE
      )
    ) {
      base::message(paste0("Installing ", x))
      utils::install.packages(
        x,
        quiet = TRUE
      )
    }
  }
)

# ------------------------------------------------------------------------------
# then, load error messages and messaging functions
# ------------------------------------------------------------------------------

# load message functions
source(here::here("R", "get_message.R"))

# ingest messages from disk
messages <- load_messages(here::here("i18n"))

# assign as part of function
get_msg <- make_msg_extracter(messages = messages, lang = detect_lang())

# ------------------------------------------------------------------------------
# next, execute the confirmation script
# ------------------------------------------------------------------------------

source(here::here("R", "00_confirm_sys_reqs.R"))

# ==============================================================================
# activate the renv-locked project environment
# ==============================================================================

# activate environment
source("renv/activate.R")

# restore the environment without asking the user
renv::restore(prompt = FALSE)
