# ==============================================================================
# Entry point for the workflow selector
# ==============================================================================

# Load project setup (paths, messages, functions)
source(here::here("R", "_setup.R"))

# Invoke the interactive workflow selector
run_workflow()
