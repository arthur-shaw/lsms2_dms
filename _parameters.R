# =============================================================================
# 1. Server details
# =============================================================================

server    <- ""
workspace <- ""
user      <- ""
password  <- ""

# =============================================================================
# 2. Questionnaires
# =============================================================================

# For each questionniare, provide:
#
# 1. text that identifies the questionnaire(s), which could be:
#   - a full name/title
#   - a sub-text
#   - a regular expression
#
# 2. the "questionnaire variable" value as it appears in Designer. To find it:
#   - log into Designer
#   - open the questionnaire
#   - click on `SETTINGS`
#   - copy the values in the `questionnaire variable` filed
#
# For more information, please see:
# https://docs.mysurvey.solutions/questionnaire-designer/components/questionnaire-variable/

# -----------------------------------------------------------------------------
# Household questionnaire
# -----------------------------------------------------------------------------

# text that identifies the questionnaire(s)
household_qnr_expr <- ""
# value of the questionnaire variable
household_qnr_var  <- ""

# name of the member-level file, as it appears in Designer and without `.dta`
members_roster_var <- ""

# name of variables used for outlier groups: admin1 and urban/rural
admin1_var <- ""
urb_rur_var <- ""

# -----------------------------------------------------------------------------
# Community questionnaire
# -----------------------------------------------------------------------------

# text that identifies the questionnaire(s)
community_qnr_expr <- ""
# value of the questionnaire variable
community_qnr_var  <- ""

# =============================================================================
# 3. Validation behavior
# =============================================================================

# Provide a comma-delimted vector of interview statuses to review
# See the values here:
# https://docs.mysurvey.solutions/headquarters/export/system-generated-export-file-anatomy/#coding_status
# Status values supported by this project:
# - Completed: 100
# - ApprovedBySupervisor: 120
# - ApprovedByHeadquarters: 130
suso_statuses_to_reject <- c(100, 120)

# Provide a comma-delimited vector of issue types to reject
# {susoreview} uses the following codes:
# - 1 = Reject
# - 2 = Comment a variable
# - 3 = Survey Soluttions validation error
# - 4 = Review
issue_codes_to_reject <- c(1)

# =============================================================================
# 4. Report period
# =============================================================================

report_start  <- ""
report_end    <- ""
