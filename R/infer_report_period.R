#' Infer Report Period from Dates
#'
#' Accept optional report start and end date strings, validate them, infer a
#' sensible reporting period when either is missing, and return a named list
#' of ISO 8601 character dates for use in a parameterised Quarto report.
#'
#' @param start Character string. Report start date (YYYY-MM-DD format).
#'   Defaults to `""` (missing).
#' @param end Character string. Report end date (YYYY-MM-DD format).
#'   Defaults to `""` (missing).
#' @param today Date. The reference date for inference. Defaults to
#'   `lubridate::today()`. Useful for testing.
#' @param boundary_day Integer. Day of week (1=Monday, 7=Sunday) marking
#'   the start of current week coverage. Defaults to 5 (Friday). If
#'   `lubridate::wday(today)` is before this day, inference uses the
#'   previous week; otherwise, the current week through today.
#'
#' @return A list with two named elements:
#'   - `start`: Character string in ISO 8601 format (YYYY-MM-DD)
#'   - `end`: Character string in ISO 8601 format (YYYY-MM-DD)
#'
#' @details
#'
#' Both `start` and `end` are normalised before inference:
#' 1. Trimmed of whitespace.
#' 2. Coerced to `NA` if empty or already missing.
#' 3. Parsed with `lubridate::ymd()`.
#' 4. If parsing fails, a warning is issued and the value is treated as `NA`.
#'
#' When both dates are valid (not `NA`), they are returned unchanged
#' (after validation that `end >= start`).
#'
#' When at least one date is missing, both are inferred based on `today` and
#' `boundary_day`. The logic is:
#' - If `wday(today) < boundary_day`: use previous week (Mon–Sun)
#' - Otherwise: use current week Monday through today
#'
#' @examples
#' # Both dates missing, today is 2026-03-26 (Thursday, before boundary_day=5)
#' infer_report_period(
#'   start = "", end = "",
#'   today = lubridate::as_date("2026-03-26"),
#'   boundary_day = 5
#' )
#' # $start
#' # [1] "2026-03-23"
#' # $end
#' # [1] "2026-03-29"
#'
#' @importFrom lubridate ymd wday floor_date days
infer_report_period <- function(
  start        = "",
  end          = "",
  today        = lubridate::today(),
  boundary_day = 5L
) {
  # ===== Step 1: Validate boundary_day =====
  boundary_day <- as.integer(boundary_day)
  if (is.na(boundary_day) || boundary_day < 1 || boundary_day > 7) {
    stop(
      paste(
        "boundary_day must be an integer between 1 (Monday) and 7 (Sunday).",
        "Got: ",
      ),
      boundary_day,
      "."
    )
  }

  # ===== Step 2: Normalise start and end =====
  normalise_date <- function(value) {
    # Trim whitespace
    value <- trimws(value)

    # Coerce empty or NA to NA
    if (value == "" || is.na(value)) {
      return(NA_character_)
    }

    # Attempt to parse
    parsed <- lubridate::ymd(value, quiet = TRUE)

    # If parsing failed, warn and return NA
    if (is.na(parsed)) {
      warning(
        "'", deparse(substitute(value)), "' value '", value,
        "' is not a valid YYYY-MM-DD date and will be treated as missing."
      )
      return(NA_character_)
    }

    # Return as character for now; will convert to Date later for comparison
    return(value)

  }

  # Create a wrapper that can pass argument names
  normalise_start <- function(s) {
    s <- trimws(s)
    if (s == "" || is.na(s)) return(NA)
    parsed <- lubridate::ymd(s, quiet = TRUE)
    if (is.na(parsed)) {
      warning(
        "'start' value '", s,
        "' is not a valid YYYY-MM-DD date and will be treated as missing."
      )
      return(NA)
    }
    return(parsed)
  }

  normalise_end <- function(e) {
    e <- trimws(e)
    if (e == "" || is.na(e)) return(NA)
    parsed <- lubridate::ymd(e, quiet = TRUE)
    if (is.na(parsed)) {
      warning(
        "'end' value '", e,
        "' is not a valid YYYY-MM-DD date and will be treated as missing."
      )
      return(NA)
    }
    return(parsed)
  }

  start_date <- normalise_start(start)
  end_date <- normalise_end(end)

  # ===== Step 3: Branch logic =====

  if (!is.na(start_date) && !is.na(end_date)) {
    # Branch 1: Both dates are valid
    if (end_date < start_date) {
      stop(
        "end date (", format(end_date, "%Y-%m-%d"),
        ") must not be before start date (",
        format(start_date, "%Y-%m-%d"), ")."
      )
    }
    # Return both dates unchanged
    result_start <- start_date
    result_end <- end_date
  } else {
    # Branch 2: At least one date is missing → infer both
    dow <- lubridate::wday(today, week_start = 1)
    this_monday <- lubridate::floor_date(today, unit = "week", week_start = 1)

    if (dow < boundary_day) {
      # Before boundary → previous week
      inferred_start <- this_monday - lubridate::days(7)
      inferred_end <- this_monday - lubridate::days(1)
    } else {
      # Boundary or later → this week through today
      inferred_start <- this_monday
      inferred_end <- today
    }

    result_start <- inferred_start
    result_end <- inferred_end
  }

  # ===== Step 4: Return formatted dates =====
  report_period <- list(
    start = format(result_start, "%Y-%m-%d"),
    end = format(result_end, "%Y-%m-%d")
  )

  return(report_period)

}
