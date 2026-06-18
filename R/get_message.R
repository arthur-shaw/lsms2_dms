#' Load all messages from YAML
#'
#' @param dir Character. Directory where 
#'
#' @return Named list, where:
#'
#' - names are the names, without extension,
#' of YAML files found in `dir`
#' - contents correspond to the contents of the YAML files
#'
#' @importFrom fs dir_ls path_file path_ext_remove
#' @importFrom cli cli_abort
#' @importFrom purrr set_names map
#' @importFrom yaml read_yaml
load_messages <- function(dir) {

  yaml_file_paths <- dir |>
    # get the path of all dta files in the target directory
    fs::dir_ls(regexp = "*\\.y[a]ml")

  if (length(yaml_file_paths) == 0) {
    cli::cli_abort(
      message = c(
        "x" = get_msg("get_messages", "no_yaml_in_dir")
      )
    )
  }

  messages <- yaml_file_paths |>
    # make the file name, without extension, the name of each path
    purrr::set_names(nm = ~ fs::path_ext_remove(fs::path_file(.x))) |>
    # replace the path with the data
    purrr::map(yaml::read_yaml)

  return(messages)

}

#' Make a function factory for extracting messages
#'
#' @return Function, where `messages` and `lang` are already defined.
#'
#' @importFrom cli cli_abort
#' @importFrom purrr pluck
make_msg_extracter <- function(messages, lang) {

  # resolve references now when the factory function
  # rather than rely on lazy evaluation and be surprised
  force(messages)
  force(lang)

  function(...) {

    path <- list(...)

    if (length(path) == 0) {
      cli::cli_abort(
        message = c(
          "x" = "No message path supplied to resolver."
        )
      )
    }

    # Attempt to retrieve node
    node <- tryCatch(
      purrr::pluck(messages, !!!path),
      error = function(e) NULL
    )

    path_str <- paste(path, collapse = ".")

    if (is.null(node)) {

      cli::cli_abort(
        message = c(
          "x" = "No message found at path: '{path_str}'."
        )
      )

    }

    # Ensure language exists
    if (!lang %in% names(node)) {

      available_langs <- names(node)

      cli::cli_abort(
        message = c(
          "Language '{lang}' not available at path: '{path_str}'.",
          "Available languages: {available_langs}"
        )
      )
    }

    node[[lang]]

  }
}

#' Detect the user's language from system locale
#'
#' Checks (in order): explicit override → LANGUAGE env var → LC_MESSAGES →
#' LC_ALL → LANG → Sys.getlocale("LC_MESSAGES").
#' Extracts the ISO 639-1 two-character code
#' and validates it against supported languages.
#'
#' @param override Character. A user-supplied language code (e.g. `"fr"`).
#' When non-`NULL` and non-`NA`, this is returned immediately after
#' validation.
#' @param supported Character vector of ISO 639-1 codes your YAML defines.
#' Defaults to `c("en", "fr")`.
#' @param fallback Single string. Returned (with a warning) when detection
#' fails or the detected code is not in `supported`. Defaults to `"en"`.
#'
#' @return A single ISO 639-1 language code string.
detect_lang <- function(
  override  = NULL,
  supported = c("en", "fr"),
  fallback  = "en"
) {

  # ------------------------------------------------------------------ #
  # Helper: validate a candidate code against `supported`              #
  # ------------------------------------------------------------------ #

  validate <- function(code) {
    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      if (code %in% supported) {
        return(code)
      }
      warning(
        sprintf(
          "Language '%s' is not supported. Falling back to '%s'.",
          code, fallback
        ),
        call. = FALSE
      )
    }
    fallback
  }

  # ------------------------------------------------------------------ #
  # Helper: extract a 2-char ISO 639-1 code from a raw locale string   #
  # Handles formats like:                                              #
  #   fr_FR.UTF-8  |  fr-FR  |  French_France.1252  |  fr  |  C  |  "" #
  # ------------------------------------------------------------------ #

  parse_locale <- function(raw) {
    if (is.null(raw) || is.na(raw) || !nzchar(raw) || raw %in% c("C", "POSIX")) {
      return(NULL)
    }

    # Windows "Language_Territory.codepage" e.g. "French_France.1252"
    # or "English_United States.1252"
    win_match <- regmatches(raw, regexpr("^[A-Za-z]+(?=[_ ])", raw, perl = TRUE))
    if (length(win_match) == 1L && nchar(win_match) > 2L) {
      code <- windows_language_to_iso(win_match)
      if (!is.null(code)) return(code)
    }

    # POSIX "ll", "ll_TT", "ll_TT.encoding", "ll-TT" — grab first 2 chars
    posix_match <- regmatches(raw, regexpr("^[a-zA-Z]{2,3}", raw))
    if (length(posix_match) == 1L) {
      code <- tolower(substr(posix_match, 1, 2))
      # Sanity-check: must be letters only
      if (grepl("^[a-z]{2}$", code)) return(code)
    }

    NULL
  }

  # ------------------------------------------------------------------ #
  # 1. Explicit user override                                          #
  # ------------------------------------------------------------------ #

  if (!is.null(override) && !is.na(override) && nzchar(override)) {
    return(validate(tolower(trimws(override))))
  }

  # ------------------------------------------------------------------ #
  # 2. Walk the standard locale environment variables                  #
  #    Order mirrors GNU gettext priority.                             #
  # ------------------------------------------------------------------ #

  env_vars <- c("LANGUAGE", "LC_ALL", "LC_MESSAGES", "LANG")

  for (var in env_vars) {
    raw <- Sys.getenv(var, unset = NA_character_)

    # LANGUAGE may be a colon-separated priority list; take first entry
    if (!is.na(raw) && var == "LANGUAGE" && nzchar(raw)) {
      raw <- strsplit(raw, ":", fixed = TRUE)[[1L]][1L]
    }

    code <- parse_locale(raw)
    if (!is.null(code)) return(validate(code))
  }

  # ------------------------------------------------------------------ #
  # 3. Sys.getlocale() — most reliable on Windows when env vars absent  #
  # ------------------------------------------------------------------ #

  for (category in c("LC_MESSAGES", "LC_ALL", "LC_CTYPE")) {
    raw <- tryCatch(
      Sys.getlocale(category),
      error = function(e) NA_character_   # LC_MESSAGES absent on Windows
    )
    code <- parse_locale(raw)
    if (!is.null(code)) return(validate(code))
  }

  # ------------------------------------------------------------------ #
  # 4. Nothing worked — return fallback with a warning                  #
  # ------------------------------------------------------------------ #

  warning(
    sprintf(
      "Could not detect system language; falling back to '%s'.",
      fallback
    ),
    call. = FALSE
  )
  fallback

}

#' Windows English-name → ISO 639-1 lookup
#'
#' Covers the ~30 most common Windows locale names. Extend as needed.   #
#'
#' @param win_name Character. Language name.
#'
#' @return Character. ISO 639-1 two-character language code.
windows_language_to_iso <- function(win_name) {

  table <- c(
    Afrikaans   = "af", Albanian    = "sq", Arabic      = "ar",
    Basque      = "eu", Belarusian  = "be", Bulgarian   = "bg",
    Catalan     = "ca", Chinese     = "zh", Croatian    = "hr",
    Czech       = "cs", Danish      = "da", Dutch       = "nl",
    English     = "en", Estonian    = "et", Faeroese    = "fo",
    Finnish     = "fi", French      = "fr", Galician    = "gl",
    German      = "de", Greek       = "el", Hebrew      = "he",
    Hungarian   = "hu", Icelandic   = "is", Indonesian  = "id",
    Italian     = "it", Japanese    = "ja", Korean      = "ko",
    Latvian     = "lv", Lithuanian  = "lt", Macedonian  = "mk",
    Malay       = "ms", Maltese     = "mt", Norwegian   = "no",
    Polish      = "pl", Portuguese  = "pt", Romanian    = "ro",
    Russian     = "ru", Serbian     = "sr", Slovak      = "sk",
    Slovenian   = "sl", Spanish     = "es", Swedish     = "sv",
    Thai        = "th", Turkish     = "tr", Ukrainian   = "uk",
    Vietnamese  = "vi"
  )

  # returns NA (named) if not found → caught upstream
  unname(table[win_name])   

}
