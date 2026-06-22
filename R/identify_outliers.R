#' Resolves the `by` argument into a list of values for downstream work
#'
#' @description
#' The `by` argument could be specified as:
#'
#' - `NULL`. This is the default value.
#' - Character. A comma-separated list of variable names (e.g., `"var1, var2"`).
#' - tidyselect expression. An expression to select the `by` columns
#' (e.g. `c(var1, var2)`)
#'
#' These various specifications need to be translated into a set of objects.
#' This function translates all input formats into a single set outputs.
#'
#' @return List composed of the following named elements
#'
#' - `by_vars`. Character vector of variable names.
#' - `by_vars_chr`. Human-readable expression for outlier documentation
#' in the description field.
#' - `by_is_null`. Boolean flag of whether `by` resolves to `NULL`.
#'
#' @importFrom rlang quo_is_null enquo get_expr
#' @importFrom tidyselect eval_select
resolve_by <- function(by, df) {

  # quote `by` for downstream evaluation of the quosure
  by_quo <- rlang::enquo(by)

  # ============================================================================
  # Case 1: `NULL`
  # that is, `by = NULL`
  # ============================================================================

  if (rlang::quo_is_null(quo = by_quo)) {
    return(
      list(
        by_vars = NULL,
        by_expr_chr = "NULL",
        by_is_null = TRUE
      )
    )
  }

  # ============================================================================
  # Case 2: Atomic character vector
  # for example: `by = "s00q01"` or `by = "NULL"`
  # ============================================================================

  if (is.character(by)) {

    # allow "NULL" as a string
    if (length(by) == 1 && trimws(by) %in% c("NULL", "")) {
      return(
        list(
          by_vars = NULL,
          by_expr_chr = "NULL",
          by_is_null = TRUE
        )
      )
    }

    # split comma-separted list provided in `by` argument
    # into an vector of variable names
    by_vars <- by |>
      # split character by commma with one or more lead or trailing white spaces
      # for example: `"var1,var2"`, `"var1, var2"`, `"var1 ,  var2"`, etc.
      strsplit(split = "\\s*,\\s*") |>
      # convert the list into a character vector
      unlist()

    # check whether any variable names in `by` are not in `df`
    missing_by_vars <- base::setdiff(by_vars, names(df))
    if (length(missing_by_vars) > 0) {
      cli::cli_abort(
        "Variables specified in `by` are not in `df`: {missing_by_vars}"
      )
    }

    return(
      list(
        by_vars = by_vars,
        by_expr_chr = by,
        by_is_null = FALSE
      )
    )

  }

  # ============================================================================
  # Case 3: (tidyselect) expression
  # for example: `c(var1, var2)`
  # ============================================================================

  # get the column names matching the (tidyselect) expression in `by`
  by_vars <- by_quo |>
    # evaluate the expression in the context of the data frame specified in `df`
    # selecting matching columns
    (\(x) suppressWarnings(
      tidyselect::eval_select(data = df, expr = x)
    ))() |>
    # capture the names of matching columns in character vector
    names()

  return(
    list(
      by_vars = by_vars,
      by_expr_chr = paste(
        base::deparse(rlang::get_expr(by_quo)),
        collapse = ""
      ),
      by_is_null = length(by_vars) == 0
    )
  )

}


#' Identifier outliers
#'
#' @param df_to_check Data frame of observations to check for outliers.
#' @param df_full Data frame of all survey observations, from which outlier
#' thresholds are computed.
#' @param var Bare variable name. Variable to check for outliers.
#' @param by Either a tidy-select expression
#' (e.g., `c(var1, var2)`, `dplyr::starts_with("var")`)
#' or a character with one variable name (e.g., `"var" ) or several names
#' separated by commas (e.g., `"var1, var2"`)
#' @param exclude Numeric vector. One or more values to exclude from the
#' algorithm (e.g., 0 in zero-inflated distributions, DK values like 9999).
#' @param transform Character. Name of tranformation for data prior to outlier
#' detection. One of: "none" (no transformation), "log" (natural logarithm).
#' @param n_mad Numeric. Acceptable distance from the median as the
#' number of median absolute deviations.
#' @param min_obs Numeric. Minimum number of within-group observations for
#' outlier detection to be deemed valid.
#' @param bounds Character. Identify the bound(s) to use when identifying
#' outliers. One of: c("both", "upper", "lower").
#' @param type Numeric. Type of issue. Values are as follows:
#' `c(Reject = 1, Comment = 2, Review = 4)`
#' @param desc Character. Short, HQ-facing description of the issue.
#' @param comment Character. Longer, field staff-facing description of the
#' issue. Note: comments will evaluated as glue expressions, where expressions
#' are evaluated in the context of the data passed to the outlier function.
#' If expressions need to be evaluated beforehand, wrap that segment in a glue
#' expression so that its scope is outside of the outlier function.
#' @param comment_question Boolean. Whether or not to add a comment to the
#' variable specified in `var`.
#' issue.
#'
#' @return Data frame of outlier issues that are at the interview level and,
#' if `comment_question = TRUE`, comments at the question level.
#'
#' @importFrom cli cli_abort
#' @importFrom rlang enquo as_name quo_is_null expr_text enexpr as_name sym
#' @importFrom tidyselect eval_select
#' @importFrom dplyr group_by pick summarise n ungroup mutate left_join if_else
#' between filter rowwise select bind_rows case_when
#' @importFrom tibble tibble
#' @importFrom glue glue glue_collapse
#' @importFrom withr local_options
identify_outliers <- function(
  df_to_check,
  df_full,
  var,
  by = NULL,
  exclude = NULL,
  transform = "none",
  n_mad = 2,
  min_obs = 30,
  bounds = "upper",
  type = 1,
  desc,
  comment,
  comment_question = FALSE
) {

  # ============================================================================
  # check args
  # ============================================================================

  # ----------------------------------------------------------------------------
  # var
  # ----------------------------------------------------------------------------

  # TODO: variable exist in input data

  # ----------------------------------------------------------------------------
  # by
  # ----------------------------------------------------------------------------

  # TODO: variables exist in input data

  # ----------------------------------------------------------------------------
  # transform
  # ----------------------------------------------------------------------------

  # TODO: in a a set of valid values

  # ============================================================================
  # defuse/transform for later use/evaluation
  # ============================================================================

  # ----------------------------------------------------------------------------
  # var
  # ----------------------------------------------------------------------------

  var_chr <- rlang::as_name(rlang::enquo(var))

  # ----------------------------------------------------------------------------
  # by
  # ----------------------------------------------------------------------------

  by_vals <- resolve_by(
    df = df_full,
    by = by
  )

  by_vars <- by_vals$by_vars
  by_expr_chr <- by_vals$by_expr_chr
  by_is_null <- by_vals$by_is_null

  # ----------------------------------------------------------------------------
  # exclude
  # ----------------------------------------------------------------------------

  exclude_expr_chr <- rlang::expr_text(
    expr = rlang::enexpr(exclude),
    width = 500
  )

  # ============================================================================
  # return an empty, zero-row tibble if data to check has no observations
  # ============================================================================

  if (nrow(df_to_check) == 0) {

    df_issues <- tibble::tibble(
      interview__id = NA_character_,
      interview__key = NA_character_,
      issue_type = NA_real_,
      issue_desc = NA_character_,
      issue_comment = NA_character_,
      issue_vars = NA_character_,
      issue_loc = NA_character_,
      .rows = 0
    )

    return(df_issues)

  }

  # ============================================================================
  # compute thresholds for outliers
  # either by group(s) in `by` or overall
  # ============================================================================

  # silence info messages about grouping from `summarise`
  withr::local_options(dplyr.summarise.inform = FALSE)

  df_thresholds <- df_full |>
    (\(x) {
      if (!by_is_null) {
        dplyr::group_by(
          .data = x,
          dplyr::across(dplyr::all_of(by_vars))
        )
      } else {
        x
      }
    })() |>
    # change excluded values, if any, to NA
    (\(x) {
      if (!is.null(exclude)) {
        dplyr::mutate(
          .data = x,
          {{var}} := dplyr::if_else(
            condition = {{var}} %in% exclude,
            NA_real_,
            {{var}}
          )
        )
      } else {
        x
      }
    })() |>
    # transform values before outlier detection
    (\(x) {
      if (transform == "log") {
        # set to `NA` any values for which `log` is undefined
        # prevents `Inf` and `NaN` values
        dplyr::mutate(
          .data = x,
          {{var}} := dplyr::if_else(
            condition = {{var}} <= 0,
            NA_real_,
            {{var}}
          )
        ) |>
        dplyr::mutate(
          {{var}} := log({{var}})
        )
      } else {
        x
      }
    })() |>
    dplyr::summarise(
      n_obs = base::sum(!is.na({{var}}), na.rm = TRUE),
      med = stats::median({{var}}, na.rm = TRUE),
      mad = stats::mad({{var}}, na.rm = TRUE)
    ) |>
    (\(x) {
      if (!by_is_null) {
        dplyr::ungroup(x = x)
      } else {
        x
      }
    })() |>
    dplyr::mutate(
      # create bounds
      ll = med - (n_mad * mad),
      ul = med + (n_mad * mad)
    )

  # ============================================================================
  # combine raw data and thresholds to filter to outliers
  # ============================================================================

  df_outliers <- df_to_check |>
    # drop observations with excluded values
    # so that they are not compared against outlier thresholds and classified
    dplyr::filter(!{{var}} %in% exclude) |>
    # transform variable
    dplyr::mutate(
      # because the embrace operator is not correctly evaluated inside of
      # the if / else construct
      # need to create a temporary column where the embrace operator works
      # and then remove that column
      .temp_col = {{var}},
      transformed_val = if (transform == "log") {
        log(.temp_col)
      } else if (transform == "none") {
        .temp_col
      } else {
        .temp_col
      },
      .temp_col = NULL
    ) |>
    (\(x) {

      if (!by_is_null) {

        df_w_thresholds <- dplyr::left_join(
          x = x,
          y = df_thresholds,
          by = by_vars
        )

      } else {

        # if there is no `by` variable, then summary is a single-row df
        # extract atomic values from the columns of that df
        n_obs <- df_thresholds$n_obs
        med <- df_thresholds$med
        mad <- df_thresholds$mad
        ul <- df_thresholds$ul
        ll <- df_thresholds$ll

        # inject values as fixed values in columns
        df_w_thresholds <- dplyr::mutate(
          .data = x,
          n_obs = n_obs,
          med = med,
          mad = mad,
          ul = ul,
          ll = ll
        )

      }

      df_w_thresholds

    })() |>
    # determine whether value lies within the bound(s)
    dplyr::mutate(
      is_outlier = dplyr::case_when(
        n_obs >= min_obs & bounds == "both" ~
          dplyr::between(
            x = transformed_val,
            right = ul,
            left = ll
          ),
        n_obs >= min_obs & bounds == "upper" ~
          transformed_val > ul,
        n_obs >= min_obs & bounds == "lower" ~
          transformed_val < ll & (ll < ul),
        .default = NA
      )
    ) |>
    dplyr::filter(is_outlier == TRUE)

  # ============================================================================
  # construct the data frame of issues
  # ============================================================================

  # if no outliers found, construct an empty data frame
  if (nrow(df_outliers) == 0) {

    df_issues <- tibble::tibble(
      interview__id = NA_character_,
      interview__key = NA_character_,
      issue_type = NA_real_,
      issue_desc = NA_character_,
      issue_comment = NA_character_,
      issue_vars = NA_character_,
      issue_loc = NA_character_,
      .rows = 0
    )

  # if any outliers found, construct the data frame's contents
  } else {

    df_issues <- df_outliers |>
      dplyr::mutate(
        # recast from potentially haven class (if has special values)
        # to simple numeric so that can construct the comment properly
        {{var}} := as.numeric({{var}}),
        # evaluate any glue expressions in `desc` before dropping into
        # template below
        desc = glue::glue(desc),
        issue_type = type,
        issue_desc = glue::glue(
          "{desc}",
          "[GROUP VAL: ",
          "value={.data[[var_chr]]}, ",
          "n_obs={n_obs}, ",
          "med={med}, ",
          "ll={ll}, ",
          "ul={ul}, ",
          "transformed_val={transformed_val}",
          "]",
          "[FUN ARGS: ",
          "exclude={exclude_expr_chr}, ",
          "transform={transform}, ",
          "n_mad={n_mad}, ",
          "min_obs: {min_obs}, ",
          "bounds: {bounds}, ",
          "by: {by_expr_chr}",
          "]",
          .sep = "\n"
        ),
        issue_comment = glue::glue(comment),
        issue_vars = var_chr,
        issue_loc = NA_character_
      ) |>
      dplyr::select(
        interview__id, interview__key,
        issue_type, issue_desc, issue_comment, issue_vars, issue_loc
      )

  }

  # ============================================================================
  # construct the data frame of question-level comments
  # ============================================================================

  # create a data frame of question-level comments
  # if the user requests those comments, construct an appropriate data frame
  # otherwise, construct an empty data frame for row-binding below
  if (comment_question == TRUE) {

    main_id_vars <- c("interview__id", "interview__id")

    # get the names of all ID columns
    id_vars <- base::grep(
      x = base::names(df_outliers),
      pattern = "__id$",
      value = TRUE
    )

    # subset to those other than the main ID variables
    # that is, to all roster ID variables
    roster_vars <- id_vars[!id_vars %in% main_id_vars]

    # if any roster ID variables are present, construct coordinates to locate
    # the variable
    # otherwise, do not construct the coordinates
    # in both cases, create one issue per outlier observation with a comment
    # type
    if (length(roster_vars) > 0) {

      # construct a comma-separated series of roster coordinates
      # taking values from all ID variables in the same row
      # to identify where in the offending observation is located
      # (e.g., `2, 1, 3` for row 2 of parent roster, row 1 of child, row 3
      # of grandchild)
      df_outliers_w_loc <- df_outliers |>
        dplyr::rowwise() |>
        dplyr::mutate(
          # first, construct the series of comma-separated coordinates
          issue_loc = glue::glue_collapse(
            x = dplyr::pick(dplyr::all_of(roster_vars)),
            sep = ", "
          ),
          # then, enclose this series of cordinates in square brackets
          # to be understood # as an array by the API endpoint
          issue_loc = paste0("[", issue_loc,"]")
        ) |>
        dplyr::ungroup()

    } else {

      df_outliers_w_loc <- df_outliers |>
        dplyr::mutate(
          issue_loc = NA_character_
        )

    }

    df_var_lvl_comments <- df_outliers_w_loc |>
      dplyr::mutate(
        # recast from potentially haven class (if has special values)
        # to simple numeric so that can construct the comment properly
        {{var}} := as.numeric({{var}}),
        # evaluate any glue expressions in `desc` before dropping into
        # template below
        desc = glue::glue(desc),
        issue_type = 2,
        issue_desc = glue::glue(
          "{desc}",
          "[GROUP VAL: ",
          "value={.data[[var_chr]]}, ",
          "n_obs={n_obs}, ",
          "med={med}, ",
          "ll={ll}, ",
          "ul={ul}, ",
          "transformed_val={transformed_val}",
          "]",
          "[FUN ARGS: ",
          "exclude={exclude_expr_chr}, ",
          "transform={transform}, ",
          "n_mad={n_mad}, ",
          "min_obs: {min_obs}, ",
          "bounds: {bounds}, ",
          "by: {by_expr_chr}",
          "]",
          .sep = "\n"
        ),
        issue_comment = glue::glue(comment),
        issue_vars = var_chr
      ) |>
      dplyr::select(
        interview__id, interview__key,
        issue_type, issue_desc, issue_comment, issue_vars, issue_loc
      )

  } else {

    df_var_lvl_comments <- tibble::tibble(
      interview__id = NA_character_,
      interview__key = NA_character_,
      issue_type = NA_real_,
      issue_desc = NA_character_,
      issue_comment = NA_character_,
      issue_vars = NA_character_,
      issue_loc = NA_character_,
      .rows = 0
    )

  }

  df_issues_all <- dplyr::bind_rows(
    df_issues, df_var_lvl_comments
  )

  return(df_issues_all)

}
