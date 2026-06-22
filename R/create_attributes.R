#' Create data attributes
#'
#' @param dfs_filtered List of data frames filtered to observations of interest
#'
#' @return Data frame of all created data attributes
#' for the observations of interest
#'
#' @importFrom susoreview count_obs any_vars count_vars
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
create_attributes <- function(
  dfs_filtered
) {

  # ============================================================================
  # [1] household roster
  # ============================================================================

  attrib_n_heads <- susoreview::count_obs(
    df = dfs_filtered$members,
    where = hh01_q03 == 1,
    attrib_name = "n_heads",
    attrib_vars = "hh01_q03"
  )

  # ============================================================================
  # [7] food away from home
  # ============================================================================

  # food away from home
  fafh_meals <- c(
    "hh07_q03", # breakfast
    "hh07_q05", # lunch
    "hh07_q07", # dinner
    "hh07_q09", # snacks
    "hh07_q011", # hot drinks
    "hh07_q13", # non-alcoholic drinks
    "hh07_q15" # alcoholic drinks
  ) |>
	paste(collapse = "|")

  attrib_conso_fafh <- dfs_filtered$members |>
    dplyr::mutate(
      fafh = dplyr::if_any(
        .cols = dplyr::matches(fafh_meals),
        .fns = ~ .x == 1
      )
    ) |>
    susoreview::any_obs(
      where = fafh == 1,
      attrib_name = "any_food_away_from_home",
      attrib_vars = fafh_meals
    )

  # ============================================================================
  # [8, 9A-9D] food and non-food consumption
  # ============================================================================

  # all other consumtpion
  conso_spec <- tibble::tribble(
    ~ attrib_name, ~ fn_name, ~ df_name, ~ condition, ~ attrib_vars,
    "n_foods_at_home", "count_vars", "households", "hh08_q04__",
      "hh08_q04__",
    "any_nf_cons_7d", "any_obs", "non_food_7_days", "hh09a_q02 == 1",
      "hh09a_q02",
    "any_nf_cons_30d", "any_obs", "non_food_30_days", "hh09b_q02 == 1",
      "hh09b_q02",
    "any_nf_cons_6m", "any_obs", "non_food_6_months", "hh09c_q02 == 1",
      "hh09c_q02",
    "any_nf_cons_12m", "any_obs", "non_food_12_months", "hh09d_q02 == 1",
      "hh09d_q02",
  ) |>
  # prepend name of list containing the target data frame
	dplyr::mutate(df_name = paste0("dfs_filtered$", df_name))

  attribs_consumption <- purrr::pmap(
    .l = conso_spec,
    .f = create_attribute_from_spec
  )

  # ============================================================================
  # [14] assets
  # ============================================================================

  attrib_any_assets <- susoreview::any_vars(
    df = dfs_filtered$households,
    var_pattern = "hh14_q02__",
    var_val = 1,
    attrib_name = "any_assets"
  )

  # ============================================================================
  # bind together all attributes
  # ============================================================================

  # ----------------------------------------------------------------------------
  # transform `attribs_*` objects from a list of df to single df
  # ----------------------------------------------------------------------------

  # note: not relevant for LSMS 2.0

  # ----------------------------------------------------------------------------
  # put together all attributes data
  # ----------------------------------------------------------------------------

  # compose the regular expression to target two types of objects
  objects_rexpr <- c(
    "^attribs_", # objects previously lists of dfs
    "^attrib_" # objects simple that are simple df
  ) |>
    paste(collapse = "|")

  # put together in a single data frame all attributes
  attribs <- dplyr::bind_rows(mget(ls(pattern = objects_rexpr)))

  return(attribs)

}

#' Create attributes from a set of character-based specs
#'
#' @param attrib_name Character. Name of attribute.
#' @param fn_name Character. Name of `{susoreview}` function to use.
#' @param df_name Character. Name of the data frame.
#' @param condition Character. Expression used by the function.
#' @param attrib_vars Character. Regular expression for selecting variables used
#' in creating the attribute.
#'
#' @return Data frame. Same return value as that of the function in `fn_name`.
#'
#' @importFrom cli cli_abort
#' @importFrom base get0 is.null switch
#' @importFrom rlang caller_env parse_expr expr sym eval_bare
create_attribute_from_spec <- function(
  attrib_name,
  fn_name,
  df_name,
  condition,
  attrib_vars
) {

  valid_fn_names <- c(
    # from single column
    "extract_attribute",
    "create_attribute",
    # from several columns
    "count_vars",
    "count_list",
    "any_vars",
    # from several rows
    "count_obs",
    "any_obs",
    "sum_vals"
  )

  # check that function is valid
  if (!fn_name %in% valid_fn_names) {

    cli::cli_abort(
      message = c(
        "x" = "Invalid attribute function name provided",
        "i" = "Use either {.or {.arg {valid_fn_names}}}"
      )
    )

  }
  
  # fetch the matching data frame; return NULL if match not found
  df <- base::get0(
    x = df_name,
    envir = rlang::caller_env(),
    ifnotfound = NULL
  )

  # check whether the data frame values returned is NULL
  if (base::is.null(df)) {

    cli::cli_abort(
      message = c(
        "x" = "Data frame named {.arg {df_name}} does not exist.",
        "i" = "Please correct the name provided."
      )
    )

  }

  # transform character string into an expression
  condition_expr <- rlang::parse_expr(condition)

  # compose the call based on the function name provided
  call_expr <- base::switch(
    fn_name,
    # from a single column
    extract_attribute = rlang::expr(
      susoreview::extract_attribute(
        df = df,
        var = !!rlang::sym(condition),
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    create_attribute = rlang::expr(
      susoreview::create_attribute(
        df = df,
        condition = !!condition_expr,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    # from several columns
    count_vars = rlang::expr(
      susoreview::count_vars(
        df = df,
        var_pattern = condition,
        var_val = 1,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    count_list = rlang::expr(
      susoreview::count_list(
        df = df,
        var_pattern = condition,
        missing_vals = c("##N/A##", "", NA_character_),
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    any_vars = rlang::expr(
      susoreview::any_vars(
        df = df,
        var_pattern = condition,
        var_val = 1,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    # from several rows
    count_obs = rlang::expr(
      susoreview::count_obs(
        df = df,
        where = !!condition_expr,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    any_obs = rlang::expr(
      susoreview::any_obs(
        df = df,
        where = !!condition_expr,
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    ),
    sum_vals = rlang::expr(
      susoreview::sum_vals(
        df = df,
        var = !!rlang::sym(condition),
        attrib_name = attrib_name,
        attrib_vars = attrib_vars
      )
    )
  )

  # evaluate the composed expression
  attrib_df <- rlang::eval_bare(call_expr)

  return(attrib_df)

}
