# ==============================================================================
# LOGICAL ISSUES
# ==============================================================================

#' Create issues from logical
#'
#' @description
#' Create the following issue types for cases of interest:
#'
#' - Impossible situations
#' - Highly unlikely situations
#' - Internal inconsistencies
#'
#' @param df_attribs Data frame of attributes.
#' @param get_msg Function for retrieving the right message
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom susoreview create_issue
#' @importFrom glue glue
#' @importFrom dplyr bind_rows
create_logical_issues <- function(
  df_attribs,
  get_msg
) {

  # ============================================================================
  # impossible situations
  # ============================================================================

  # ----------------------------------------------------------------------------
  # no reference person
  # ----------------------------------------------------------------------------

  desc_no_head <- get_msg("issues", "no_head", "desc")

  issue_no_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads == 0,
    type = 1,
    desc = desc_no_head,
    comment = glue::glue(
      get_msg("issues", "no_head", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # more than 1 reference person
  # ----------------------------------------------------------------------------

  desc_more_than_one_head <- get_msg("issues", "more_than_one_head", "desc")

  issue_more_than_one_head <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "n_heads",
    where = n_heads > 1,
    type = 1,
    desc = desc_more_than_one_head,
    comment = glue::glue(
      get_msg("issues", "more_than_one_head", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # no food consumption in the past 7 days
  # ----------------------------------------------------------------------------

  desc_no_food <- get_msg("issues", "no_food", "desc")

  issue_no_food_consumed <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c("any_food_away_from_home", "n_foods_at_home"),
    where = any_food_away_from_home == 0 & n_foods_at_home == 0,
    type = 1,
    desc = desc_no_food,
    comment = glue::glue(
      get_msg("issues", "no_food", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # no non-food consumption
  # ----------------------------------------------------------------------------

  desc_no_non_food <- get_msg("issues", "no_non_food", "desc")

  issue_no_nf_conso <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = c(
      "any_nf_cons_7d", "any_nf_cons_30d", "any_nf_cons_6m", "any_nf_cons_12m"
    ),
    type = 1,
    where = dplyr::if_all(
      .cols = c(
        any_nf_cons_7d, any_nf_cons_30d, any_nf_cons_6m, any_nf_cons_12m
      ),
      .fns = ~ .x == 0
    ),
    desc = desc_no_non_food,
    comment = glue::glue(
      get_msg("issues", "no_non_food", "comment")
    )
  )

  # ----------------------------------------------------------------------------
  # owns no consumer assets
  # ----------------------------------------------------------------------------

  desc_owns_no_assets <- get_msg("issues", "owns_no_assets", "desc")

  issue_owns_no_assets <- susoreview::create_issue(
    df_attribs = df_attribs,
    vars = "any_assets",
    where = any_assets == 0,
    type = 1,
    desc = desc_owns_no_assets,
    comment = glue::glue(
      get_msg("issues", "owns_no_assets", "comment")
    )
  )

  # ============================================================================
  # unlikely situations
  # ============================================================================

  # N/A - essetially all handled in SuSo

  # ============================================================================
  # internal inconsistencies
  # ============================================================================

  # N/A - essetially all handled in SuSo

  # ============================================================================
  # combine issues
  # ============================================================================

  obj_expr_issues <- "^issue[s]*_"

  # combine all issues
  # note: `dplyr::bind_rows()` combines lists of data frames
  issues <- dplyr::bind_rows(mget(ls(pattern = obj_expr_issues)))

  # ============================================================================
  # return issues issues
  # ============================================================================

  if (nrow(issues) == 0) {

    issues <- tibble::tibble(
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

  return(issues)

}

# ==============================================================================
# OUTLIERS
# ==============================================================================

#' Convert character vector to a single, comma-separated string
#'
#' @description
#' Creates a compilation of `by` variables in a form that
#' `identify_outliers()` can process via `resolve_by()`.
#'
#' @param x Character vector.
#'
#' @return Length 1, comma-separated string.
to_comma_sep_str <- function(x) {
  paste(x, collapse = ", ")
}

#' Create outlier issues
#'
#' @description
#' Create outlier issues for cases of interest while drawing from all data
#'
#' @param dfs_full List of data frames that contain all survey observations.
#' @param dfs_filtered List of data frames that are filtered to observations
#' of interest.
#' @param admin1_var Character.
#' @param urb_rur_var Character.
#' @param get_msg Function for retrieving the right message
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom purrr map pmap
#' @importFrom dplyr semi_join
#' @importFrom tibble tribble
#' @importFrom rlang sym
#' @importFrom glue glue
#' @importFrom scales label_number
#' @importFrom haven zap_label
create_outlier_issues <- function(
  dfs_full,
  dfs_filtered,
  admin1_var,
  urb_rur_var,
  get_msg
) {

  # ============================================================================
  # create outlier for each data set of interest
  # ============================================================================

  # ----------------------------------------------------------------------------
  # extract comment strings
  # ----------------------------------------------------------------------------

  desc <- get_msg("outliers", "global", "desc")
  comment_intro <- get_msg("outliers", "global", "comment_intro")
  comment_var <- get_msg("outliers", "global", "comment_var")
  comment_body <- get_msg("outliers", "global", "comment_body")

  # force `renv` to include `scales` as a dependency
  # since the expressions in glue'd in from `outliers.yaml` isn't detected
  if (FALSE) scales::label_number()

  # ----------------------------------------------------------------------------
  # household-level
  # ----------------------------------------------------------------------------

  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
  # compose group variables
  # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

  by_area <- c(admin1_var, urb_rur_var)
  by_construction_materials <- c(
    "hh11_q12", # walls
    "hh11_q13", # roof
    "hh11_q14", # floor
    "hh11_q15" # number of rooms
  )
  by_area_materials <- c(by_area, by_construction_materials)

  #' Add variables for the daily expenditure
  #'
  #' @param df Data frame
  #'
  #' @return Data frame with `daily_elec_exp` and `daily_trash_exp` added.
  #'
  #' @importFrom dplyr mutate case_when
  add_daily_rates <- function(df) {

    df |>
      dplyr::mutate(
        daily_elec_exp = dplyr::case_when(
          # daily
          hh12_q06b == 1 ~ hh12_q06,
          # weekly
          hh12_q06b == 2 ~ hh12_q06 / 7,
          # fortnightly
          hh12_q06b == 3 ~ hh12_q06 / 14,
          # monthly
          hh12_q06b == 4 ~ hh12_q06 / 30,
          # quarterly
          hh12_q06b == 5 ~ hh12_q06 / (3*30),
          # yearly
          hh12_q06b == 6 ~ hh12_q06 / (365.25),
          # default
          .default = hh12_q06
        ),
        daily_trash_exp = dplyr::case_when(
          # daily
          hh13_q18b == 1 ~ hh13_q18a,
          # weekly
          hh13_q18b == 2 ~ hh13_q18a / 7,
          # fortnightly
          hh13_q18b == 3 ~ hh13_q18a / 14,
          # monthly
          hh13_q18b == 4 ~ hh13_q18a / 30,
          # quarterly
          hh13_q18b == 5 ~ hh13_q18a / (3*30),
          # yearly
          hh13_q18b == 6 ~ hh13_q18a / (365.25),
          # default
          .default = hh13_q18a
        )
      )
  }

  hhold_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    # --------------------------------------------------------------------------
    # [11] housing
    # --------------------------------------------------------------------------
    "hh11_q04",
      to_comma_sep_str(c("hh11_q04_tu", by_area_materials)), # hypothetical rent
    "hh11_q05",
      to_comma_sep_str(c("hh11_q05_tu", by_area_materials)), # actual rent
    "hh11_q09", to_comma_sep_str(by_area_materials), # repairs
    "hh11_q11", to_comma_sep_str(by_area_materials), # improvements
    "hh11_q15",	to_comma_sep_str(by_area_materials), #	number rooms
    # --------------------------------------------------------------------------
    # [12] energy
    # --------------------------------------------------------------------------
    "daily_elec_exp", "NULL", # electricity expenditure, scaled to daily
    "hh12_q09", "NULL", # number of lightbulbs
    "hh12_q16", "NULL", # number of outages in past 7 days
    "hh12_q24", "hh12_q23, hh12_q24_tu", # time to collect fuel, by fuel, time
    "hh12_q25", 
      to_comma_sep_str(c("hh12_q23", by_area)),
      # fuel expenditure in past 30 days, by fuel and area
    # --------------------------------------------------------------------------
    # [13] WASH
    # --------------------------------------------------------------------------
    "hh13_q04",
      to_comma_sep_str(c("hh13_q02", by_area)),
      # time to get water, by source and area
    "hh13_q18a",
      to_comma_sep_str(c("hh13_q17", by_area)),
      # refuse disposal expenditure in past 12 months, by method and area
    # --------------------------------------------------------------------------
    # [15A] household business
    # --------------------------------------------------------------------------
    "hh15a_q18", "hh15a_q17",
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(desc = get_msg("outliers", "household", var)) |>
  dplyr::ungroup()

  issues_hhold_lvl <- purrr::pmap(
    .l = hhold_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = add_daily_rates(dfs_filtered$households),
      df_full = add_daily_rates(dfs_full$households),
      var = !!rlang::sym(..1),
      by = ..2,
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        glue::glue(comment_intro),
        glue::glue(comment_var),
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # asset-level
  # ----------------------------------------------------------------------------

  asset_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "hh14_q03", "r_assets__id",
    "hh14_q05", "r_assets__id",
    "hh14_q06", "r_assets__id",
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(desc = get_msg("outliers", "asset", var)) |>
  dplyr::ungroup()

  issues_asset_lvl <- purrr::pmap(
    .l = asset_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$r_assets,
      df_full = dfs_full$r_assets,
      var = !!rlang::sym(..1),
      by = ..2,
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        glue::glue(comment_intro),
        glue::glue(comment_var),
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # business-level
  # ----------------------------------------------------------------------------

  biz_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "hh15b_q19", "NULL",
    "hh15b_q20", "hh15b_q1_code",
  )

  issues_biz_lvl <- purrr::pmap(
    .l = biz_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$r_business,
      df_full = dfs_full$r_business,
      var = !!rlang::sym(..1),
      by = ..2,
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the appropriate thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # income-level
  # ----------------------------------------------------------------------------

  issues_income_lvl <- identify_outliers(
    df_to_check = dfs_filtered$r_income,
    df_full = dfs_full$r_income,
    var = hh17_q03,
    by = r_income__id,
    exclude = NULL,
    transform = "log",
    bounds = "upper",
    type = 2,
    desc = get_msg("outliers", "income", "desc"),
    comment = paste(
        # evaluate variable label inside outlier function
        glue(get_msg("outliers", "income", "comment_intro")),
        # get literal text
        get_msg("outliers", "income", "comment_var"),
        comment_body
    ),
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # education expenditure-level
  # ----------------------------------------------------------------------------

  educ_exp_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "hh02_q17", "hh02_q17_edu_expense__id",
  )	|>
	dplyr::rowwise() |>
  dplyr::mutate(desc = get_msg("outliers", "education_exp", var)) |>
	dplyr::ungroup()

  issues_educ_exp_lvl <- purrr::pmap(
    .l = educ_exp_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$hh02_q17_edu_expense,
      df_full = dfs_full$hh02_q17_edu_expense,
      var = !!rlang::sym(..1),
      by = !!rlang::sym(..2),
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate inside the outlier function
        get_msg("outliers", "education_exp", "comment_intro"),
        # evaluate before the outlier function
        glue::glue(comment_var),
        # show the outlier amount
        # using the appropriate thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # medical services expenditure-level
  # ----------------------------------------------------------------------------

  medical_services_exp_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "hh03a_q31", "services_roster__id", # medical service fees
    "hh03a_q33", "services_roster__id", # transport to/from medical service venue

  issues_medical_services_exp_lvl <- purrr::pmap(
    .l = medical_services_exp_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$services_roster,
      df_full = dfs_full$services_roster,
      var = !!rlang::sym(..1),
      by = ..2,
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the appropriate thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      ),
      comment_question = TRUE
    )
  )


  # ----------------------------------------------------------------------------
  # member-level
  # ----------------------------------------------------------------------------

  member_lvl_specs <- tibble::tribble(
    ~ var, ~ by,

    # --------------------------------------------------------------------------
    # [2] education
    # --------------------------------------------------------------------------
    "hh02_q14", "NULL", # scholarship
    # health
    "hh03a_q13", "NULL", #meidcal transport
    "hh03a_q15", "NULL", # consultation costs
    "hh03a_q18", "NULL", # medicines
    "hh03a_q23", "NULL", # overnight health facility stays
    "hh03a_q25", "NULL", # transport to/from hospital
    # --------------------------------------------------------------------------
    # [3D] family planning
    # --------------------------------------------------------------------------
    "hh03d_q08", "NULL", # number of antenatal visits
    # --------------------------------------------------------------------------
    # [4] labor
    # --------------------------------------------------------------------------
    "hh04_q34", "NULL", # hours worked last week
    "hh04_q40_amount", "hh04_q40_unit", # pay per time unit
    "hh04_q41_amount", "hh04_q41_unit", # profit per time unit
    # --------------------------------------------------------------------------
    # [7] FAFH
    # --------------------------------------------------------------------------
    "hh07_q04", "NULL", # Breakfast away from home
    "hh07_q06", "NULL",	# Lunch away from home
    "hh07_q08", "NULL", # Dinner away from home
    "hh07_q10", "NULL", # Snack away from home
    "hh07_q12", "NULL", # Hot drinks away from home
    "hh07_q14", "NULL", # Non-alcoholic drinks away from home
    "hh07_q16", "NULL", # Alcoholic drinks away from home
  ) |>
	dplyr::rowwise() |>
  # TODO: update message reference
  dplyr::mutate(desc = get_msg("outliers", "member", var)) |>
	dplyr::ungroup()

  issues_member_lvl <- purrr::pmap(
    .l = member_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$members,
      df_full = dfs_full$members,
      var = !!rlang::sym(..1),
      by = ..2,
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the appropriate thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # food-level
  # ----------------------------------------------------------------------------

  #' Add unit price of purchase
  #'
  #' @param df Food expenditure data frame
  #'
  #' @import dplyr mutate
  #' @import rlang .data
  add_food_unit_price <- function(df) {
    df |>
      dplyr::mutate(
        unit_purchase_price = .data$hh08_q11 / .data$hh08_q10_amount
      )
  }

  food_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "hh08_q05_amount", "hh08_q05_unit", # Total quantity for unit
    "hh08_q10_amount", "hh08_q10_unit", # Quantity purchased for unit
    "unit_purchase_price", "hh08_q10_unit",
  ) |>
	dplyr::rowwise() |>
  # TODO: update message reference
  dplyr::mutate(desc = get_msg("outliers", "food", var)) |>
  dplyr::ungroup()

  issues_food_lvl <- purrr::pmap(
    .l = food_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = add_food_unit_price(dfs_filtered$food_consumption_at_home),
      df_full = add_food_unit_price(dfs_full$food_consumption_at_home),
      var = !!rlang::sym(..1),
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      desc = glue::glue(desc),
      comment = paste(
        # evaluate
        glue::glue(comment_intro),
        glue::glue(comment_var),
        # show the outlier amount
        # using the appropriate thousands and decimal marks
        # evaluating the data in the context of the outlier function
        comment_body
      ),
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # 7-day non-food expenditure
  # ----------------------------------------------------------------------------

  issues_non_food_7d <- identify_outliers(
    df_to_check = dfs_filtered$non_food_7_days,
    df_full = dfs_full$non_food_7_days,
    var = hh09a_q03,
    by = non_food_7_days__id,
    exclude = NULL,
    transform = "log",
    n_mad = 2,
    min_obs = 30,
    bounds = "upper",
    type = 2,
    # TODO: compose message later
    desc = "",
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # 30-day non-food expenditure
  # ----------------------------------------------------------------------------

  issues_non_food_30d <- identify_outliers(
    df_to_check = dfs_filtered$non_food_30_days,
    df_full = dfs_full$non_food_30_days,
    var = hh09b_q03,
    by = non_food_30_days__id,
    exclude = NULL,
    transform = "log",
    n_mad = 2,
    min_obs = 30,
    bounds = "upper",
    type = 2,
    # TODO: compose message later
    desc = "",
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # 6-month non-food expenditure
  # ----------------------------------------------------------------------------

  issues_non_food_6m <- identify_outliers(
    df_to_check = dfs_filtered$non_food_6_months,
    df_full = dfs_full$non_food_6_months,
    var = hh09c_q03,
    by = non_food_6_months__id,
    exclude = NULL,
    transform = "log",
    n_mad = 2,
    min_obs = 30,
    bounds = "upper",
    type = 2,
    # TODO: compose message later
    desc = "",
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # 12-month non-food expenditure
  # ----------------------------------------------------------------------------

  issues_non_food_12m <- identify_outliers(
    df_to_check = dfs_filtered$non_food_12_months,
    df_full = dfs_full$non_food_12_months,
    var = hh09d_q03,
    by = non_food_12_months__id,
    exclude = NULL,
    transform = "log",
    n_mad = 2,
    min_obs = 30,
    bounds = "upper",
    type = 2,
    # TODO: compose message later
    desc = "",
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # parcel-level
  # ----------------------------------------------------------------------------

# hfc	ag01_q08		number of plots is not an outlier

  # ----------------------------------------------------------------------------
  # plot-level
  # ----------------------------------------------------------------------------

  add_amt_per_area <- function(df, area_var) {
    df |>
      dplyr::mutate(
        # fertilizer
        amt_fertilizer_type1 = ag02_q16b1_qty / {{area_var}},
        amt_fertilizer_type2 = ag02_q16c1_qty / {{area_var}},
        # pesticide or herbicide
        amt_pest_herb_type1 = ag02_q16b1_qty / {{area_var}},
        amt_pest_herb_type2 = ag02_q16c1_qty / {{area_var}}
      )
  }

  plot_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    "ag02_q07_size", "ag02_q07_unit", # plot size
    "amt_fertilizer_type1", "ag02_q16b1_unit", # fertilizer per ha, by unit
    "amt_fertilizer_type2", "ag02_q16c1_unit", # fertilizer per ha, by unit
    "amt_pest_herb_type1", "ag02_q18b1_unit", # pesticide/herbicide per ha, unit
    "amt_pest_herb_type2", "ag02_q18c1_qty", # pesticide/herbicide per ha, unit
  )

  # ----------------------------------------------------------------------------
  # parcel-plot-crop-level
  # ----------------------------------------------------------------------------

  amt_per_plot_area <- function(
    crop_df,
    plot_df,
    area_var
  ) {

    crop_df |>
      dplyr::left_join(
        y = plot_df,
        by = c("interview__id", "r_parcel__id", "r_plot__id")
      ) |>
      dplyr::mutate(
        amt_seed_temp_non_veg = ag03_q05_quantity / {{area_var}},
        amt_seed_temp_veg = ag03_q10_quantity / {{area_var}},
        num_plants = ag03_q17 / {{area_var}},
        amt_seed_tree_perm = ag03_q21_quantity /  {{area_var}}
      )

  }

  crop_lvl_specs <- tibble::tribble(
    ~ var, ~ by,
    # quantity seed planted per area, by crop and unit
    "amt_seed_temp_non_veg", "r_crop__id, ag03_q05_unit", # temporary, non-veg
    "amt_seed_temp_veg", "r_crop__id, ag03_q10_unit", # temporary, non-veg
    "num_plants", "r_crop__id", # number trees/plans per area, by crop
    "amt_seed_tree_perm", "r_crop__id", "ag03_q21_unit", # tree / permanent

  )

  # ----------------------------------------------------------------------------
  # crop-{traditional|improved} level
  # ----------------------------------------------------------------------------

  #' Add unit price variable
  #'
  #' @param df Data frame
  #' @param value Bare variable name. Value column.
  #' @param quantity Bare variable name. Quantity column.
  #' @param suffix Character. Optional suffix to add to `unit_price`.
  #'
  #' @importFrom dplyr if_else mutate
  #' @import rlang
  add_unit_price <- function(df, value, quantity, suffix = "") {

    col_suffix <- dplyr::if_else(
      condition = suffix == "",
      true = "",
      false = paste0("_", suffix)
    )

    df |>
      dplyr::mutate(
        "unit_price{col_suffix}" := {{value}} / {{quantity}}
      )

  }

  issues_seed_purchase <- identify_outliers(
    df_to_check = add_unit_price(
      df = dfs_filtered$r_seed_type,
      value = ag05_q07,
      quantity = ag05_q06_amount
    ),
    df_full = add_unit_price_seed(
      df = dfs_full$r_seed_type,
      value = ag05_q07,
      quantity = ag05_q06_amount
    ),
    var = unit_price,
    by = c(
      # seed type
      r_seeds_05__id, r_seed_type__id,
      # purchase unit
      ag05_q06_unit,
      # whether used coupon/voucher when purchasing
      ag05_q08
    ),
    exclude = NULL,
    transform = "log",
    bounds = "upper",
    type = 2,
    # TODO: compose description
    desc = "",
    # TODO: compose comment
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # crop-input level
  # ----------------------------------------------------------------------------

  issues_input_purchase <- identify_outliers(
    df_to_check = dfs_filtered$r_crop_inputs,
    df_full = dfs_full$r_crop_inputs,
    var = ag06_q03_quantity,
    by = c(
      # crop input
      r_crop_inputs,
      # unit
      ag06_q03_unit
    ),
    exclude = NULL,
    transform = "log",
    bounds = "upper",
    type = 2,
    # TODO: compose description
    desc = "",
    # TODO: compose comment
    comment = "",
    comment_question = TRUE
  )

  # ----------------------------------------------------------------------------
  # crop-input level
  # ----------------------------------------------------------------------------

  crop_labor_lvl_specs <- tibble::tribble(
    ~ var,
    "ag08_q03", # hired labor
    "ag08_q09" # free/exchange labor
  )

  issues_crop_labor <- purrr::pmap(
    .l = crop_labor_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$ag08r_labor,
      df_full = dfs_full$ag08r_labor,
      var = !!rlang::sym(..1),
      by = ag08r_labor__id, # person type
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # livestock level
  # ----------------------------------------------------------------------------

  livestock_lvl_specs <- tibble::tribble(
    ~ var,
    "ag10_q04", # num kept
    "ag10_q05", # num kept are exotic
    "ag10_q08", # num kept but not owned
    "ag10_q10", # num owned but not kept
    "ag10_q12", # num kept 12 months ago
    "ag10_q13", # num born
    "ag10_q14", # num bought
    "unit_price_bought", # constructed with `add_unit_price()`
    "ag10_q16", # num received
    "ag10_q17", # num sold
    "unit_price_sold", # constructed with `add_unit_price()`
    "ag10_q19", # num slaughtered
    "unit_price_slaughtered", # constructed with `add_unit_price()`
    "ag10_q23", # num died from disease
    "ag10_q24", # num died from other causes
    "ag10_q27", # num went missing
  )

  issues_livestock <- purrr::pmap(
    .l = livestock_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check =
        dfs_filtered$lv_roster |>
	      add_unit_price(
          value = ag10_q15,
          quantity = ag10_q14,
          suffix = "bought"
        ) |>
	      add_unit_price(
          value = ag10_q18,
          quantity = ag10_q17,
          suffix = "sold"
        ) |>
	      add_unit_price(
          value = ag10_q21,
          quantity = ag10_q19,
          suffix = "slaughtered"
        )
      ,
      dfs_full =
        dfs_full$lv_roster |>
	      add_unit_price(
          value = ag10_q15,
          quantity = ag10_q14,
          suffix = "bought"
        ) |>
	      add_unit_price(
          value = ag10_q18,
          quantity = ag10_q17,
          suffix = "sold"
        ) |>
	      add_unit_price(
          value = ag10_q21,
          quantity = ag10_q19,
          suffix = "slaughtered"
        )
      ,
      var = !!rlang::sym(..1),
      by = lv_roster__id, # livestock type
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # livestock cost level
  # ----------------------------------------------------------------------------

  add_animal_counts <- function(
    costs_df,
    anim_df
  ) {

    num_anim_per_group <- anim_df |>
      # generate an ID that matches the livestock costs roster
      # and that aggregates detailed livestock to livestock groups
      dplyr::mutate(
        new_anim_id = dplyr::recode_values(
          x = .data$livestock__id,
          # recodes from old ID to new ID
          # where multiple
          c(11, 12, 13) ~ 1,
          c(21, 22) ~ 2,
          c(31) ~ 3,
          c(41, 42, 43) ~ 4,
          c(51, 52, 53) ~ 5,
          c(61) ~ 6,
          c(71) ~ 7,
          c(81) ~ 8
        )
      ) |>
      # compute a sum within each newly created livestock group
      dplyr::summarise(
        num_animals = sum(num, na.rm = TRUE),
        .by = new_anim_id
      )

    costs_w_anim_counts <- dplyr::left_join(
      x = costs_df,
      y = anim_df,
      by = c("lvstk_id__id" == "new_anim_id")
    )

    return(costs_w_anim_counts)

  }

  livestock_costs_lvl_specs <- tibble::tribble(
    ~ var,
    "ag11_q05", # food
    "ag11_q06", # breeding
    "ag11_q07", # preventative treatment
    "ag11_q08", # curative treatment
  )

  issues_livestock_costs <- purrr::pmap(
    .l = livestock_costs_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = add_animal_counts(
        costs_df = dfs_filtered$lvstk_id,
        anim_df = dfs_filtered$lv_roster
      ),
      dfs_full = add_animal_counts(
        costs_df = dfs_full$lvstk_id,
        anim_df = dfs_full$lv_roster
      ),
      var = !!rlang::sym(..1),
      by = lvstk_id__id, # livestock group in costs roster
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # livestock product level
  # ----------------------------------------------------------------------------

  # TODO: consider moving this into the livestock level headinc
  # different survey section, same livestocks data frame

  livestock_products_lvl_specs <- tibble::tribble(
    "ag12_q03", # num clutching periods
    "ag12_q04", # num laying eggs
    "ag12_q05", # num eggs per clutching period
    "unit_price_liters_per_anim", # actually number of liters per milked animal
    "unit_price_milk_sold", # unit price of milk sold
    "ag12_q19",
  )

  issues_livestock_products <- purrr::pmap(
    .l = livestock_products_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$lv_roster |>
        add_unit_price(
          value = ag12_q13,
          quantity = ag12_q12,
          suffix = "liters_per_anim"
        ) |>
        add_unit_price(
          value = ag12_q16,
          quantity = ag12_q15,
          suffix = "milk_sold"
        )
      ,
      dfs_full = dfs_full$lv_roster |>
        add_unit_price(
          value = ag12_q13,
          quantity = ag12_q12,
          suffix = "liters_per_anim"
        ) |>
        add_unit_price(
          value = ag12_q16,
          quantity = ag12_q15,
          suffix = "milk_sold"
        )
      ,
      var = !!rlang::sym(..1),
      by = lv_roster__id, # livestock
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )

  # ----------------------------------------------------------------------------
  # livestock labor level
  # ----------------------------------------------------------------------------

  add_hourly_rate <- function(
    df,
    daily_rate_var,
    hours_per_day_var
  ) {

    df |>
      dplyr::mutate(
        hourly_rate = {{daily_rate_var}} / {{hours_per_day_var}}
      )

  }

  livestock_labor_lvl_specs <- tibble::tribble(
    ~ var,
    "ag13_q03", # num workers
    "hourly_rate", # computed hourly rate
  )

  issues_livestock_labor <- purrr::pmap(
    .l = livestock_labor_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = add_hourly_rate(
        df = dfs_filtered$r_aglabor_nonhh,
        daily_rate_var = ag13_q06,
        hours_per_day_var = ag13_q05b
      ),
      dfs_full = add_hourly_rate(
        df = dfs_full$r_aglabor_nonhh,
        daily_rate_var = ag13_q06,
        hours_per_day_var = ag13_q05b
      ),
      var = !!rlang::sym(..1),
      by = r_aglabor_nonhh, # livestock
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )


  # ----------------------------------------------------------------------------
  # ag equipment level
  # ----------------------------------------------------------------------------

  ag_equipment_lvl_specs <- tibble::tribble(
    ~ var,
    "ag15_q05",
    "ag15_q06",
  )

  issues_ag_equipment <- purrr::pmap(
    .l = ag_equipment_lvl_specs,
    .f = ~ identify_outliers(
      df_to_check = dfs_filtered$ag_assets,
      df_full = dfs_full$ag_assets,
      var = !!rlang::sym(..1),
      by = r_aglabor_nonhh__id, # ag equipment
      exclude = NULL,
      transform = "log",
      bounds = "upper",
      type = 2,
      # TODO: compose description
      desc = "",
      # TODO: compose comment
      comment = "",
      comment_question = TRUE
    )
  )

  # ============================================================================
  # combine issues
  # ============================================================================

  # expression to identify issue objects by name
  obj_expr_issues <- "^issue[s]*_"

  # combine all issues
  # note: `dplyr::bind_rows()` combines lists of data frames
  issues <- dplyr::bind_rows(mget(ls(pattern = obj_expr_issues)))

  # ============================================================================
  # return issues issues
  # ============================================================================

  if (nrow(issues) == 0) {

    issues <- tibble::tibble(
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

  return(issues)

}
#' Create issues
#'
#' @description
#' Create issues for outliers and non-outliers alike
#'
#' @param df_attribs Data frame of attributesa.
#' @param dfs_full List of data frames that contain all survey observations.
#' @param dfs_filtered List of data frames that are filtered to observations
#' of interest.
#' @param admin1_var Character.
#' @param urb_rur_var Character.
#' @param get_msg Function for retrieving the right message

#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom dplyr bind_rows
create_issues <- function(
  df_attribs,
  dfs_full,
  dfs_filtered,
  admin1_var,
  urb_rur_var,
  get_msg
) {

  # ============================================================================
  # Create issues
  # ============================================================================

  issues_logical <- create_logical_issues(
    df_attribs = df_attribs,
    get_msg = get_msg
  )

  issues_outlier <- create_outlier_issues(
    dfs_full = dfs_full,
    dfs_filtered = dfs_filtered,
    admin1_var = admin1_var,
    urb_rur_var = urb_rur_var,
    get_msg = get_msg
  )

  # ============================================================================
  # Combine all issues
  # ============================================================================

  # expression to match intermediary issues objects
  # but, importantly, not the combined issues object
  obj_expr_issues <- "^issue[s]*_"

  # combine all issues
  issues <- dplyr::bind_rows(mget(ls(pattern = obj_expr_issues)))

  # remove intermediary objects to lighten load on memory
  rm(list = ls(pattern = obj_expr_issues))

  return(issues)

}

#' Add issues for unanswered questions
#'
#' @description
#' To the data frame of issues, add an issue for each interview
#' where answers have been left unaswered.
#'
#' To do so, extract information from `interview__diagnostics`.
#' Then, construct an issues uses that information.
#'
#' @return Data frame of issues, of the form created by
#' `susoreview::create_issue()`
#'
#' @importFrom susoreview add_issue_if_unanswered
add_issue_for_unanswered_q <- function(
  dfs_filtered,
  interviews,
  issues
) {

  # extract number of questions unanswered
  # use `interview__diagnostics` file rather than request stats from API
  interview_stats <- dfs_filtered$interview__diagnostics |>
    prepare_interview_stats()

  # add error if interview completed, but questions left unanswered
  # returns issues data supplemented with unanswered question issues
  issues_plus_unanswered <- susoreview::add_issue_if_unanswered(
    df_cases_to_review = interviews,
    df_interview_stats = interview_stats,
    df_issues = issues,
    n_unanswered_ok = 0,
    issue_desc = get_msg("issues", "any_unanswered", "desc"),
    issue_comment = glue::glue(get_msg("issues", "any_unanswered", "comment"))
  )

  return(issues_plus_unanswered)

}










# hfc	hh12_q19		If no grid in community, then grid must not be reported in community qnr


# ==============================================================================
# 🌾 AGRICULTURE
# ==============================================================================

# hfc	ag01_q04_size, ag01_q04_unit, ag01_q05		parcel area by uses is not an outlier
# 👆 can't do because q5 is multi-select

# 👇 likely reference an outdated version of the questionnaire 
# hfc	ag03_q09, ag03_q01a		nb of plantings by crop is not an outlier
# hfc	ag03_q14, ag03_q01a		nb of beds by crop is not an outlier
# hfc	ag03_q14, ag03_q01a		size of beds by crop is not an outlier


# 👇 since reporting is at the state/condition level per parcel-plot-crop, it's unclear how to use
# hfc	ag03_q30_quantity, ag03_q30_unit, ag03_q30_condition, ag02_q07_size,ag02_q07_unit, ag03_q04		crop yield is not an outlier
# hfc	ag03_q31_quantity, ag03_q31_unit, ag03_q31_condition, ag03_q15		crop average yield is not an outlier
# hfc	ag04_q09, ag04_q10, cropType		sell price is not an outlier: check both quantity and values

# 👇 percentage should be between 0 and 100. No need for outlier check
# suso,hfc	ag04_q04, ag04_q07, ag04_q16, ag04_q17, ag04_q18, ag04_q19,  ag04_q20,ag04_q21,  , ag04_q22, , ag04_q23, ag04_q25		sum of share of crop dispositions <100

# 👇 no connection between seed and plot(s) of a particular size
# hfc	ag05_q06_qantity, ag05_q06_unit, ag02_q07_size,ag02_q07_unit, ag03_q03		quantity purcased per Hectar is not an outlier
