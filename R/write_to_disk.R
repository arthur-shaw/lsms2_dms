#' Write a data frame to disk
#'
#' @param df Data frame.
#' @param dir Character. Directory where file should be written.
#'
#' @importFrom rlang current_env
#' @importFrom fs path
#' @importFrom haven write_dta
#' @importFrom writexl write_xlsx
write_df_to_disk <- function(
  df,
  df_name = NULL,
  dir
) {

  if (is.null(df_name)) {
    df_name <- base::substitute(df, env = rlang::current_env()) |>
      base::deparse()
  }

  # Stata
  haven::write_dta(
    data = df,
    path = fs::path(dir, paste0(df_name, ".dta"))
  )

  # Excel
  writexl::write_xlsx(
    x = df,
    path = fs::path(dir, paste0(df_name, ".xlsx")),
    col_names = TRUE
  )

}

#' Write data frame list element to disk
#' 
#' @param df_list List of data frames
#' @param df_name Character. Name of entry in list containing the target df.
#' @param dir Character. Directory where data should be written.
#'
#' @importFrom fs path
#' @importFrom haven write_data
write_list_el_to_disk <- function(df_list, df_name, dir) {

  df <- df_list[[df_name]]

  # Stata
  haven::write_dta(
    data= df,
    path = fs::path(
      dir, paste0(df_name, ".dta")
    )
  )

  # Excel
  writexl::write_xlsx(
    x = df,
    path = fs::path(dir, paste0(df_name, ".xlsx")),
    col_names = TRUE
  )

}

#' Write all elements of data fram list to disk
#' 
#' @inheritParams write_list_el_to_disk
#'
#' @param purrr walk
write_df_list_to_disk <- function(df_list, dir) {

  # capture the names of data frame entries in list
  list_names  <- names(df_list)

  # iternatively save entries to disk
  purrr::walk(
    .x = list_names,
    .f = ~ write_list_el_to_disk(
      df_list = df_list,
      df_name = .x,
      dir = dir
    )

  )
}
