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

#' Write issues files to disk in Excel and Stata formats
#'
#' @description
#' Take issues data, add the interview URL on the server, and write that
#' updated data to disk in a format-appropriate way.
#' 
#' For Excel, as a link. For Stata, as a URL as a string.
#'
#' @param df Data frame. Issues.
#' @param server Character. Base server URL.
#' @param workspace Character. Workspace name.
#' @param sheet_name Character. Name of the sheet where issues stored.
#' @param dir Character. Directory where to save the files.
#'
#' @importFrom dplyr mutate select
#' @importFrom openxlsx2 create_hyperlink wb_workbook wb_dims wb_save
#' @importFrom haven write_dta
#' @import fs path
write_issues_to_disk <- function(
  df,
  server,
  workspace,
  sheet_name = "issues",
  dir
) {

  # ----------------------------------------------------------------------------
  # construct interview URL and link
  # ----------------------------------------------------------------------------

  df_w_url <- df |>
    dplyr::mutate(
      interview_url = paste(
        server, workspace,
        "Interview", "Review", interview__id,
        sep = "/"
      ),
      interview_link = openxlsx2::create_hyperlink(
        text = interview__key,
        file = interview_url
      ),
      .before = 1
    )

  # ----------------------------------------------------------------------------
  # Excel file
  # ----------------------------------------------------------------------------

  # remove URL
  df_for_excel <- dplyr::select(df_w_url, -interview_url)

  # create workbook with sheet containing data
  wb <- openxlsx2::wb_workbook()
  wb$add_worksheet(sheet_name)
  wb$add_data(
    sheet = sheet_name,
    x = df_for_excel,
    na = ""
  )

  # add the link as a formula
  # locate the index of the column
  link_col_index <- which(names(df_for_excel) == "interview_link")
  # add a formula at that index
  wb$add_formula(
    sheet = sheet_name,
    x = df_for_excel$interview_link,
    dims = openxlsx2::wb_dims(
      rows = 2:(nrow(df_for_excel) + 1),
      cols = link_col_index
    )
  )

  openxlsx2::wb_save(
    wb,
    fs::path(dir, "issues.xlsx"),
    overwrite = TRUE
  )

  # ----------------------------------------------------------------------------
  # Stata file
  # ----------------------------------------------------------------------------

  df_for_stata <- dplyr::select(df_w_url, -interview_link)

  haven::write_dta(
    data = df_for_stata,
    path = fs::path(dir, "issues.dta")
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
