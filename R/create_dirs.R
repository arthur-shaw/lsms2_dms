#' Create directories that may not exist
#'
#' @description
#' Create all directories enumerated in directories
#' returned by `construct_paths()`.
#'
#' @param dirs List of directories returned by `construct_paths()`.
#'
#' @importFrom fs dir_create
create_dirs <- function(dirs) {

  # remove files from directory list  
  dirs$files <- NULL
  dirs$scripts <- NULL

  # flatten the list into a vector
  dir_paths <- dirs |>
    unlist()

  # create all directories, if they do not already exist
  fs::dir_create(path = dir_paths)

}
