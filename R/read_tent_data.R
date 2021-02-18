#' read_tent_data
#'
#' @param year path to year folder where all sites folders are hosted
#' @param ratio Argument with two possible values "NEE" and "ET"
#'
#' @return a tibble
#' @export
#' @importFrom utils data
#' @examples
#' # Basic usage
#' read_tent_data(year = "reformated_files/2003", ratio = "ET")
read_tent_data <- function(year, ratio) {

  list_files <- function(year, ratio) {
    dir(paste0(here::here(),"/", year, "/"),
        full.names = TRUE) %>%
      dir(pattern = "*.csv", full.names = TRUE) %>%
      tibble::enframe(value = "path") %>%
      dplyr::filter(stringr::str_detect(path, ratio)) %>%
      dplyr::select(path)
  }


  read_summary <- function(flnm, ratio) {
    readr::read_csv(flnm, col_types = readr::cols(date = readr::col_character())) %>%
      #read.csv(flnm) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(plot = stringr::str_remove(plot, ".txt"),
             plot = stringr::str_remove(plot, "plot"),
             ratio = ratio,
             year = stringr::str_extract(year, "[[:digit:]]+")) %>%
      dplyr::select(year, ratio, dplyr::everything(), -X1)
  }


  output <- list_files(year, ratio) %>%
    dplyr::group_nest(path) %>%
    dplyr::mutate(filename = purrr::map(path, ~gsub(".*/","",.)),
           year = purrr::map(path, ~stringr::str_extract(year, "[[:digit:]]+")),
           data = purrr::map(path, ~read_summary(., ratio)),
           dim = purrr::map(data, ~length(colnames(.)))) %>%
    dplyr::select(year,filename, dim, data) %>%
    tidyr::unnest(c(year,filename, dim))# %>%
  #filter(dim == vars) %>%
  #unnest(data)
  return(output)
}
