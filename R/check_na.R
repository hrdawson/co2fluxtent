#' Check NA values
#' The function checks if the tibble contains NA values. Give as an output a tibble with variables that have NA values at the amount of this.
#'
#' @param df A tibble or dataframe
#'
#' @return A tibble with the variables that contain NA values.
#' @export
#'
#' @examples
#' # Basic usage
#' mtcars %>%
#' check_na()
check_na <- function(df) {
  n <- function(x) {
    sum(is.na(x))
  }
  df <- df %>%
    dplyr::summarise_all(list(~n(.)))

  t_df <- data.table::transpose(df)
  colnames(t_df) <- rownames(df)
  rownames(t_df) <- colnames(df)
  t_df <- t_df %>%
    tibble::rownames_to_column(.data = .) %>%
    tibble::as_tibble(.) %>%
    purrr::set_names(c("vars", "amount_na")) %>%
    dplyr::filter(amount_na > 0)
  return(t_df)
}
