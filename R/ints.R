#' Delete characters in a string which occur before a specified pattern
#'
#' @description
#' Vectorised over 'string' and 'pattern'. from PaulESantos/pesa.
#' @param string The string from which to extract.
#' @param pattern Pattern to look for. The default interpretation is a regular expression.
#' @param pos Position of the character used as a marker.
#'
#' @return The part of the string after the pattern.
#' @export
str_delete_before <- function(string, pattern, pos = 1){
  pos_pattern <- stringr::str_locate_all(string = string,
                                         pattern = pattern)
  start_pattern <- pos_pattern[[1]][,"start"]
  start_pattern <- rev(start_pattern)
  start_pattern <- start_pattern[pos]

  string_delete <- stringr::str_sub(string = string,
                                    start = start_pattern + 1,
                                    end = stringr::str_length(string) )

  return(string_delete)
}
