#' Helper functions to extract info from Clip and Communication Numbers
#'
#'
#' @param clip_column vector with clip names
#'
#' @return returns Date, ClipNr, Coder, Group, and Species as vectors
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all distinct
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower str_remove str_c str_remove_all str_sub
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom lubridate dmy
#'
#'

date_from_clip <- function(clip_column){
  xx = unlist(clip_column) %>%
    str_split(pattern = ' ') %>%
    lapply(function(x){
      unlist(str_split(x, pattern = '_'), recursive = F)
    }) %>%
    sapply(function(x){
      jj = x[
        (x %>% str_detect(str_c(1900:2100, collapse = '|')) &
           x %>% str_detect(pattern = '-'))|
          x %>% str_detect(str_c(c('\\.0', '\\.1', '\\.2'), collapse = '|'))
      ]
      if(length(jj) == 0){return(NA)}
      return(jj)
    }) %>%
    unlist() %>%
    as.character %>%
    str_remove_all('[:upper:]') %>%
    str_remove_all('[:lower:]')

  xy <- which(str_detect(xx, '\\.'))
  yy <- xx[xy] %>% dmy()
  xx[xy] <- as.character(yy)
  return(xx)
}
