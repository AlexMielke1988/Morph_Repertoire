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

clipnr_from_clip <- function(clip_column){
  unlist(clip_column) %>%
    str_replace_all(pattern = 'Clip ', replacement = 'Clip') %>%
    str_split(pattern = ' ') %>%
    sapply(function(x){
      xx = x[
        x %>% str_detect('Clip')
      ] %>% str_remove('Clip')
      if(length(xx) == 0){xx = NA}
      xx
    }) %>%
    unlist()
}
