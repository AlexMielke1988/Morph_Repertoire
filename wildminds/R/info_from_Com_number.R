#' Helper functions to extract info from Clip and Communication Numbers
#'
#'
#' @param com_number_column vector with Communication Numbers
#' @param output can be 'coder', 'group', or 'species'
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

info_from_Com_number <- function(com_number_column, output = c('coder', 'group', 'species')){
  field.site.list <- data.frame(
    code = c("10", "11", "12", "13", "14", "20", "25", "30", "31", "32", "35", "36", "40", "50", "51", "52", "60", "61", "62",
             "70", "80", "81", "82", "90", "91", "92"),
    coder = c("CH", "CH", "CH", "CH", "CH", "CG", "DR", "GB", "GB", "GB", "VE", "VE", "BF", "AS", "AS", "AS", "KG", "KG", "KG",
              "AK", "LS", "LS", "LS", "MH", "MH", "MH"),
    group = c("Sonso", "Waibira", "Bwindi", "Bossou", "Tai", "Bwindi", "Bossou", "Waibira",
              "Kalinzu", "Issa", "Goualougo", "Goualougo", "Sonso", "Sonso", "Waibira", "Children",
              "WambaE1", "WambaP", "Kalinzu", "Sabangau", "Tai", "Waibira", "Sonso", "Waibira", "Sonso", "Children"),
    species = c("EAC", "EAC", "MG", "WAC", "WAC", "MG", "WAC", "EAC", "EAC", "EAC", "CAC",
                "LG", "EAC", "EAC", "EAC", "HS", "BNB", "BNB", "EAC", "OU", "WAC", "EAC", "EAC", "EAC", "EAC", "HS" )
  )
  unlist(com_number_column) %>%
    str_sub(1,2) %>%
    data.frame %>%
    rename('code' = '.') %>%
    left_join(field.site.list) %>%
    select(output) %>%
    unlist() %>%
    as.vector() %>%
    suppressMessages()
}
