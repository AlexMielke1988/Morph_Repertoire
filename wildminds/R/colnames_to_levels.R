#' Helper functions, adds column name to every level of the dataframe
#'
#'
#' @param modifier.data data frame with columns representing modifiers
#'
#' @return Same data frame, but column names added to levels
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite expand_grid
#'
#'

colnames_to_levels <- function(modifier.data){
  # add modifier name to levels
  for (i in colnames(modifier.data)) {
    modifier.data[, i] <- modifier.data %>%
      select(i) %>%
      mutate(coln = i) %>%
      unite(i,
            .data$coln,
            i,
            sep = ".",
            remove = T,
            na.rm = T
      ) %>%
      unlist() %>%
      na_if(y = i) %>%
      as.vector()
  }
  return(modifier.data)
}
