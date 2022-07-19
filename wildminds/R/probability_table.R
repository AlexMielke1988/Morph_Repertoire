#' Takes the dataset and modifiers and calculates conditional and unconditional probabilities of all Gesture/Modifier Level combinations
#'
#' Basis for a couple of other functions
#'
#' @param data ata frame as extracted from Filemaker
#' @param modifiers a vector with the names of the modifiers to be tested
#'
#' @return data frame containing all gesture actions and modifier levels with their occurrences and probabilities
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom reshape2 melt
#'
#' @export
#'

probability_table <- function(data, modifiers) {

  # select gesture actions
  gesture.action <- data %>%
    select(.data$Gesture_record) %>%
    unlist() %>%
    as.vector() %>%
    suppressMessages()

  # select modifiers
  modifier.data <- data %>%
    select(all_of(modifiers))

  # create probabilities between gestures and modifier levels
  probability_table <-
    bind_rows(lapply(seq_along(modifier.data), function(x) {
      #create grid of gestures and modifiers
      probability_table <- expand_grid(
        gesture_action = unique(gesture.action),
        level = unique(modifier.data[, x]) %>%
          na.omit() %>%
          unlist() %>%
          as.vector()
      ) %>%
        mutate(modifier = colnames(modifier.data)[x]) %>%
        distinct() %>%
        mutate(level = as.character(.data$level)) %>%
        # count occurrence of combinations
        left_join(
          melt(table(
            gesture.action, unlist(modifier.data[, x])
          )) %>%
            mutate(Var2 = as.character(.data$Var2)),
          by = c("gesture_action" = "gesture.action", "level" = "Var2")
        ) %>%
        rename("count" = .data$value) %>%
        # calculate probabilities of occurrence
        left_join(
          melt(table(
            gesture.action, unlist(modifier.data[, x])
          ) /
            rowSums(table(
              gesture.action, unlist(modifier.data[, x])
            ))) %>%
            mutate(Var2 = as.character(.data$Var2)),
          by = c("gesture_action" = "gesture.action", "level" = "Var2")
        ) %>%
        rename("probability" = .data$value) %>%
        suppressMessages()

      probability_table
    }))

  # make pretty
  probability_table <- probability_table %>%
    arrange(.data$gesture_action, .data$modifier, .data$level) %>%
    select(.data$gesture_action, .data$modifier, .data$level, .data$count, .data$probability)
  return(probability_table)
}
