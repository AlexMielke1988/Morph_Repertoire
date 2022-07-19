#' Calculate conditional probabilities of modifier levels across modifiers within the same gesture action, to see if some can be removed
#'
#' For example, if in a Reach gesture, Hand Flexion and Finger Flexion occur together 100% of the time, we can just treat them as the same
#'
#' @param data data frame as extracted from Filemaker (potentially cleaned)
#' @param modifiers name of the modifiers that should be included
#' @param threshold above which conditional probability (both ways) should modifier levels be considered to be strongly connected?
#'
#' @return Function returns a dataframe with those modifier level combinations that have above-threshold conditional probabilities both ways
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#'
#' @export
#'
#'

conditional_modifiers <- function(data,
                                  modifiers,
                                  threshold = 0.98) {
  # calculate basic probabilitiesprobabilities
  prob.table <- probability_table(
    data = data,
    modifiers = modifiers
  )
  prob.table <- prob.table %>%
    filter(.data$count > 0)

  # for each gesture action, go through and determine whether there are any modifier levels that perfectly predict each other
  cond_modifiers <- lapply(
    # select gesture actions one after another
    unique(prob.table$gesture_action),
    function(y) {
      # calculate combination probabilities for that Gesture
      two_det <- calculate_prob_of_comb(
        data = data %>%
          filter(.data$Gesture_record == y),
        # select only modifiers that occur in that gesture action
        modifiers = unique(
          prob.table %>%
            filter(.data$gesture_action == y &
                     .data$count > 0) %>%
            select(.data$modifier) %>%
            unlist()
        )
        # select the symmetrical information from the calculate_prob_of_comb function
      )$symmetrical %>%
        # select only cases where both conditional probabilities are high
        filter(.data$conditionalBgA > threshold &
                 .data$conditionalAgB > threshold) %>%
        mutate(gesture_action = y) %>%
        # select columns
        select(
          .data$gesture_action,
          .data$mod1,
          .data$mod2,
          .data$observed,
          .data$unconditional.prob
        )

      return(two_det)
    }
  ) %>%
    bind_rows()%>%
    filter(.data$observed > 0)

  return(cond_modifiers)
}
