#' Compares the distribution of Goals of the same gesture action against what would be expected from unconditional probabilities
#'
#' Function uses Fisher's exact test of Goal/Gesture combinations to determine those with higher-than-expected occurrence
#'
#' @param data data frame as extracted from Filemaker
#' @param threshold minimum number of times a gesture has to be marked as a certain goal before it is included
#' @param goal_met Do Goal_met have to be 'yes'?
#' @param reduce.goals if TRUE, some goals will be combined (e.g., GroomMe and GroomYou become 'Groom')
#' @param reduce.gestures if TRUE, some gestures are combined (e.g., HittingSoft, HitSoft, and Hitting become 'Hit)
#'
#' @return Function returns data frame with the observed probabilities for each Gesture/Goal combination, how often the gesture and goal occurred in total, how specific the Goal is to the Gesture [Specificity.gesture], how specific the Gesture is to the Goal [Specificity.goal], and the expected probabilities for the goal within the gesture and the gesture within the goal with p-values
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom stats chisq.test complete.cases dist fisher.test frequency median na.omit quantile anova
#'
#' @export
#'
gesture_context_analytical <- function(data,
                                       threshold = 10,
                                       goal_met = TRUE,
                                       reduce.goals = TRUE,
                                       reduce.gestures = TRUE) {
  # if user wants to reduce Goals, the following are combined
  if (reduce.goals) {
    data$Goal <- data$Goal %>%
      str_replace_all(pattern = "GroomMe", replacement = "Groom") %>%
      str_replace_all(pattern = "GroomMe2", replacement = "Groom") %>%
      str_replace_all(pattern = "GroomYou", replacement = "Groom") %>%
      str_replace_all(pattern = "Groom2", replacement = "Groom") %>%
      str_replace_all(pattern = "AffilationContact", replacement = "Affiliation") %>%
      str_replace_all(pattern = "AffilationDistant", replacement = "Affiliation") %>%
      str_replace_all(pattern = "AffilationMeContact", replacement = "Affiliation") %>%
      str_replace_all(pattern = "AffilationRestContact", replacement = "Affiliation") %>%
      str_replace_all(pattern = "AffilationUnclear", replacement = "Affiliation") %>%
      str_replace_all(pattern = "AffilationYouContact", replacement = "Affiliation") %>%
      str_replace_all(pattern = "GiveMeActive", replacement = "GiveMe") %>%
      str_replace_all(pattern = "GiveMeNurse", replacement = "GiveMe") %>%
      str_replace_all(pattern = "GiveMePassive", replacement = "GiveMe") %>%
      str_replace_all(pattern = "MoveAway2", replacement = "MoveAway") %>%
      str_replace_all(pattern = "PlayChangeChangeacon", replacement = "PlayChange") %>%
      str_replace_all(pattern = "PlayChangeContactcha", replacement = "PlayChange") %>%
      str_replace_all(pattern = "SexualAttentionMe", replacement = "SexualAttention") %>%
      str_replace_all(pattern = "SexualAttentionYou", replacement = "SexualAttention") %>%
      str_replace_all(pattern = "SocioSexualAttentionMe", replacement = "SocioSexualAttention") %>%
      str_replace_all(pattern = "SocioSexualAttentionYou", replacement = "SocioSexualAttention") %>%
      str_replace_all(pattern = "StopBehaviourHere", replacement = "StopBehaviour") %>%
      str_replace_all(pattern = "StopBehaviourStay", replacement = "StopBehaviour") %>%
      str_replace_all(pattern = "SupportMe", replacement = "Support") %>%
      str_replace_all(pattern = "SupportYou", replacement = "Support") %>%
      str_replace_all(pattern = "TakeObjectActive", replacement = "TakeObject") %>%
      str_replace_all(pattern = "TakeObjectNurse", replacement = "TakeObject") %>%
      str_replace_all(pattern = "TakeObjectPassive", replacement = "TakeObject") %>%
      str_replace_all(pattern = "TravelMe", replacement = "Travel") %>%
      str_replace_all(pattern = "TravelYou", replacement = "Travel")
  }
  # if the user chooses to combine Gestures, the following are combined
  if (reduce.gestures) {
    data$Gesture_record <- data$Gesture_record %>%
      str_replace_all(pattern = "BiteThreat", replacement = "Bite") %>%
      str_replace_all(pattern = "ChestBeatInformalOther", replacement = "ChestBeat") %>%
      str_replace_all(pattern = "ChestBeatInformalStanding", replacement = "ChestBeat") %>%
      str_replace_all(pattern = "Soft", replacement = "") %>%
      str_replace_all(pattern = "Hitting", replacement = "Hit") %>%
      str_replace_all(pattern = "Knocking", replacement = "Knock") %>%
      str_replace_all(pattern = "LeafClipDrop", replacement = "LeafClip") %>%
      str_replace_all(pattern = "Poking", replacement = "Poke") %>%
      str_replace_all(pattern = "Stomping", replacement = "Stomp") %>%
      str_replace_all(pattern = "TouchLongObject", replacement = "TouchObject") %>%
      str_replace_all(pattern = "TouchLongOther", replacement = "Touch")
  }
  # if the user specifies that only gestures matter if Goal_met was 'yes', remove all others
  if (goal_met) {
    data <- data %>%
      # only include if goal was met
      filter(.data$Goal_met == "Yes")
  }
  # remove unknown or unclear goals
  data <- data %>%
    filter(
      .data$Goal != "Unknown" &
        .data$Gesture_record != "Unknown" &
        .data$Gesture_record != "Unclear" &
        .data$Gesture_record != "PotentiallyNew" &
        .data$Goal != "NotApplicable" &
        .data$Goal != "Other"
    )


  gesture_context <- data %>%
    # select only gesture record and goal
    select(.data$Gesture_record, .data$Goal) %>%
    # count for combo, gesture, and goal
    group_by(.data$Gesture_record, .data$Goal) %>%
    mutate(Observed.sum = n()) %>%
    ungroup(.data$Gesture_record, .data$Goal) %>%
    # remove doubles
    distinct() %>%
    group_by(.data$Gesture_record) %>%
    mutate(Gesture.count = sum(.data$Observed.sum)) %>%
    ungroup(.data$Gesture_record) %>%
    group_by(.data$Goal) %>%
    mutate(Goal.count = sum(.data$Observed.sum)) %>%
    ungroup(.data$Goal) %>%
    # remove rare gestures and contexts
    filter(.data$Gesture.count > threshold &
      .data$Goal.count > threshold) %>%
    # make probabilities
    mutate(
      Specificity.gesture = .data$Observed.sum / .data$Gesture.count,
      Specificity.goal = .data$Observed.sum / .data$Goal.count
    ) %>%
    arrange(-.data$Specificity.gesture)

  if(nrow(gesture_context) == 1){
    gesture_context$p = 0
    gesture_context$x2 = NA
    gesture_context$phi = NA
    gesture_context$Expected = 1
    gesture_context$Prob.increase = 1.000001
    gesture_context$test.condition = 'all'
    gesture_context$null.condition = 'null'
    return(list(results = gesture_context))
  }

  # apply significance tests to get p-value of connection
  gesture_context <- gesture_context %>%
    # create counts for significance tests
    mutate(
      AllFreq = nrow(data),
      a = .data$Observed.sum,
      b = .data$Gesture.count - .data$a,
      c = .data$Goal.count - .data$a,
      d = .data$AllFreq - (.data$a + .data$b + .data$c)
    ) %>%
    mutate(NRows = nrow(gesture_context)) %>%
    rowwise() %>%
    # calculate p-value based on Fisher's exact test for count data
    mutate(p = as.vector(unlist(fisher.test(
      matrix(
        c(.data$a, .data$b, .data$c, .data$d),
        ncol = 2,
        byrow = T
      )
    )[1]))) %>%
    # calculate X2 based on Pearson's Chi-squared Test for Count Data
    mutate(x2 = as.vector(unlist(chisq.test(
      matrix(
        c(.data$a, .data$b, .data$c, .data$d),
        ncol = 2,
        byrow = T
      )
    )[1]))) %>%
    # calculate Phi
    mutate(phi = sqrt((
      .data$x2 / (.data$a + .data$b + .data$c + .data$d)
    ))) %>%
    # calculate expected value under independence
    mutate(Expected = as.vector(unlist(chisq.test(
      matrix(
        c(.data$a, .data$b, .data$c, .data$d),
        ncol = 2,
        byrow = T
      )
    )$expected[1]))) %>%
    # calculate change in probability over expected
    mutate(Prob.increase = .data$Observed.sum / .data$Expected) %>%
    ungroup() %>%
    arrange(desc(.data$Prob.increase)) %>%
    mutate(p = round(.data$p, 4)) %>%
    mutate(x2 = round(.data$x2, 2)) %>%
    mutate(phi = round(.data$phi, 2)) %>%
    select(
      -.data$a,
      -.data$b,
      -.data$c,
      -.data$d,
      -.data$NRows,
      -.data$AllFreq
    ) %>%
    suppressWarnings()

  gesture_context$test.condition <- "all"
  gesture_context$null.condition <- "null"

  return(list(results = gesture_context))
}
