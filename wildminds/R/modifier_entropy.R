#' Function that goes through the modifiers one by one and tests whether they have any impact on the entropy of the distribution of a target variable
#'
#' Calculates the observed Shannon entropy of the target within a modifier level; then randomises the modifier x times and calculates the entropy for each iteration, and compares the observed and expected entropy values
#' We make the assumption that a modifier that has no impact on the entropy of the target might not contain relevant information
#'
#'
#' @param data data frame as extracted from Filemaker
#' @param modifiers a vector with the names of the modifiers to be tested
#' @param gesture_action a vector with the column that the gesture actions are stored in
#' @param plot.action a string of the gesture action of interest
#' @param cutoff minimum number of times a modifier level should occur to be counted
#' @param target vector of the same length as the gesture_action variable
#' @param method either 'shuffle' (pvalues are established by shuffling target across all cases), 'upsample' (shuffling, but controlling for cluster probabilities), or 'bootstrap' (pvalues are established by comparing each level against the distribution across all other levels). The second should be more reliable if levels are very imbalanced
#'
#' @return Data frame with the modifier, their level, the number of occurrences, the ratio observed/expected entropy, and a p-value (proportion of expected entropy values that are smaller than the observed value)
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all transmute
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite expand_grid
#' @importFrom purrr transpose
#' @importFrom tibble rownames_to_column
#'
#' @export
#'

modifier_entropy <- function(data,
                             modifiers,
                             gesture_action,
                             plot.action,
                             remove_unknown = FALSE,
                             remove_play = FALSE,
                             cutoff = 1,
                             target,
                             method = 'shuffle') {
  # define gesture action column
  gesture.action.all <- data %>%
    pull(gesture_action) %>%
    suppressMessages()

  # select modifier data
  modifier.data <- data %>%
    filter(gesture.action.all == plot.action) %>%
    select(all_of(modifiers))

  # select target
  target.act <- target[gesture.action.all == plot.action]

  # remove unknown or other Goals
  if(remove_unknown){
    rem <- !(target.act %in% c('Unknown', 'Unclear', 'Other', 'StaySame'))
    target.act <- target.act[rem]
    modifier.data <- modifier.data[rem,]
  }

  # remove unknown or other Goals
  if(remove_play){
    rem <- !(target.act %in% c('Play', 'PlayContinue'))
    target.act <- target.act[rem]
    modifier.data <- modifier.data[rem,]
  }

  # make Flexion and Orientation for back etc
  if ('Flexion_elbow' %in% colnames(modifier.data)) {
    modifier.data$Flexion_elbow <-
      ifelse(
        modifier.data$Body_part_signaller %in% c(
          "Back",
          "Body",
          "BodyFront",
          "BodyChest",
          "Face_Mouth",
          "Genitals",
          "Head",
          "Arm",
          "Leg"
        ) &
          is.na(modifier.data$Flexion_elbow),
        "NV",
        modifier.data$Flexion_elbow
      )
  }
  if ('Flexion_wrist' %in% colnames(modifier.data)) {
    modifier.data$Flexion_wrist <-
      ifelse(
        modifier.data$Body_part_signaller %in% c(
          "Back",
          "Body",
          "BodyFront",
          "BodyChest",
          "Face_Mouth",
          "Genitals",
          "Head",
          "Arm",
          "Leg"
        ) &
          is.na(modifier.data$Flexion_wrist),
        "NV",
        modifier.data$Flexion_wrist
      )
  }
  if ('Orientation' %in% colnames(modifier.data)) {
    modifier.data$Orientation <-
      ifelse(
        modifier.data$Body_part_signaller %in% c(
          "Back",
          "Body",
          "BodyFront",
          "BodyChest",
          "Face_Mouth",
          "Genitals",
          "Head",
          "Arm",
          "Leg"
        ) &
          is.na(modifier.data$Orientation),
        "NV",
        modifier.data$Orientation
      )
  }
  if ('Laterality' %in% colnames(modifier.data)) {
    modifier.data$Laterality <-
      ifelse(
        modifier.data$Body_part_signaller %in% c(
          "Back",
          "Body",
          "BodyFront",
          "BodyChest",
          "Face_Mouth",
          "Genitals",
          "Head"
        ) &
          is.na(modifier.data$Laterality),
        "NV",
        modifier.data$Laterality
      )
  }
  # create row numbers
  row.nums <- modifier.data %>%
    transmute(row_number()) %>%
    unlist(F, F)

  # add modifier name to levels
  modifier.data <- colnames_to_levels(modifier.data)

  # remove modifier levels that occur fewer than cutoff
  xx <- unlist(modifier.data) %>%
    table()
  xx <- xx[xx >= cutoff] %>%
    names()
  for (i in 1:ncol(modifier.data)) {
    modifier.data[, i] <-
      ifelse(!(unlist(modifier.data[, i]) %in% xx),
             NA,
             unlist(modifier.data[, i]))
  }


  # remove modifiers that only have one expression in the gesture action
  if(ncol(modifier.data) > 1){
    modifier.remove <- modifier.data %>%
      apply(2, table) %>%
      sapply(length)
    modifier.data <-
      modifier.data[, names(modifier.remove[modifier.remove > 1])] %>%
      data.frame()
    }


  # remove cases where all modifiers are NA

  gesture.action <-
    gesture.action.all[rowSums(is.na(modifier.data)) / ncol(modifier.data) != 1]
  target.act <-
    target.act[rowSums(is.na(modifier.data)) / ncol(modifier.data) != 1]
  xx_cols = colnames(modifier.data)
  modifier.data <-
    modifier.data[rowSums(is.na(modifier.data)) / ncol(modifier.data) != 1, ] %>%
    data.frame()
  colnames(modifier.data) = xx_cols

  if(nrow(distinct(modifier.data)) == 1){
    return(data.frame(
      modifier = NA,
      level = NA,
      count = NA,
      entropy.observed = 0,
      entropy.expected = 0,
      entropy.ratio = 1,
      pvalue.entropy = 0
    ) %>%
      drop_na())
  }

  # remove modifiers that only have one expression in the gesture action
  if(ncol(modifier.data) > 1){
  modifier.remove <-
    modifier.data %>%
    apply(2, unique) %>%
    lapply(length) %>%
    unlist()
  }
  if(ncol(modifier.data) == 1){
    modifier.remove <-
      modifier.data %>%
      apply(2, unique) %>%
      apply(2, length) %>%
      unlist()
  }
  modifier.data <-
    modifier.data[, names(modifier.remove[modifier.remove > 1])] %>%
    data.frame()

  xx_cols = colnames(modifier.data)

  # # save columns names
  # colnames(modifier.data) <- xx_cols

  # # na rows
  # rem.rows <- sapply(seq_along(modifier.data),
  #                    function(x) {
  #                      which(is.na(modifier.data[, x]))
  #                    }) %>%
  #   unlist(F, F) %>%
  #   as.vector() %>%
  #   unique()

  # remove NAs
  # target.act <-
  #   target.act[setdiff(1:nrow(modifier.data), rem.rows)]
  # modifier.data <-
  #   modifier.data[setdiff(1:nrow(modifier.data), rem.rows), ] %>%
  #   data.frame()
  # colnames(modifier.data) <- xx_cols
  # row.nums.short <- setdiff(row.nums, rem.rows)

  if (ncol(modifier.data) < 1 | nrow(modifier.data) <= 1) {
    return(
      data.frame(
        modifier = NA,
        level = NA,
        count = NA,
        entropy.observed = NA,
        entropy.expected = NA,
        entropy.ratio = NA,
        pvalue.entropy = NA
      )
    )
  }

  # set all possible levels of the target in the dataset
  all.order <-
    target.act %>%
    unique() %>%
    sort() %>%
    data.frame() %>%
    rename("target" = ".") %>%
    arrange(target) %>%
    select(target)

  all.dist <-
    target.act %>%
    na.omit() %>%
    Table() %>%
    sort() %>%
    data.frame() %>%
    rownames_to_column('target') %>%
    rename("Freq" = ".") %>%
    arrange(target) %>%
    select(Freq) %>%
    unlist(F, F) %>%
    as.numeric()

  # run loop for each column of the modifier data
  ran.entropy.group <-
    lapply(seq_along(modifier.data), function(x) {
      # create data frame for randomisation
      modifier.data.ran <- modifier.data
      # run loop across levels of modifier x
      mod.levels <-
        lapply(unique(modifier.data.ran[, x]) %>% na.omit(), function(k) {

          # calculate expected entropy of that distribution
          ent.obs <-
            infotheo::condentropy(X = target.act[modifier.data[, x] == k & !is.na(modifier.data[,x])])

          if(method == 'shuffle'){
            # run randomisation
            ran.ent <- lapply(1:100, function(y) {
              # calculate distribution of target within level of modifier after randomisation
              dist.ran <-
                target.act[sample(modifier.data.ran[, x] %>% na.omit) == k] %>%
                na.omit() %>%
                Table() %>%
                sort() %>%
                data.frame() %>%
                rownames_to_column('target') %>%
                rename("Freq" = ".") %>%
                right_join(all.order, by = 'target') %>%
                replace_na(list('Freq' = 0)) %>%
                arrange(target) %>%
                select(Freq) %>%
                unlist(F, F) %>%
                as.numeric()

              # calculate entropy of target distribution for randomised modifier level
              ent.ran <-
                infotheo::condentropy(X = target.act[sample(modifier.data.ran[, x] %>% na.omit) == k])


              return(list(entropy = ent.ran))
            })

          }

          if(method == 'upsample'){
            # run randomisation

            max.outcome <- (modifier.data.ran[, x] %>% table() %>% max()) + 1
            add_rows <- sapply(unique(modifier.data.ran[, x]) %>% na.omit(), function(i){
              which.outcome <- which(i == modifier.data.ran[, x])
              return(sample(x = rep(which.outcome, 2),
                            size = (max.outcome - length(which.outcome)),
                            replace = TRUE))
            }) %>% unlist()
            add_input <- modifier.data.ran[add_rows,] %>%
              data.frame()
            add_target <- target.act[add_rows]

            modifier.data.ran.up <- rbind(modifier.data.ran, add_input)
            target.act.up <- c(target.act, add_target)

            ran.ent <- lapply(1:100, function(y) {

              # calculate entropy of target distribution for randomised modifier level
              ent.ran <-
                infotheo::condentropy(X = target.act.up[sample(modifier.data.ran.up[, x] %>% na.omit) == k])

              return(list(entropy = ent.ran))
            })

          }


          if(method == 'bootstrap'){
            # run randomisation
            ran.ent <- lapply(1:100, function(y) {
              # calculate distribution of target within level of modifier after randomisation
              dist.ran <-
                sample(target.act[modifier.data[, x] != k & !is.na(modifier.data[,x])], replace = T) %>%
                na.omit() %>%
                Table() %>%
                sort() %>%
                data.frame() %>%
                rownames_to_column('target') %>%
                rename("Freq" = ".") %>%
                right_join(all.order, by = 'target') %>%
                replace_na(list('Freq' = 0)) %>%
                arrange(target) %>%
                select(Freq) %>%
                unlist(F, F) %>%
                as.numeric()

              # calculate entropy of target distribution for randomised modifier level
              ent.ran <-
                infotheo::condentropy(X = sample(target.act[modifier.data[, x] != k & !is.na(modifier.data[,x])], replace = T))

              return(list(entropy = ent.ran))
            })

          }

          # transpose
          ran.ent <- purrr::transpose(ran.ent)

          # return data frame that summarises the relationship of the observed and expected entropy
          return(
            data.frame(
              modifier = colnames(modifier.data)[x],
              level = k,
              count = (modifier.data[, x] == k) %>% sum(na.rm = T),
              entropy.observed = ent.obs,
              entropy.expected = unlist(ran.ent$entropy, F, F) %>% mean(na.rm = TRUE),
              entropy.ratio = mean((
                ent.obs / unlist(ran.ent$entropy, F, F)[unlist(ran.ent$entropy, F, F) > 0]
              ), na.rm = T),
              pvalue.entropy = sum(unlist(ran.ent$entropy, F, F) <= ent.obs) /
                length(ran.ent$entropy)
            )
          )
        })
      return(bind_rows(mod.levels))
    }) %>% bind_rows()

  return(ran.entropy.group)
}
