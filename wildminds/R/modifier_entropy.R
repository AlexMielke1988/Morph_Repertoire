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
                             cutoff = 1,
                             target) {
  # define gesture action column
  gesture.action.all <- data %>%
    select(gesture_action) %>%
    unlist() %>%
    as.vector() %>%
    suppressMessages()

  # select modifier data
  modifier.data <- data %>%
    filter(gesture.action.all == plot.action) %>%
    select(all_of(modifiers))

  # select target
  target.act <- target[gesture.action.all == plot.action]

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
  xx <- xx[xx > cutoff] %>%
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
      pvalue.entropy = 0,
      JS.observed = 0,
      JS.expected = 0,
      JS.ratio = 1,
      pvalue.JS = 0
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
        pvalue.entropy = NA,
        JS.observed = NA,
        JS.expected = NA,
        JS.ratio = NA,
        pvalue.JS = NA
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
          # calculate distribution of target within level of modifier
          dist.obs <-
            target.act[modifier.data[, x] == k & !is.na(modifier.data[,x])] %>%
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

          # calculate expected entropy of that distribution
          ent.obs <-
            -sum(dist.obs / sum(dist.obs) * log2(dist.obs / sum(dist.obs)),
                 na.rm = TRUE)

          # calculate the JS distance between the overall distribution of target levels and that within the modifier level
          dist.obs.stan <- dist.obs
          dist.obs.stan[dist.obs.stan == 0] <- 0.001
          dist.obs.stan <- dist.obs.stan / sum(dist.obs.stan)

          all.dist.stan <- all.dist / sum(all.dist)

          n <- 0.5 * (dist.obs.stan + all.dist.stan)
          JS.obs <-
            0.5 * (sum(dist.obs.stan * log(dist.obs.stan / n)) + sum(all.dist.stan * log(all.dist.stan / n)))

          # run randomisation
          ran.ent <- lapply(1:100, function(y) {
            # calculate distribution of target within level of modifier after randomisation
            dist.ran <-
              target.act[sample(modifier.data.ran[, x] %>% na.omit) == k] %>%
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
              -sum(dist.ran /
                     sum(dist.ran) *
                     log2(dist.ran / sum(dist.ran)),
                   na.rm = TRUE)

            # calculate the JS distance between the overall distribution of target levels and that of the randomised modifier level
            dist.ran.stan <- dist.ran
            dist.ran.stan[dist.ran.stan == 0] <- 0.001
            dist.ran.stan <- dist.ran.stan / sum(dist.ran.stan)
            n <- 0.5 * (dist.ran.stan + all.dist.stan)
            JS.ent <-
              0.5 * (sum(dist.ran.stan * log(dist.ran.stan / n)) + sum(all.dist.stan * log(all.dist.stan / n)))

            return(list(entropy = ent.ran,
                        JS.divergence = JS.ent))
          })

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
                length(ran.ent$entropy),
              JS.observed = JS.obs,
              JS.expected = unlist(ran.ent$JS.divergence, F, F) %>% mean(na.rm = TRUE),
              JS.ratio = mean((
                JS.obs / unlist(ran.ent$JS.divergence, F, F)
              ), na.rm = T),
              pvalue.JS = sum(unlist(ran.ent$JS.divergence, F, F) >= JS.obs) /
                length(ran.ent$JS.divergence)
            )
          )
        })
      return(bind_rows(mod.levels))
    }) %>% bind_rows()

  return(ran.entropy.group)
}
