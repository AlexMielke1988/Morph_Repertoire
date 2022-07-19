#' Calculate probabilities of modifiers and modifier combinations occurring
#'
#'
#' @param data data frame as extracted from Filemaker (potentially cleaned)
#' @param modifiers name of the modifiers that should be included
#'
#' @return Function returns a dataframe with observed probabilities for each combination in the dataset
#'
#' @importFrom Rfast Table
#' @importFrom arrangements combinations
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower str_c
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite drop_na
#' @importFrom utils head
#'
#' @export
#'

calculate_prob_of_comb <- function(data, modifiers) {
  # select only modifiers of interest
  modifier.data <- data %>%
    select(all_of(modifiers))

  # add the name of the modifier to every level to ensure that overlap in names doesn't matter
  modifier.data <- colnames_to_levels(modifier.data)

  # remove cases where all the modifiers are NA
  modifier.data <-
    modifier.data[rowSums(is.na(modifier.data)) / ncol(modifier.data) != 1, ]

  # create a matrix where every possible modifier level is a column, and gets a 1 if it is present and a 0 if not for each event
  mod.matr <- unique(unlist(modifier.data)) %>%
    na.omit() %>%
    unlist() %>%
    as.vector()
  modifier.matr <-
    matrix(0,
      ncol = length(mod.matr),
      nrow = nrow(modifier.data)
    ) %>% data.frame()
  colnames(modifier.matr) <- mod.matr

  # go and add 1s for those who are present
  for (i in colnames(modifier.data)) {
    # produce all possible modifier levels
    for (f in 1:length(mod.matr)) {
      if (str_detect(mod.matr[f], i)) {
        modifier.matr[, f] <-
          ifelse(modifier.data[, i] == mod.matr[f], 1, 0) %>% as.vector()
        modifier.matr[, f] <-
          ifelse(is.na(modifier.data[, i]), NA, modifier.matr[, f]) %>% as.vector()
      }
    }
  }

  # make list of all modifiers present in a given row
  elements.pres <- lapply(1:nrow(modifier.data), function(x) {
    xx <-
      modifier.data[x, ] %>%
      unlist() %>%
      na.omit() %>%
      as.vector()
  })

  # make list of all possible modifiers that could have been chosen in a given row
  elements.pos <- lapply(1:nrow(modifier.matr), function(x) {
    xx <-
      colnames(modifier.matr)[!is.na(modifier.matr[x, ])] %>%
      unlist() %>%
      as.vector()
  })
  # calculate all observed AU combinations for each element/observation, using the function defined below
  zz.pres <- rapply(elements.pres,
    function(x) {
      yy <- unlist(x, recursive = FALSE, use.names = FALSE)
      xx <-
        compute_possible_combs(mods = yy, max_comb_len = 2)
    },
    how = "unlist"
  )
  # calculate all possible AU combinations for each element/observation
  zz.pos <- rapply(elements.pos,
    function(x) {
      yy <- unlist(x, recursive = FALSE, use.names = FALSE)
      xx <-
        compute_possible_combs(mods = yy, max_comb_len = 2)
    },
    how = "unlist"
  )
  # count how many times each AU combination occurred
  xtab.pres <- Table(zz.pres)
  xtab.pos <- Table(zz.pos)

  # add these together in a data frame
  xtab.pres <- data.frame(
    combination = names(xtab.pres),
    observed = xtab.pres
  )
  xtab <- data.frame(
    combination = names(xtab.pos),
    possible = xtab.pos
  ) %>%
    left_join(xtab.pres) %>%
    replace_na(list(observed = 0)) %>%
    suppressMessages()

  # remove those within the same modifier
  xtab$combination %>%
    str_split(pattern = ":") %>%
    sapply(function(x) {
      str_split(x, pattern = ".") %>%
        lapply(head, -1) %>%
        lapply(str_c, collapse = ".") %>%
        unlist() %>%
        duplicated() %>%
        sum()
    }) -> remove


  # calculate conditional probabilities of modifier2 happening, conditional on modifier1 is present
  cond.probs <-
    bind_rows(lapply(colnames(modifier.matr), function(x) {
      # check how often each option for each modifier if the current modifier is TRUE
      xx <- modifier.matr %>%
        filter(modifier.matr[, x] == 1) %>%
        select(-x) %>%
        apply(2, Table, simplify = F) # this should result in a list where each modifier level is listed with the 0, 1, or NAs
      xx <- bind_rows(lapply(seq_along(xx), function(y) {
        # for each modifier/modifier combination, count how often co-occurrence was possible, and how often it actually occurred
        data.frame(
          mod1 = x,
          mod2 = names(xx)[y],
          possible = sum(xx[[y]][c("0", "1")], na.rm = T),
          selected = as.vector((xx[[y]]["1"]))
        )
      })) %>%
        mutate(conditional.probability = .data$selected / .data$possible) %>%
        replace_na(list(conditional.probability = 0)) %>%
        select(-.data$possible, -.data$selected) %>%
        suppressMessages()
      return(xx)
    }))

  # calculate unconditional probabilities and counts, and make pretty
  xtab <- xtab %>%
    filter(remove == 0) %>%
    mutate(unconditional.prob = .data$observed / .data$possible) %>%
    separate(
      .data$combination,
      into = c("mod1", "mod2"),
      sep = ":",
      remove = T
    ) %>%
    left_join(cond.probs, by = c("mod1" = "mod1", "mod2" = "mod2")) %>%
    left_join(cond.probs, by = c("mod1" = "mod2", "mod2" = "mod1")) %>%
    rename(
      "conditionalBgA" = .data$conditional.probability.x,
      "conditionalAgB" = .data$conditional.probability.y
    ) %>%
    replace_na(list(conditionalAgB = 0, conditionalBgA = 0)) %>%
    drop_na(.data$mod1, .data$mod2) %>%
    filter(.data$observed > 0) %>%
    suppressWarnings()


  # calculate mutual information of the two elements
  xtab$mutual.information <- -1 * (log(
    (xtab$unconditional.prob) /
      (xtab$conditionalBgA *
        xtab$conditionalAgB)
  )) /
    (-1 * log(xtab$unconditional.prob))

  xtab$mutual.information <-
    ifelse(is.infinite(xtab$mutual.information),
      NA,
      xtab$mutual.information
    )

  # create wide and long format
  xtab.wide <- xtab

  xtab.long <- data.frame(
    mod1 = c(xtab$mod1, xtab$mod2),
    mod2 = c(xtab$mod2, xtab$mod1),
    possible = c(xtab$possible, xtab$possible),
    observed = c(xtab$observed, xtab$observed),
    unconditional.prob = c(xtab$unconditional.prob, xtab$unconditional.prob),
    conditional.prob = c(xtab$conditionalBgA, xtab$conditionalAgB)
  ) %>%
    drop_na(.data$mod1, .data$mod2) %>%
    filter(.data$observed > 0)

  # find those that are determined in one direction, with theshold set at 0.98
  one_way_determined <- xtab.long %>%
    filter(.data$conditional.prob >= 0.98) %>%
    arrange(.data$mod2) %>%
    select(.data$mod1, .data$mod2) %>%
    rename(
      "modifier" = .data$mod1,
      "determined_by" = .data$mod2
    )

  # find those that are determined in both directions
  two_way_determined <- xtab.wide %>%
    filter(.data$conditionalAgB >= 0.98 &
             .data$conditionalBgA >= 0.98) %>%
    arrange(.data$mod2) %>%
    select(.data$mod1, .data$mod2) %>%
    rename(
      "modifier" = .data$mod1,
      "determined_by" = .data$mod2
    )

  return(
    list(
      symmetrical = xtab.wide,
      long.form = xtab.long,
      one_way_determined = one_way_determined,
      two_way_determined = two_way_determined
    )
  )
}


# helper ------------------------------------------------------------------

## function that quickly calculates all combinations within a vector
compute_possible_combs <- function(mods, max_comb_len) {
  # given a vector of elements, computes all possible unique combinations among them
  lapply(1:min(length(mods), max_comb_len), function(comb_len) {
    apply(
      combinations(x = mods, k = comb_len),
      MARGIN =  1,
      FUN = paste,
      collapse = ":"
    )
  })
}
