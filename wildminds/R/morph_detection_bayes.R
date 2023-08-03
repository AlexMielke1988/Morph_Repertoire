#' Takes modifiers for one gesture action, performs Bayesian Latent Class Analysis, and identifies clusters of modifiers
#'
#' LCA is used to find the cluster solution with the highest BIC
#'
#' @param data data frame as extracted from Filemaker
#' @param modifiers a vector with the names of the modifiers to be tested
#' @param gesture_action a vector with the column that the gesture actions are stored in
#' @param plot.action a string of the gesture action of interest
#' @param cutoff minimum number of times a modifier level should occur to be counted
#'
#' @return Function returns cluster plot, information about the possible cluster solutions for individual elements and bigrams and their specificity to that cluster, BIC values for the cluster solutions, plot of the BIC, and the modifier data that were used
#' @return If there are fewer than 3 cases or only one modifier level for the gesture action, there is no information given
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all transmute
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite expand_grid
#' @importFrom plotly plot_ly
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline qplot ylim
#' @importFrom purrr transpose
#' @importFrom stats na.omit
#' @importFrom rpart rpart
#' @importFrom BayesLCA blca.em
#'
#' @export
#'

morph_detection_bayes <- function(data,
                                      modifiers,
                                      gesture_action,
                                      plot.action,
                                      cutoff = 1) {
  # define gesture action column
  gesture.action.all <- data %>%
    pull(gesture_action)

  # select modifier data
  modifier.data.original <- data %>%
    filter(gesture.action.all == plot.action) %>%
    select(all_of(modifiers))

  modifier.data <- modifier.data.original

  # make Flexion and Orientation for back etc
  if ("Body_part_signaller" %in% colnames(modifier.data)) {
    if ("Flexion_elbow" %in% colnames(modifier.data)) {
      modifier.data$Flexion_elbow <-
        ifelse(
          modifier.data$Body_part_signaller %in% c(
            "Back",
            "Body",
            "Bottom",
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
    if ("Flexion_wrist" %in% colnames(modifier.data)) {
      modifier.data$Flexion_wrist <-
        ifelse(
          modifier.data$Body_part_signaller %in% c(
            "Back",
            "Body",
            "Bottom",
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
    if ("Orientation" %in% colnames(modifier.data)) {
      modifier.data$Orientation <-
        ifelse(
          modifier.data$Body_part_signaller %in% c(
            "Back",
            "Body",
            "Bottom",
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
    if ("Laterality" %in% colnames(modifier.data)) {
      modifier.data$Laterality <-
        ifelse(
          modifier.data$Body_part_signaller %in% c(
            "Back",
            "Body",
            "Bottom",
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
  }

  # add modifier name to levels
  modifier.data <- colnames_to_levels(modifier.data)

  # remove modifier levels that occur fewer than cutoff
  xx <- unlist(modifier.data) %>%
    table()
  xx <- xx[xx < cutoff] %>%
    names()
  for (i in 1:ncol(modifier.data)) {
    modifier.data[, i] <-
      ifelse((unlist(modifier.data[, i]) %in% xx),
             str_c(colnames(modifier.data)[i], 'Other', sep = '.'),
             unlist(modifier.data[, i]))
  }


  # remove modifiers that only have one expression in the gesture action
  modifier.remove <- modifier.data %>%
    apply(2, table) %>%
    sapply(length)
  modifier.data <- modifier.data %>%
    rownames_to_column('row.nums') %>%
    select(c(row.nums, names(modifier.remove[modifier.remove > 1]))) %>%
    column_to_rownames('row.nums')


  # if 1 or 0 modifiers have multiple expressions, just let us know that there are no different repertoires
  if (ncol(modifier.data) < 1) {
    return(
      list(
        gesture.action = plot.action,
        plot = NA,
        cluster.info = NA,
        solutions = NA,
        solution.plot = NA,
        full.data = data %>%
          filter(gesture.action.all == plot.action) %>%
          select(all_of(modifiers)) %>%
          data.frame() %>%
          mutate(cluster = 1),
        distinction.info = data.frame(
          nr.clusters = 1,
          nr.clusters.distinct = 1
        )
      )
    )
  }


  modifier.data <- modifier.data %>%
    rownames_to_column('row.nums') %>%
    drop_na() %>%
    column_to_rownames('row.nums')

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
  modifier.remove <-
    lapply(seq_along(modifier.data), function(x)
      modifier.data[, x] %>% table) %>%
    sapply(length)
  names(modifier.remove) = colnames(modifier.data)

  modifier.data <- modifier.data %>%
    rownames_to_column('row.nums') %>%
    select(c(row.nums, names(modifier.remove[modifier.remove > 1]))) %>%
    column_to_rownames('row.nums')

  modifier.data <- modifier.data %>%
    rownames_to_column('row.nums') %>%
    drop_na() %>%
    column_to_rownames('row.nums')

  if (ncol(modifier.data) == 1) {
    if (colnames(modifier.data) == '.') {
      colnames(modifier.data) =
        str_split(modifier.data[1, 1], pattern = '\\.') %>%
        unlist() %>%
        head(1)
    }
  }

  # if 1 or 0 modifiers have multiple expressions, just let us know that there are no different repertoires
  if (ncol(modifier.data) < 1) {
    return(
      list(
        gesture.action = plot.action,
        plot = NA,
        cluster.info = NA,
        solutions = NA,
        solution.plot = NA,
        full.data = data %>%
          filter(gesture.action.all == plot.action) %>%
          select(all_of(modifiers)) %>%
          data.frame() %>%
          mutate(cluster = 1),
        distinction.info = data.frame(
          nr.clusters = 1,
          nr.clusters.distinct = 1
        )
      )
    )
  }
  # naming within step_dummy
  nodash_names <- function(var, lvl, ordinal) {
    dummy_names(
      var = '',
      lvl = lvl,
      ordinal = ordinal,
      sep = ""
    )
  }

  # create dummy coded data from modifier data
  modifier.matr <- recipe( ~ ., data = modifier.data) %>%
    step_dummy(all_nominal_predictors(),
               naming = nodash_names,
               one_hot = TRUE) %>%
    step_naomit(all_numeric()) %>%
    prep() %>%
    bake(new_data = NULL)

  # set number of possible clusters, repeat 5 times
  max.clusters <- apply(modifier.matr, 1,
                        function(x)
                          as.character(paste0(x, collapse = ''))) %>% table()
  max.clusters <- max(5, sum(max.clusters > cutoff) + 1)
  clusters <- rep(1:max.clusters, 10) %>% sort

  # fit for all possible cluster solutions
  fits <- lapply(clusters, function(x) {
    fit1 <- blca.em(modifier.matr,
                    restarts = 200,
                    iter = 5000,
                    G = x,
                    verbose = FALSE,
                    conv = 1e-08,
                    start.vals = 'across') %>%
      suppressWarnings()
  })


  results.fit <- lapply(fits, function(x) {
    return(data.frame(BIC = x$BIC))
  }) %>% bind_rows() %>%
    mutate(clusters = clusters,
           # add minimum cluster size
           min_cluster_size = sapply(fits, function(x) {
             if (x$classprob %>% length() > 1) {
               x$Z <- x$Z
               labels_df <-
                 apply(x$Z, 1, function(y)
                   sample(names(y), 1, prob = y))
               data.frame(labels_df) %>%
                 rownames_to_column('label') %>%
                 left_join(x$counts %>%
                             data.frame() %>%
                             rownames_to_column('label')) %>%
                 group_by(labels_df) %>%
                 summarise(sums = sum(.)) %>%
                 pull(sums) %>%
                 min() %>%
                 suppressMessages()
             }
           }))

  # remove those that have fewer than cutoff values
  results.fit$min_cluster_size[results.fit$clusters == 1] <-
    nrow(modifier.data)
  results.fit$BIC[results.fit$min_cluster_size < cutoff] = NA
  results.fit <- results.fit %>% mutate(rownr = row_number())

  # pull the fit for the best model of those with the selected number of clusters

  distinctions <- sapply(seq_along(results.fit$clusters), function(k) {
    fit.best <- fits[[k]]

    if (length(fit.best$classprob) == 1) {
      matrix.lca <- cbind(modifier.data,
                          .cluster = 1,
                          .rownames = rownames(modifier.data))
    }
    if (length(fit.best$classprob) > 1) {
      if (nrow(fit.best$Z) == nrow(modifier.matr)) {
        grouping <-
          apply(fit.best$Z, 1, function(x)
            sample(names(x), 1, prob = x)) %>%
          str_remove('Group ')
      }

      if (nrow(fit.best$Z) != nrow(modifier.matr)) {
        labels_df <-
          apply(fit.best$Z, 1, function(x)
            sample(names(x), 1, prob = x))
        row_sequence <- apply(modifier.matr, 1,
                              function(x)
                                as.character(paste0(x, collapse = '')))
        grouping <- labels_df[row_sequence] %>% str_remove('Group ')
      }

      matrix.lca <-
        cbind(
          modifier.data,
          .cluster = as.numeric(grouping),
          .rownames = rownames(modifier.data)
        )
    }
    ### create information for elements and bigrams, how specific are they to each cluster

    # save modifier data
    # modifier.data <- modifier.data %>%
    #   filter(gesture.action == plot.action)

    # create list that contains all observed modifier levels per event

    # for (i in 1:ncol(modifier.data)) {
    #   modifier.data[str_detect(modifier.data[, i], '.NV'), i] = NA
    # }
    #

    xx.list <- lapply(1:nrow(modifier.data), function(y) {
      modifier.data[y, ] %>%
        unlist(use.names = FALSE) %>%
        na.omit() %>%
        unlist(use.names = FALSE) %>%
        sort() %>%
        as.character()
    })

    # create list that contains all possible modifier levels per event
    xx.list.non.na <- lapply(1:nrow(modifier.data), function(y) {
      colnames(modifier.matr)[!is.na(modifier.matr[y, ])] %>%
        sort() %>%
        as.character()
    })


    ## make list for all clusters that contain all possible combinations occurring in the cluster, how often they occurred, and how specific they were to that cluster
    cluster_info <-
      lapply(unique(matrix.lca$.cluster), function(x) {
        xx.cluster <-
          probability_of_combination(xx.list[matrix.lca$.cluster == x], maxlen = ncol(modifier.data))
        xx.possible <-
          probability_of_combination(xx.list.non.na[matrix.lca$.cluster == x], maxlen = ncol(modifier.data))
        xx.all <-
          probability_of_combination(xx.list, maxlen = ncol(modifier.data))

        #### figure out which one is probability and which one is specificity!
        xx.combinations <- xx.cluster %>%
          left_join(xx.possible %>% dplyr::select(combination, count), by = 'combination') %>%
          mutate(probability = count.x / count.y) %>%
          dplyr::select(-count.y) %>%
          left_join(xx.all %>% dplyr::select(combination, count), by = 'combination') %>%
          mutate(specificity = count.x / count) %>%
          dplyr::select(-count) %>%
          mutate(count.cluster = count.x) %>%
          mutate(modifier = combination) %>%
          dplyr::select(modifier,
                        count.cluster,
                        probability,
                        specificity,
                        nr.rules) %>%
          mutate(cluster = x,
                 gesture_action = plot.action)

        return(xx.combinations)
      })

    names(cluster_info) <- unique(matrix.lca$.cluster)

    summary_morphs <- cluster_info %>%
      bind_rows() %>%
      filter(probability == 1 & specificity == 1) %>%
      distinct(cluster, .keep_all = T) %>%
      pull(cluster) %>%
      unlist(F, F) %>%
      as.numeric() %>%
      unique() %>%
      sort()

    if(length(summary_morphs) < length(unique(matrix.lca$.cluster))){
      xx.additional <-
        additional_rules(cluster_info, summary_morphs) %>%
        pull(cluster) %>%
        unique()
      summary_morphs <- sort(c(summary_morphs, xx.additional))
    }


    distinction_info <-
      data.frame(
        nr.clusters = matrix.lca$.cluster %>% as.numeric() %>% max(),
        nr.clusters.distinct = length(summary_morphs)
      )

    return(distinction_info$nr.clusters - distinction_info$nr.clusters.distinct)
  })

  results.fit$distinctions <- distinctions
  if(0 %in% distinctions|1 %in% distinctions){
    fit.best <- fits[[results.fit %>%
                        filter(distinctions %in% c(0,1)) %>%
                        arrange(desc(BIC)) %>%
                        head(1) %>%
                        pull(rownr)]]
  }
  if(!(0 %in% distinctions|1 %in% distinctions)){
    fit.best <- fits[[results.fit$BIC %>% which.max()]]
  }

  # pull the fit for the best model of those with the selected number of clusters

  if (length(fit.best$classprob) == 1) {
    matrix.lca <- cbind(modifier.data,
                        .cluster = 1,
                        .rownames = rownames(modifier.data))
  }
  if (length(fit.best$classprob) > 1) {

    if(nrow(fit.best$Z) == nrow(modifier.matr)){
      grouping <-
        apply(fit.best$Z, 1, function(x)
          sample(names(x), 1, prob = x)) %>%
        str_remove('Group ')
    }

    if(nrow(fit.best$Z) != nrow(modifier.matr)){
      labels_df <-
        apply(fit.best$Z, 1, function(x)
          sample(names(x), 1, prob = x))
      row_sequence <- apply(modifier.matr, 1,
                            function(x)
                              as.character(paste0(x, collapse = '')))
      grouping <- labels_df[row_sequence] %>% str_remove('Group ')
    }

    matrix.lca <-
      cbind(modifier.data,
            .cluster = as.numeric(grouping),
            .rownames = rownames(modifier.data))

  }
  ### create information for elements and bigrams, how specific are they to each cluster

  # save modifier data
  # modifier.data <- modifier.data %>%
  #   filter(gesture.action == plot.action)

  # create list that contains all observed modifier levels per event

  # for (i in 1:ncol(modifier.data)) {
  #   modifier.data[str_detect(modifier.data[, i], '.NV'), i] = NA
  # }


  xx.list <- lapply(1:nrow(modifier.data), function(y) {
    modifier.data[y,] %>%
      unlist(use.names = FALSE) %>%
      na.omit() %>%
      unlist(use.names = FALSE) %>%
      sort() %>%
      as.character()
  })

  # create list that contains all possible modifier levels per event
  xx.list.non.na <- lapply(1:nrow(modifier.data), function(y) {
    colnames(modifier.matr)[!is.na(modifier.matr[y,])] %>%
      sort() %>%
      as.character()
  })


  ## make list for all clusters that contain all possible combinations occurring in the cluster, how often they occurred, and how specific they were to that cluster
  cluster_info <-
    lapply(unique(matrix.lca$.cluster), function(x) {
      xx.cluster <-
        probability_of_combination(xx.list[matrix.lca$.cluster == x], maxlen = ncol(modifier.data))
      xx.possible <-
        probability_of_combination(xx.list.non.na[matrix.lca$.cluster == x], maxlen = ncol(modifier.data))
      xx.all <-
        probability_of_combination(xx.list, maxlen = ncol(modifier.data))

      #### figure out which one is probability and which one is specificity!
      xx.combinations <- xx.cluster %>%
        left_join(xx.possible %>%
                    dplyr::select(combination, count), by = 'combination') %>%
        mutate(probability = count.x / count.y) %>%
        dplyr::select(-count.y) %>%
        left_join(xx.all %>%
                    dplyr::select(combination, count), by = 'combination') %>%
        mutate(specificity = count.x / count) %>%
        dplyr::select(-count) %>%
        mutate(count.cluster = count.x) %>%
        mutate(modifier = combination) %>%
        dplyr::select(modifier,
                      count.cluster,
                      probability,
                      specificity,
                      nr.rules) %>%
        mutate(cluster = x,
               gesture_action = plot.action)

      return(xx.combinations)
    })

  names(cluster_info) <- unique(matrix.lca$.cluster)

  summary_morphs <- cluster_info %>%
    bind_rows() %>%
    filter(probability == 1 & specificity == 1) %>%
    distinct(cluster, .keep_all = T) %>%
    pull(cluster) %>%
    unlist(F, F) %>%
    as.numeric() %>%
    unique() %>%
    sort()

  if(length(summary_morphs) < length(unique(matrix.lca$.cluster))){
    xx.additional <-
      additional_rules(cluster_info, summary_morphs) %>%
      pull(cluster) %>%
      unique()
    summary_morphs <- sort(c(summary_morphs, xx.additional))
  }


  distinction_info <-
    data.frame(
      nr.clusters = matrix.lca$.cluster %>% as.numeric() %>% max(),
      nr.clusters.distinct = length(summary_morphs)
    )

  full.data <- modifier.data.original %>%
    rownames_to_column('row.nums') %>%
    mutate(row.nums = as.numeric(row.nums)) %>%
    left_join(cbind(
      matrix.lca %>%
        data.frame() %>%
        select(.rownames, .cluster) %>%
        mutate(rownums = as.numeric(.rownames),
               cluster = as.numeric(.cluster))
    ),
    by = c('row.nums' = 'rownums')) %>%
    select(modifiers, cluster) %>%
    suppressMessages()


  cluster_info <- cluster_info %>%
    bind_rows()

  # BICs values
  p2 <- qplot(
    x = results.fit$clusters,
    y =  results.fit$BIC,
    xlab = "Clusters",
    ylab = "BIC",
    main = "Cluster Solutions - BIC"
  ) +
    geom_hline(mapping = aes(yintercept = max(results.fit$BIC)), linetype = 2) +
    theme_classic()



  if (max(matrix.lca$.cluster, na.rm = T) > 1) {
    p3 <- plot_bipartite(
      prob.table = cluster_info %>%
        filter(nr.rules == 1),
      select.modifier = "cluster",
      plot.title = plot.action,
      cutoff = cutoff,
      threshold = 0,
      remove.full = FALSE
    )
  }

  if (max(matrix.lca$.cluster, na.rm = T) > 1) {
    # repeat data to make it easier to cluster
    aa <- bind_rows(
      cbind(modifier.data,
            cluster = as.factor(matrix.lca$.cluster)),
      cbind(modifier.data,
            cluster = as.factor(matrix.lca$.cluster)),
      cbind(modifier.data,
            cluster = as.factor(matrix.lca$.cluster)),
      cbind(modifier.data,
            cluster = as.factor(matrix.lca$.cluster)),
      cbind(modifier.data,
            cluster = as.factor(matrix.lca$.cluster))
    )

    multi.class.model <- rpart(cluster ~ .,
                               data = aa)

    # rpart.plot(
    #   multi.class.model,
    #   type = 4,
    #   fallen.leaves = FALSE,
    #   cex = 0.5,
    #   extra = 0,
    #   box.palette = "Greys"
    # ) %>%
    #   suppressWarnings()
    # p4 <- recordPlot()
    var_importance <- multi.class.model$variable.importance / 5
  }
  if (max(matrix.lca$.cluster, na.rm = T) <= 1) {
    multi.class.model <- NA
    var_importance <- NA
    p3 <- NA
  }


  return(
    list(
      gesture.action = plot.action,
      network.plot = p3,
      tree = multi.class.model,
      tree.var.importance = var_importance,
      cluster.info = cluster_info,
      solutions = results.fit,
      solution.plot = p2,
      distinction.info = distinction_info,
      full.data = full.data,
      results.fit = results.fit
    )
  )
}



probability_of_combination <- function(elements, maxlen) {
  # calculate all combinations per observation
  combs <- unlist(lapply(elements, function(x) {
    possible_combinations(x, maxlen)
  }))

  # count how many times each AU combination occurred
  n.combs <- Table(combs)
  observed.prob <- n.combs / length(elements)

  # put results in a data frame
  data.frame(
    combination = names(observed.prob),
    observed.prob = observed.prob,
    count = n.combs,
    row.names = NULL,
    nr.rules = str_count(names(observed.prob), ':') + 1
  ) %>%
    arrange(nr.rules)
}


possible_combinations <- function(elements, maxlen) {
  unlist(lapply(1:min(length(elements), maxlen),
                function(comb_len) {
                  apply(
                    arrangements::combinations(x = elements, k = comb_len),
                    MARGIN = 1,
                    FUN = paste,
                    collapse = ":"
                  )
                }))
}





additional_rules <- function(cluster_info, summary_morphs) {
  additional.rules <-
    lapply(setdiff(cluster_info %>% bind_rows %>% pull(cluster),
                   summary_morphs), function(x) {
      # select gesture action and cluster number

      add_rules <-
        lapply(1:max(cluster_info %>%
                       bind_rows() %>%
                       filter(cluster == x) %>%
                       pull(nr.rules) %>%
                       max()), function(m){

                         cs <- cluster_info %>%
                           bind_rows() %>%
                           filter(nr.rules == m &
                                    cluster == x &
                                    specificity == 1) %>%
                           separate(modifier,
                                    into = str_c('m',
                                                 1:m,
                                                 sep = ''),
                                    sep = '\\:')

                         if(nrow(cs) == 0){
                           return(cluster_info %>%
                                    bind_rows() %>%
                                    filter(cluster == 1.5))
                         }

                         if(m == 1){
                           cs_mod <- cs %>%
                             separate(m1,
                                      into = c('mod', 'lev'),
                                      sep = '\\.',
                                      remove = FALSE)
                           cs_agg <- aggregate(cs_mod$probability,
                                               by = list(cs_mod$mod), sum) %>%
                             filter(x == 1)
                           if(nrow(cs_agg > 0)){
                             cs_agg <- cs_mod %>%
                               left_join(cs_agg, by = c('mod' = 'Group.1')) %>%
                               suppressMessages() %>%
                               filter(x == 1) %>%
                               select(-mod, -lev, -x) %>%
                               unite(modifier, str_c('m',
                                                     1:m,
                                                     sep = ''), sep = ':')
                           }
                           comb_probs <- cs_agg
                         }

                         if(m > 1){
                           combs <- expand.grid(
                             rep(list(str_c('m',
                                            1:m,
                                            sep = '')
                             ), m)) %>%
                             distinct() %>%
                             data.frame() %>%
                             select(- c(m))

                           combs <- combs[
                             sapply(1:nrow(combs), function(k) (table(combs[k,] %>%
                                                                        unlist()) %>%
                                                                  max()) == 1),]
                           if(m > 2){
                             combs <- sapply(1:nrow(combs),
                                             function(k)
                                               str_c(sort(combs[k,] %>% unlist()), collapse = '_')
                             ) %>%
                               unique()
                           }
                           if(m == 2){
                             combs <- sapply(1:length(combs),
                                             function(k)
                                               str_c(sort(combs[k] %>% unlist()), collapse = '_')
                             ) %>%
                               unique()
                           }

                           comb_probs <- lapply(combs, function(k){
                             cols <- str_split(k, '_') %>% unlist()
                             cs_mod <- cs %>%
                               separate(setdiff(str_c('m',
                                                      1:m,
                                                      sep = ''),
                                                cols),
                                        into = c('mod', 'lev'),
                                        sep = '\\.',
                                        remove = FALSE)
                             cs_agg <- aggregate(cs_mod$probability,
                                                 by = cs_mod[,c(cols, 'mod')], sum) %>%
                               filter(x == 1)

                             cs_agg <- cs_mod %>%
                               left_join(cs_agg) %>%
                               suppressMessages() %>%
                               filter(x == 1) %>%
                               select(-mod, -lev, -x) %>%
                               unite(modifier, str_c('m',
                                                     1:m,
                                                     sep = ''), sep = ':')

                             return(cs_agg)
                           }) %>%
                             bind_rows() %>%
                             distinct()
                         }

                         return(comb_probs)
                       }) %>% bind_rows()

      if(nrow(add_rules) > 0){
        add_rules <- add_rules %>%
          filter(nr.rules == min(add_rules$nr.rules))
      }

      return(add_rules)
    })  %>% bind_rows()
  return(additional.rules)
}

