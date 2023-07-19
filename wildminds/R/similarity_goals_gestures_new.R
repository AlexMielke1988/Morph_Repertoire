#' Calculate and plot the similarity of Gestures or Contexts based on the respective other column
#'
#' Uses two approaches: UMAP to reduce dimensions combined with K-Means clustering; Dendrograms; and Correspondence Analysis using FactoMineR
#'
#' @param gesture_action vector with the gesture actions
#' @param goal vector with the meaning variable
#' @param parameter parameter to be plotted; either 'Goal' (plots the distribution of goals) or 'Gesture' (plots the distribution of gestures)
#' @param trials number of times the cluster detection algorithm should be performed a robust solution
#' @param n_epochs how many iterations for the UMAP dimension reduction?
#' @param n_neighbours sets the n_neighbors argument in UMAP - the smaller, the more fine-grained the clustering algorithm solution is
#'
#' @return plots of the K-Means cluster solution with UMAP dimension reduction, dendrogram, and correspondence analysis. Data for dendrogram, cluster solutions, and collocation analysis
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline position_jitter
#' @importFrom reshape2 acast
#' @importFrom dendextend get_subdendrograms hang.dendrogram set ladderize
#' @importFrom umap umap umap.defaults
#' @importFrom mclust Mclust mclustBIC
#' @importFrom ClusterR Optimal_Clusters_KMeans KMeans_rcpp
#' @importFrom FactoMineR CA
#' @importFrom factoextra fviz_ca_row
#' @importFrom stats as.dendrogram order.dendrogram
#'
#' @export
#'


similarity_goals_gestures_new <- function(gesture_action,
                                          goal,
                                          parameter = "Goal",
                                          trials = 20,
                                          n_epochs = 5000,
                                          n_neighbours = 4) {
  # remove unclear
  rem <- gesture_action %in% c('Unknown', NA, 'Unclear', 'Other') |
    goal %in% c('Unknown', NA, 'Unclear', 'Other')

  gesture_action <- gesture_action[!rem]
  goal <- goal[!rem]

  # create matrix of Goal by Gesture from object
  if (parameter == "Goal") {
    goal_matrix <- data.frame(Goal = goal,
                              Gesture_record = gesture_action) %>%
      group_by(Goal, Gesture_record) %>%
      summarise(Observed.sum = n()) %>%
      ungroup %>%
      acast(
        Goal ~ Gesture_record,
        fun.aggregate = sum,
        fill = 0,
        value.var = "Observed.sum"
      )
  }
  # create matrix of Gesture by Goal from object
  if (parameter == "Gesture") {
    goal_matrix <- data.frame(Goal = goal,
                              Gesture_record = gesture_action) %>%
      group_by(Goal, Gesture_record) %>%
      summarise(Observed.sum = n()) %>%
      ungroup %>%
      acast(
        Gesture_record ~ Goal,
        fun.aggregate = sum,
        fill = 0,
        value.var = "Observed.sum"
      )
  }

  custom.config <- umap.defaults # set configuration to default
  custom.config$n_neighbors <- n_neighbours

  boot_clus <- lapply(1:trials, function(k) {
    custom.config$n_epochs <-
      sample(size = 1,
             rnorm(10,
                   mean = n_epochs,
                   sd = n_epochs / 10)) # number of epochs for the UMAP algorithm
    # if there are fewer than 3 modifier levels, set number of components to 2
    custom.config$n_components <- 2

    matrix.umap <- umap(
      goal_matrix,
      random.state = 123,
      method = 'naive',
      config = custom.config
    )


    max.clus <- min(nrow(matrix.umap$layout) - 1, 25)
    opt.1 <- lapply(3:max.clus, function(x) {
      xx <- data.frame(
        clusters = x,
        silhouette = cluster::pam(matrix.umap$layout %>%
                                    as.matrix(), x) %>%
          cluster::silhouette(full = TRUE) %>%
          data.frame() %>%
          select(sil_width) %>%
          unlist() %>%
          mean(na.rm = T)
      )
      return(xx)
    }) %>%
      bind_rows() %>%
      select(silhouette) %>%
      unlist(F, F)
    opt.1 <- c(NA, NA, opt.1)

    return(which.max(opt.1))
  })


  k.1 <- names(boot_clus %>%
                 unlist() %>%
                 table)[boot_clus %>%
                          unlist() %>%
                          table %>%
                          which.max] %>%
    as.numeric()

  custom.config$n_components <- 2

  matrix.umap <- umap(
    goal_matrix,
    random.state = 123,
    method = 'naive',
    config = custom.config
  )

  sil.1 <- cluster::pam(matrix.umap$layout %>%
                          as.matrix(), k.1) %>%
    cluster::silhouette(full = TRUE) %>%
    data.frame() %>%
    select(sil_width) %>%
    unlist() %>%
    mean(na.rm = T)

  # if no silhouette is better than 0.3, select 1 as best cluster
  if (sil.1 < 0.3) {
    k.1 <- 1
  }

  if (k.1 == 0) {
    k.1 <- 1
  }
  # use K-Means clustering to assign clusters in the data
  umap.clust.1 <-
    cluster::pam(matrix.umap$layout %>%
                   as.matrix(), k.1)
  # add cluster membership to data
  matrix.1.umap.frame <- data.frame(matrix.umap$layout,
                                    cluster = as.factor(umap.clust.1$clustering))
  matrix.1.umap.frame$element <- rownames(matrix.1.umap.frame)


  p1 <-
    ggplot(
      matrix.1.umap.frame,
      aes(
        x = .data$X1,
        y = .data$X2,
        label = .data$element,
        color = .data$cluster
      )
    ) +
    geom_text(
      show.legend = F,
      position = position_jitter(width = 0.2, height = 0.2),
      fontface = 'bold',
      size = 3
    ) +
    ggtitle(paste(
      c(
        "Similarity; Best cluster solution: ",
        sil.1,
        "; number of clusters: ",
        k.1
      ),
      collapse = ""
    )) +
    theme_classic() +
    xlab("First Dimension") +
    ylab("Second Dimension")


  # make dendrogram using euclidean distances
  dend <- matrix.umap$layout %>%
    dist(method = "euclidean") %>%
    hclust(method = "average") %>%
    as.dendrogram() %>%
    set("branches_k_color", k = k.1) %>%
    hang.dendrogram(hang = 0.2) %>%
    ladderize() %>%
    set("labels_cex", c(.65))

  # find sub-dendrograms based on same cluster number as before
  xx <- get_subdendrograms(dend,
                           k = k.1,
                           order_clusters_as_data = TRUE)

  dend_cluster <- bind_rows(lapply(1:length(xx), function(x) {
    data.frame(
      element = as.matrix(goal_matrix[order.dendrogram(xx[[x]]),-5]) %>%
        rownames() %>% unlist(),
      cluster = x
    )
  })) %>%
    left_join(p1$data,
              by = c("element" = "element")) %>%
    select(-.data$X1,-.data$X2) %>%
    arrange(.data$cluster.y)
  colnames(dend_cluster) <-
    c("element", "dendrogram.cluster", "k.means.cluster")


  # perform correspondence analysis using FactoMineR package
  res.ca <- CA(goal_matrix, graph = FALSE)
  # plot results using factoextra package
  p2 <- fviz_ca_row(res.ca,
                    repel = TRUE,
                    col.row = "gray20",
                    max.overlaps = 10)

  dissimilarity <- (goal_matrix / rowSums(goal_matrix)) %>%
    dist(method = 'euclidean', diag = TRUE) %>%
    as.matrix()

  dissimilarity <- 1 - scale(dissimilarity,
                             center = FALSE,
                             scale = apply(dissimilarity, 2, max))
  diag(dissimilarity) = 0
  # most.similar <- data.frame(element = colnames(dissimilarity),
  #                            most.similar = colnames(dissimilarity)[dissimilarity %>%
  #                                                                     data.frame() %>%
  #                                                                     apply(1, which.max)])

  most.similar <-
    dissimilarity %>%
    melt() %>%
    #filter(value > 0.75) %>%
    group_by(Var1) %>%
    top_n(1, wt = value) %>%
    ungroup %>%
    arrange(Var1) %>%
    select(-value)

  most.similar.clusters <- most.similar %>%
    as.matrix() %>%
    graph_from_edgelist() %>%
    walktrap.community()

  most.similar.net <- most.similar %>%
    as.matrix() %>%
    graph_from_edgelist() %>%
    ggraph(layout = 'fr') +
    geom_node_text(
      mapping = aes(
        label = .data$name,
        color = rainbow(length(
          unique(most.similar.clusters$membership)
        ))[most.similar.clusters$membership],
        size = 20
      ),
      show.legend = FALSE
    ) +
    scale_edge_alpha(guide = "none") +
    theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
    ggtitle('Network Most Similar Each') +
    # make edges, labels, and arrows
    geom_edge_fan(
      label_size = 4,
      arrow = NULL,
      colour = "grey",
      fontface = "bold",
      label_dodge = unit(2, "mm"),
      angle_calc = "along",
      show.legend = F
    ) +
    geom_node_label(
      mapping = aes(
        label = .data$name,
        color = rainbow(length(
          unique(most.similar.clusters$membership)
        ))[most.similar.clusters$membership],
        size = 20,
      ),
      show.legend = FALSE
    )


  return(
    list(
      plot.cluster = p1,
      plot.dendro = ggplot(dend, horiz = TRUE),
      plot.collocations = p2,
      umap.dendro = dend,
      cluster.solutions = dend_cluster,
      collocation.analysis = res.ca,
      most.similar = most.similar,
      most.similar.net = most.similar.net
    )
  )
}
