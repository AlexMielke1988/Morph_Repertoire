#' Plots the overlap of multiple gesture actions and modifiers as bipartite network
#'
#' The function takes the probability table and selected modifiers and plots how different elements
#' modifiers connect the gesture actions, based on the conditional probabilities that the
#' modifier occurs in the gesture and that the gesture is seen when the
#' modifier is present
#'
#' @param prob.table data frame as produced by the probability_table() function
#' @param select.modifier a vector with the names of the modifiers to be tested; if 'All', it selects all modifiers established in probability.table(); if 'cluster', it plots the outcome of gesture.action.umap()
#' @param plot.title title of the resulting network plot
#' @param cutoff minimum number of occurrences for which modifier or Gesture should be included
#' @param threshold minimum conditional probability from which modifier or Gesture should be included
#' @param remove.full remove all cases where a gesture only exibits one modifier level, and only keep ambiguous ones
#'
#' @return network plot of bipartite network containing modifiers and gestures; or modifiers within cluster solutions
#' @return for networks of cluster solutions, plot only modifier levels that have specificity higher than randomly expected
#'
#' @importFrom dplyr mutate arrange desc as_tibble select all_of na_if left_join bind_rows bind_cols rename mutate_all
#' @importFrom stringr str_detect str_split str_replace_all str_to_title str_to_lower
#' @importFrom magrittr %>%
#' @importFrom tidyr replace_na gather separate unite
#' @importFrom ggplot2 ggplot geom_label aes ggtitle xlab ylab theme_classic annotate geom_hline ggtitle aes theme ggtitle xlab ylab theme_classic annotate geom_hline xlim geom_density unit
#' @importFrom ggraph ggraph create_layout geom_node_text scale_edge_alpha theme_graph geom_edge_fan geom_node_label geom_edge_fan theme_graph scale_edge_alpha
#' @importFrom igraph vertex.attributes vertex.attributes<- add_vertices V edge.attributes edge.attributes<- graph_from_data_frame graph.adjacency delete_edges add_vertices get.data.frame bipartite_mapping cluster_fast_greedy modularity
#'
#' @export
#'

plot_bipartite <- function(prob.table,
                           select.modifier,
                           plot.title = NULL,
                           cutoff = 1,
                           threshold = 0,
                           remove.full = FALSE) {
  # Plot only selected modifiers --------------------------------------------

  if (!(select.modifier %in% c("all", "cluster"))) {
    # if remove.full is set to TRUE, remove unambiguous modifiers
    if (remove.full) {
      prob.table <-
        prob.table %>%
        filter(.data$probability < 1)
    }
    # select specified modifiers, create graph object
    net.graph <- graph_from_data_frame(
      prob.table %>%
        filter(.data$modifier %in% select.modifier &
          .data$count >= cutoff) %>%
        unite(mod_level, .data$modifier, .data$level, sep = ".") %>%
        select(
          .data$gesture_action,
          .data$mod_level, .data$probability
        ),
      directed = T,
      vertices = NULL
    ) # create graph

    V(net.graph)$type <-
      bipartite_mapping(net.graph)$type # assign bipartite type as either condition or element
    V(net.graph)$color <-
      ifelse(V(net.graph)$type, "salmon", "lightblue") # color set if there are no clusters
    V(net.graph)$shape <-
      ifelse(V(net.graph)$type, "bold", "italic")

    all.layout <-
      create_layout(net.graph, layout = "igraph", algorithm = "fr") # create basic layout that all the graphs will share, so they are symmetrical

    p.occurrence <- ggraph(all.layout) +
      geom_node_text(
        mapping = aes(
          color = .data$color,
          label = .data$name,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      ) +
      scale_edge_alpha(guide = "none") +
      theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
      ggtitle(plot.title) +
      # make edges, labels, and arrows
      geom_edge_fan(
        mapping = aes(
          label = round(.data$probability, 2),
          colour = .data$type
        ),
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
          color = .data$color,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      )
    return(p.occurrence)
  }

  # Plot all modifiers ------------------------------------------------------

  if (select.modifier == "all") {
    # if remove.full is set to TRUE, remove unambiguous modifiers
    if (remove.full) {
      prob.table <-
        prob.table %>%
        filter(.data$probability < 1)
    }
    # create graph object
    net.graph <- graph_from_data_frame(
      prob.table %>%
        filter(.data$count >= cutoff) %>%
        distinct(.data$gesture_action, .data$modifier) %>%
        select(
          .data$gesture_action,
          .data$modifier
        ),
      directed = F,
      vertices = NULL
    ) # create graph
    V(net.graph)$type <-
      bipartite_mapping(net.graph)$type # assign bipartite type as either condition or element
    # missing.nodes <- setdiff(prob.table$gesture_action, V(net.graph)$name)
    # net.graph <- add_vertices(net.graph,
    #                           length(missing.nodes),
    #                           attr = list(name = missing.nodes, type = FALSE)
    # )

    V(net.graph)$color <-
      ifelse(V(net.graph)$type, "salmon", "lightblue") # color set if there are no clusters
    V(net.graph)$shape <-
      ifelse(V(net.graph)$type, "bold", "italic")

    all.layout <-
      create_layout(net.graph, layout = "igraph", algorithm = "fr") # create basic layout that all the graphs will share, so they are symmetrical

    p.occurrence <- ggraph(all.layout) +
      geom_node_text(
        mapping = aes(
          color = .data$color,
          label = .data$name,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      ) +
      scale_edge_alpha(guide = "none") +
      theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
      ggtitle(plot.title) +
      # make edges, labels, and arrows
      geom_edge_fan(
        mapping = aes(colour = .data$type),
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
          color = .data$color,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      )
    return(p.occurrence)
  }

  # Plot cluster solution from UMAP cluster function ------------------------

  if (select.modifier == "cluster") {
    # adapt dataset to have same look
    remove.mod <- prob.table %>%
      mutate(count = .data$count.cluster * .data$probability) %>%
      filter(.data$count > cutoff) %>%
      group_by(.data$modifier) %>%
      mutate(count = n()) %>%
      summarise(
        mean.prob = mean(.data$probability, na.rm = T),
        count = mean(.data$count, na.rm = T)
      ) %>%
      filter(.data$count < length(unique(prob.table$cluster))) %>%
      select(.data$modifier) %>%
      unlist(use.names = FALSE)

    # create graph object
    net.graph <- graph_from_data_frame(
      prob.table %>%
        mutate(
          count = .data$count.cluster * .data$probability,
          cluster.prob = .data$count.cluster / prob.table %>%
            distinct(cluster,
                     .keep_all = TRUE) %>%
            select(count.cluster) %>%
            sum()
        ) %>%
        filter(
          .data$modifier %in% remove.mod &
            .data$count >= cutoff &
            .data$specificity > .data$cluster.prob &
            .data$probability >= threshold
        ) %>%
        select(
          .data$cluster,
          .data$modifier,
          .data$probability
        ),
      directed = F,
      vertices = NULL
    ) # create graph
    V(net.graph)$type <-
      bipartite_mapping(net.graph)$type # assign bipartite type as either condition or element
    # missing.nodes <- setdiff(prob.table$gesture_action, V(net.graph)$name)
    # net.graph <- add_vertices(net.graph,
    #                           length(missing.nodes),
    #                           attr = list(name = missing.nodes, type = FALSE)
    # )
    
    V(net.graph)$color <-
      ifelse(V(net.graph)$type, "salmon", "lightblue") # color set if there are no clusters
    V(net.graph)$shape <-
      ifelse(V(net.graph)$type, "bold", "italic")
    
    all.layout <-
      create_layout(net.graph, layout = "igraph", algorithm = "fr") # create basic layout that all the graphs will share, so they are symmetrical
    
    p.occurrence <- ggraph(all.layout) +
      geom_node_text(
        mapping = aes(
          color = .data$color,
          label = .data$name,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      ) +
      scale_edge_alpha(guide = "none") +
      theme_graph(base_family = "sans") + # if this is removed, there is bizarrely a constant message telling us that the font does not exist
      ggtitle(plot.title) +
      # make edges, labels, and arrows
      geom_edge_fan(
        mapping = aes(
          label = round(.data$probability, 2),
          colour = .data$type
        ),
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
          color = .data$color,
          size = 50,
          fontface = .data$shape
        ),
        show.legend = FALSE
      )
    return(p.occurrence)
  }
}
