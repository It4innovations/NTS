#' Plot network from data.frame. Plotting is based on plotly.
#'
#' @description
#' Plot network from data.frame. Thickness of the edges depends on weight value. Plotting is based on plotly package.
#'
#' @param df_edges data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @param graph_title string containing the title of the plot. Default is "Network".
#' @param marker_list list with elements defining size, shape, etc. of the marker in the plot.
#' @param edge_shape_color string of RGBA color for the lines. Default is `rgba(0,100,200,0.4)`.
#' @param edge_shape_scale integer describing scale (thickness) of the lines with the smallest weight. Other edges with different weights are proportionally scaled. Default is 1.
#' @param edge_shape_size_normalization boolean, TRUE means that the size of the edges is normalize so the maximal size is equivalent to vertice size. This will override `edge_shape_scale` parameter. Default TRUE.
#' @param df_vertices data.frame with user defined coordinates `level`, `vertice`, `set`, `x`, `y` for each vertice. Default is NULL. As a example of such data_frame, you could apply get_coord(network) to your network.
#' @param color formula, formula for a variable which should be used for coloring. Example `~as.character(set)`. Formula should NOT use `"` enclosure.
#' @param colors string, name of a color palette used in plot.
#'
#' @details
#' ## Parameter `color`:
#' Color scheme could be based on variable from `df_edges` such as `set`, `level`, etc.
#'
#' ## Parameter `colors`:
#' Contains name of a color palette as string. There are available following palettes: "Set1", "Set2", "Set3", "Pastel1", "Pastel2", "Paired", "Dark2", and "BrBG".
#'
#' @return plotly structure
#' @export
#'
#' @examples
#'
#' network <- create_source(c(2,2),c(1,2)) %>%
#'  add_exit()
#'
#' ##plot network with default options and title: THIS IS NETWORK PLOT.
#' plot_network(network,
#'              graph_title = "THIS IS NETWORK PLOT")
#'
#' ## plot network with specified color of lines and marker styles.
#' plot_network(network,
#'              marker_list = list(size = 20,
#'                                 color = "green"),
#'              edge_shape_color = "rgba(255,0,0,0.6)")
#'
#' ## plot vertices with color based on level
#' plot_network(df_edges = network,
#'              color = ~as.character(level))
#'
#' ## plot vertices with different color pallete
#' plot_network(df_edges = network,
#'              colors = "Paired")
#'
#' @importFrom plotly plot_ly config layout add_trace
plot_network <- function(df_edges,
                         graph_title = "Network",
                         marker_list = list(size = 10),
                         edge_shape_color = "rgba(0,100,200,0.4)",
                         edge_shape_scale = 1,
                         edge_shape_size_normalization = TRUE,
                         df_vertices = NULL,
                         color = ~paste0(level, "_" ,set),
                         colors = rep( c('#e6194b',
                                         '#3cb44b',
                                         '#ffe119',
                                         '#4363d8',
                                         '#f58231',
                                         '#911eb4',
                                         '#46f0f0',
                                         '#f032e6',
                                         '#bcf60c',
                                         '#008080',
                                         '#e6beff',
                                         '#9a6324',
                                         '#800000',
                                         '#808000',
                                         '#ffd8b1',
                                         '#000075',
                                         '#fabebe',
                                         '#808080',
                                         '#fffac8',
                                         '#000000'), rep = 10)) {
  # Set variables ----
  if (is.null(df_vertices)) {
    df_vertices <- get_coord(df_edges)
  }

  # Remove axes ----
  axis_x <- list(title = "",
                 showgrid = FALSE,
                 showticklabels = FALSE,
                 zeroline = FALSE)

  axis_y <- list(title = "",
                 showgrid = FALSE,
                 showticklabels = FALSE,
                 zeroline = FALSE
                 )

  # Initialize plot ----
  p <- plotly::plot_ly()

  # Compute edges ----
  l_edge_shapes <- get_edge_list(df_edges,
                                 df_vertices,
                                 edge_shape_color,
                                 edge_shape_scale,
                                 edge_shape_size_normalization)

  # Make unique color set
  vert_vec <- sort(paste0(df_vertices$level, "_", df_vertices$set))
  uniq_vert_vec <- unique(vert_vec)
  colorky_futuristicne <- c('#e6194b', '#3cb44b', '#ffe119', '#4363d8', '#f58231',
                            '#911eb4', '#46f0f0', '#f032e6', '#bcf60c', '#008080',
                            '#e6beff', '#9a6324', '#800000', '#808000', '#ffd8b1',
                            '#000075', '#fabebe', '#808080', '#fffac8', '#000000')
  color_labels <- c()
  col_idx_overflow = 0
  for(i in c(1:length(uniq_vert_vec))) {
    idx = i - col_idx_overflow
    color_labels[i] <- colorky_futuristicne[idx]
    if ( i == length(colorky_futuristicne)) {
      col_idx_overflow = col_idx_overflow + length(colorky_futuristicne)
    }
  }
  color <- factor(vert_vec, labels = color_labels)
  marker_list <- list(size = 10, color = color)


  # Add trace and style
p <- p %>%
    plotly::add_trace(
      data = df_vertices,
      x = ~ x,
      y = ~ y,
      text = ~paste0("Name: ", vertice,
                     "\nLevel-set: ", level, "_", set),
      hoverinfo = 'text',
      marker = marker_list,
      type = "scatter",
      mode = "markers"
      #color = color
      #colors = colors
    ) %>%
  plotly::layout(title = graph_title,
                 xaxis = axis_x,
                 yaxis = axis_y,
                 shapes = l_edge_shapes,
                 showlegend = FALSE) %>%
    plotly::config(
      modeBarButtonsToRemove = c(
        "pan2d",
        "select2d",
        "lasso2d",
        "resetScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian",
        "toggleSpikelines",
        "zoom2d"
      )
    )
  return(p)
}

#' Get vertices coords
#'
#' @description
#' The function return data.frame with defined columns `level`,`vertice`,`set`,`x`,`y`. Needed e.g. in plot_network() function.
#'
#' @param df_edges date.frame, data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#'
#' @return data.frame with specified `level`,`vertice`,`set`,`x`,`y`.
#' @export
#'
#' @examples
#'
#'
#' network <- create_source(set = c(2,2),
#'                          weight = c(1,2)) %>%
#'    add_exit()
#'
#' get_coord(df_edges = network)
#'
#'
#' @importFrom dplyr select distinct bind_rows mutate left_join group_by group_modify ungroup `%>%`
#' @importFrom rlang .data
get_coord <- function(df_edges) {

  df_coord <- df_edges %>%
    dplyr::select(vertice = .data$to,
                  .data$level,
                  .data$set) %>%
    dplyr::distinct() %>%
    dplyr::bind_rows(data.frame(vertice = df_edges$from[1],
                                    level = 0,
                                    set = 0)) %>%
    dplyr::group_by(.data$level) %>%
    dplyr::group_modify(
      .f = function(x, y) {
        vertice_count <- nrow(x)
        if (vertice_count == 1) {
            vertice_count <- 0
        }
        x %>%
          dplyr::mutate(x = seq(-vertice_count,
                                vertice_count,
                                length.out = max(1, vertice_count)),
                        y = -y[[1]])
      }
    ) %>%
    dplyr::ungroup()
  return(df_coord)
}

#' Get list of edges
#'
#' @description
#' The function return edges which is needed in plot_network() function.
#'
#' @param df_edges date.frame, data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @param df_vertices data.frame with user defined coordinates `level`, `vertice`, `set`, `x`, `y` for each vertice. As a example of such data.frame, you could apply get_coord(network) to your network.
#' @param edge_shape_color string of RGBA color for the lines, e.g. edge_shape_color = "rgba(0,0,255,0.5)" for blue color with opacity 0.5.
#' @param edge_shape_scale integer describing scale (thickness) of the lines with the smallest weight. Other edges with different weights are proportionally scaled. Default is 1.
#' @param edge_shape_size_normalization boolean, TRUE means that the size of the edges is normalize so the maximal size is equivalent to vertice size. This will override `edge_shape_scale` parameter. Default TRUE.
#'
#' @return list of edges with specified scale and color.
#' @export
#'
#' @examples
#'
#'
#' network <- create_source(set = c(2,2),
#'                          weight = c(1,2)) %>%
#'   add_exit()
#'
#' get_edge_list(df_edges = network,
#'               df_vertices = get_coord(df_edges = network),
#'               edge_shape_color = "rgba(0,255,0,1.)")
#'
#'
get_edge_list <- function(df_edges,
                          df_vertices,
                          edge_shape_color,
                          edge_shape_scale = 1,
                          edge_shape_size_normalization = TRUE){

  if (edge_shape_size_normalization) {
    df_edges$weight <- df_edges$weight / max(df_edges$weight) * 10
  } else {
    df_edges$weight <- df_edges$weight * edge_shape_scale
  }

  l_edge_shapes <- list()
  for (i in 1:nrow(df_edges)) {
    v0 <- df_edges$from[i]
    v1 <- df_edges$to[i]

    edge_shape = list(
      type = "line",
      layer = "below",
      line = list(color = edge_shape_color,
                  width = df_edges$weight[i]),
      x0 = df_vertices$x[df_vertices$vertice == v0],
      y0 = df_vertices$y[df_vertices$vertice == v0],
      x1 = df_vertices$x[df_vertices$vertice == v1],
      y1 = df_vertices$y[df_vertices$vertice == v1]
    )

    l_edge_shapes[[i]] <- edge_shape
  }

  return(l_edge_shapes)
}


#' check whether the network is correct
#'
#' @description
#' The function checks whether the created network has one "source" and "exit" and everything is connected.
#'
#' @param df_edges date.frame, data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @param detailed bool, if TRUE, the functiont returns list with detailed results.
#'
#' @return In case of mismatch, returns TRUE.
#'
#' @examples
#'
#' network <- create_source(set = c(2,2),
#'                          weight = c(1,2)) %>%
#'   add_exit()
#'
#' check_network(network, detailed = TRUE)
#'
#' @export
check_network <- function(df_edges,
                          detailed = FALSE){
  error_source_only <- FALSE
  error_source <- FALSE
  error_exit_only <- FALSE
  error_exit <- FALSE
  error_connectivity <- FALSE
  error_levels_used <- FALSE

  # check whether is only "source" the first in the df_edges
  var <- unique(df_edges$from[which(df_edges$level == 1)])
  if (length(var) != 1) {
    error_source_only <- TRUE
  }
  if (sum(var != "source") == 1) {
    error_source <- TRUE
  }

  # check whether is only "exit" the first in the df_edges
  var <- unique(df_edges$to[which(df_edges$level == max(df_edges$level))])
  if (length(var) != 1) {
    error_exit_only <- TRUE
  }
  if (sum(var != "exit") == 1) {
    error_exit <- TRUE
  }

  # check whether all edges are connected
  var <- unique(c(df_edges$from, df_edges$to))
  var <- var[!(var %in% c("source", "exit"))] # exclude "source" and "exit" from vector

  # is everything connect
  if ((sum(!(var %in% df_edges$from)) + sum(!(var %in% df_edges$to))) > 0) {
    error_connectivity <- TRUE
  }

  # are every levels used?
  var <- diff(sort(unique(df_edges$level)))
  if ((max(var) != 1) | (min(var) != 1)) {
    error_levels_used <- TRUE
  }

  if (detailed) {
    errors <- vector(mode = 'list')
    errors$error_source_only <- error_source_only
    errors$error_source <- error_source
    errors$error_exit_only <- error_exit_only
    errors$error_exit <- error_exit
    errors$error_connectivity <- error_connectivity
    errors$error_levels_used <- error_levels_used
    return(errors)
  } else {
    return(max(error_source_only + error_source + error_exit_only + error_exit + error_connectivity + error_levels_used))
  }
}

