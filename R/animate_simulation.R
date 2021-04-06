#' Animate simulation
#'
#' @param df_edges data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @param input a `CVCS_simulation` object get from a \code{\link{simulate_flow}} or directly data.frame with columns `snap`, `level`, `x`, `y`, `text`, `remaining_time`.
#' @param speed numeric, the speed of the animation in miliseconds.
#' @param graph_title string, graph title.
#' @param df_text data.frame, data.frame containing columns `x`, `y`, `text` and `snap`. It will put `text` at `[x, y]` coordinates at time `snap`. Also see \code{\link{add_text}} and \code{\link{add_level_text}}. This is not used if input is a `CVCS_simulation` object. In that case it will use text from that object.
#' @param plotly_annotations list, a plotly annotation may be used for static text. See more in \code{\link[plotly]{add_annotations}}
#' @param edge_shape_color string, a color name used for edge shapes. Plotly names or rgba notation should be used.
#' @param edge_shape_scale numeric, a number which will multiple the edge shapes size. Might be useful if the weights are really high, or small.
#' @param exit_name string, a name of exit vertice.
#' @param timestep numeric, a timestep in which should the simulation progress.
#' @param snap numeric vector, this variable can be used to put the text only in certain timesteps (snaps).
#'
#' @return plotly animation
#' @export
#'
#' @examples
#'
#' network <- create_source(set = c(2,2), weight = 2) %>%
#'  add_level_set(weight = c(3,1),
#'                previous_set = c(1,2),
#'                set = c(1,3)) %>%
#'   add_fully_connected(weight = 3,
#'                       set = 1) %>%
#'   add_exit()
#'
#' res <- simulate_flow(network)
#'
#' animate_simulation(network,
#'                    res,
#'                    snap = 0:10)
#'
#' @importFrom plotly plot_ly add_markers add_text layout animation_opts config
#' @importFrom dplyr mutate case_when
#' @importFrom rlang .data
animate_simulation <- function(df_edges,
                               input,
                               df_text = NULL,
                               timestep = 1,
                               speed = 200,
                               graph_title = "<b>Network flow simulation</b>",
                               plotly_annotations = NULL,
                               edge_shape_color = "rgba(0,100,200,0.4)",
                               edge_shape_scale = 1,
                               exit_name = "exit",
                               snap = NULL) {

  # Check input class
  if ("CVCS_simulation" %in% class(input)) {
    df_simulation <- input[[1]]
    df_text <- input[[2]]
  } else {
    df_simulation <- input
  }

  df_vertices <- get_coord(df_edges)

  # Prepare axes definition
  axis_x <- list(
    title = "",
    showgrid = FALSE,
    showticklabels = FALSE,
    zeroline = FALSE
  )

  axis_y <- list(
    title = "",
    showgrid = FALSE,
    showticklabels = FALSE,
    zeroline = FALSE
  )

  if (!is.null(snap)) {
    if (is.numeric(snap)) {
      df_simulation <- df_simulation[df_simulation$snap %in% snap, ]

      df_text <- df_text[df_text$snap %in% snap, ]
    } else {
      stop("`snap` should be a numeric value or NULL!")
    }
  }

  timesteps <- seq(min(df_simulation$snap),
                   max(df_simulation$snap),
                   by = timestep)

  df_simulation <-
    df_simulation[df_simulation$snap %in% timesteps,]

  df_text <-
    df_text[df_text$snap %in% timesteps,]

  # Compute edges ----
  l_edge_shapes <- get_edge_list(df_edges,
                                 df_vertices,
                                 edge_shape_color,
                                 edge_shape_scale)

  p <- plotly::plot_ly()

  df_simulation <- df_simulation %>%
    dplyr::mutate(
      color = dplyr::case_when(
        .data$vertex_color == 0 ~ "rgba(0,204,204,1)",
        .data$vertex_color == 1 ~ "rgba(153,0,0,1)",
        .data$vertex_color == 2 ~ "rgba(255,179,25,1)"
      )
    )

  # colfunc <- colorRampPalette(c("blue", "red"))
  # pal <- colfunc(max(df_simulation$remaining_time))[df_simulation$remaining_time]

  p <- p %>%
    plotly::add_markers(
      data = df_simulation,
      x = ~ x,
      y = ~ y,
      frame = ~ snap,
      text = ~ paste0("<b>Vertice: ", vertice, "\nState: ", state, "\nTime: ", remaining_time,"</b>"),
      # hoverinfo = "text",
      hovertemplate = paste0("%{text}",
                             "<extra></extra>"),
      marker = list(size = 10,
                    color = ~ color)
    )

  if (!is.null(df_text)) {
    p <- p %>%
      plotly::add_text(data = df_text ,
                       x = ~ x,
                       y = ~ y,
                       text = ~ text,
                       frame = ~ snap,
                       hoverinfo = "text",
                       inherit = FALSE)
  }

  p <- p %>% plotly::layout(
      title = graph_title,
      xaxis = axis_x,
      yaxis = axis_y,
      shapes = l_edge_shapes,
      showlegend = FALSE,
      annotations =  plotly_annotations
    ) %>%
    plotly::animation_opts(speed,
                           transition = 0) %>%
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
