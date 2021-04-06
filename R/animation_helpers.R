#' Add text to df_text at level
#'
#' Add text to the text object for simulation at each level.
#'
#' @param input a `CVCS_simulation` object get from a \code{\link{simulate_flow}} or directly df_text with columns `snap`, `level`, `x`, `y`, `text`.
#' @param df_new_text data.frame, with columns `level` and `text`, `level` should reference to levels and `y` coordinate is computed based on this.
#' @param x_coord numeric, this is the `x` coordinate.
#' @param snap numeric vector, this variable can be used to put the text only in certain timesteps (snaps).
#'
#' @return an updated `CVCS_simulation` object in case of `CVCS_simulation` object input and updated data.frame in case of data.frame.
#' @export
#'
#' @examples
#'
#' df_network <- create_source(set = c(1,1),
#'                              weight = 3) %>%
#'  add_fully_connected(weight = c(1,1,2,2),
#'                      set = c(10,1,1,1)) %>%
#'  add_level_set(weight = list(c(2,1),
#'                              4,
#'                              6),
#'                previous_set = list(c(1,2),
#'                                    3,
#'                                    c(3,4)),
#'                set = c(2, 3, 1)) %>%
#'  add_exit()
#'
#' res <- simulate_flow(df_network)
#'
#' net_levels <- get_network_levels(df_network)
#' net_text <- 1:length(net_levels)
#'
#' new_text <- data.frame(level = net_levels,
#'                        text = net_text)
#'
#' res <- add_level_text(res,
#'                       new_text,
#'                       x_coord = 6.8)
#'
#' animate_simulation(df_network,
#'                    res[[1]],
#'                    res[[2]],
#'                    snap = 0)
#'
#' @importFrom dplyr select left_join
#' @importFrom rlang .data
add_level_text <- function(input,
                           df_new_text,
                           x_coord = 0,
                           snap = NULL) {

  # Check input class
  if ("CVCS_simulation" %in% class(input)) {
    df_temp <- input[[2]]
  } else {
    df_temp <- input
  }

  # Subset snaps
  if (!is.null(snap)) {
    if ( is.numeric(snap) ) {
      df_temp <- df_temp %>%
        filter(.data$snap %in% snap)
    } else {
      stop("Snap should be numeric or NULL")
    }
  }

  # Add coordinates to df_new_text from df_temp and add df_temp at the end
  df_temp <- df_temp %>%
    dplyr::select(.data$snap, .data$level, .data$y) %>%
    dplyr::left_join(df_new_text) %>%
    mutate(x = x_coord,
           text = as.character(.data$text)) %>%
    dplyr::bind_rows(df_temp %>%
                       mutate(text = as.character(.data$text)))

  # If the input was "CVC_simulation" put the text into it, otherwise return data.frame
  if ("CVCS_simulation" %in% class(input)) {
    input[[2]] <- df_temp
    return(input)
  } else {
    return(df_temp)
  }
}

#' Add text to df_text
#'
#' Add text to the text object for simulation.
#'
#' @param input a `CVCS_simulation` object get from a \code{\link{simulate_flow}} or directly df_text with columns `snap`, `level`, `x`, `y`, `text`.
#' @param df_new_text data.frame, with columns `x`, `y`, `text`.
#' @param snap numeric vector, this variable can be used to put the text only in certain timesteps (snaps).
#'
#' @return an updated `CVCS_simulation` object in case of `CVCS_simulation` object input and updated data.frame in case of data.frame.
#' @export
#'
#' @examples
#'
#' df_network <- create_source(set = c(1,1),
#'                              weight = 3) %>%
#'  add_fully_connected(weight = c(1,1,2,2),
#'                      set = c(10,1,1,1)) %>%
#'  add_level_set(weight = list(c(2,1),
#'                              4,
#'                              6),
#'                previous_set = list(c(1,2),
#'                                    3,
#'                                    c(3,4)),
#'                set = c(2, 3, 1)) %>%
#'  add_exit()
#'
#' res <- simulate_flow(df_network)
#'
#' net_coord <- get_network_levels_coord(df_network)
#' net_text <- 1:length(net_coord)
#'
#' new_text <- data.frame(x = 6.8,
#'                        y = net_coord,
#'                        text = net_text)
#'
#' res <- add_text(res,
#'                 new_text)
#'
#' animate_simulation(df_network,
#'                    res[[1]],
#'                    res[[2]],
#'                    snap = 0)
#'
#' @importFrom dplyr select left_join
#' @importFrom rlang .data
add_text <- function(input,
                     df_new_text,
                     snap = NULL) {

  # Check input class
  if ("CVCS_simulation" %in% class(input)) {
    df_temp <- input[[2]]
  } else {
    df_temp <- input
  }

  # Subset snaps
  if (is.null(snap)) {
    snap <- unique(df_temp$snap)
  } else if ( is.numeric(snap) ) {
    print("snap OK")
  } else {
    stop("Snap should be numeric or NULL")
  }

  # Add coordinates to df_new_text from df_temp and add df_temp at the end
  df_temp <- purrr::map_dfr(snap,
                            function(x, y){
                              y$snap <- x
                              return(y)
                            },
                            df_new_text) %>%
    dplyr::mutate(text = as.character(.data$text)) %>%
    dplyr::bind_rows(df_temp %>%
                       dplyr::mutate(text = as.character(.data$text)))

  # If the input was "CVC_simulation" put the text into it, otherwise return data.frame
  if ("CVCS_simulation" %in% class(input)) {
    input[[2]] <- df_temp
    return(input)
  } else {
    return(df_temp)
  }
}

#' Get levels
#'
#' Get levels from network edge definition.
#'
#' @param df_network data.frame with network defined by edges. It should contain columns `from`, `to`, `weight`, `level`.
#' @param df_vertices data.frame, with description of vertices get by the with columns `vertice`, `level`, `x`, `y`. It is an optional parameter since usually this is computed from `df_network`.
#'
#' @return vector with levels of the network
#' @export
#'
#' @examples
#' df_network <- create_source(set = c(1,1),
#'                              3) %>%
#'  add_exit()
#'
#' res <- simulate_flow(df_network)
#'
#' net_levels <- get_network_levels(df_network)
#'
get_network_levels <- function(df_network,
                               df_vertices = NULL){
  # Get vertices
  if (is.null(df_vertices)) {
    df_vertices <- get_coord(df_network)
  }
  # Get levels
  network_levels <- unique(df_vertices$level)

  return(network_levels)
}

#' Get levels coord
#'
#' Get levels coord from network edge definition.
#'
#' @param df_network data.frame with network defined by edges. It should contain columns `from`, `to`, `weight`, `level`.
#' @param df_vertices data.frame, with description of vertices get by the with columns `vertice`, `level`, `x`, `y`. It is an optional parameter since usually this is computed from `df_network`.
#'
#' @return vector with levels of the network
#' @export
#'
#' @examples
#' df_network <- create_source(set = c(1,1),
#'                              3) %>%
#'  add_exit()
#'
#' res <- simulate_flow(df_network)
#'
#' net_levels <- get_network_levels_coord(df_network)
#'
get_network_levels_coord <- function(df_network,
                                     df_vertices = NULL){
  # Get vertices
  if (is.null(df_vertices)) {
    df_vertices <- get_coord(df_network)
  }
  # Get levels
  network_levels <- unique(df_vertices$y)

  return(network_levels)
}
