#' Simulate flow through network
#'
#' @param df_edges data.frame with network defined by edges. It should contain columns `from`, `to`, `weight`, `level`.
#' @param random numeric, volatility of the time spend at vertice. When entering a vertex a time defined by the edge weights is multiplied by the `max(1, rnorm(1, 0, random))`.
#' @param input_people integer, how many people should enter into the simulation.
#' @param l_events list, list of events that should affect the state of the simulation. See details section for more information.
#' @param timestep integer, what should be the timestep for the simulation. If set to NA a greatest common divisor of the edges weight is computed and used. Default NA.
#' @param df_vertices data.frame, with description of vertices get by the with columns `vertice`, `level`, `x`, `y`. It is an optional parameter since usually this is computed from `df_edges`.
#' @param source_name string, name of the source vertice
#' @param exit_name string, name of the exit vertice
#' @param flow_text_side string, side on which side the flow text should be visualized. One of "left" or "right". Default is "left".
#' @param flow_text_distance numeric, how far the text should be from the edge of the network. Distance is a multiple of the network width. Default is 0.12.
#'
#' @return list containing two data.frames
#' \itemize{
#'    \item{simulation}{data.frame of the simulation with timesteps identified by the column `snap`}
#'    \item{flow}{data.frame with the flow for each timestep identified by the column `snap`}
#' }
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
#' df_simulation <- res[[1]]
#' animate_simulation(network,
#'                    df_simulation)
#'
#' @importFrom dplyr mutate case_when rename bind_rows summarize n pull group_by
#' @importFrom purrr map_df map_dbl
#' @importFrom stats rnorm
#' @importFrom rlang .data
simulate_flow <- function(df_edges,
                          random = 0,
                          input_people = 100,
                          l_events = NULL,
                          timestep = NA,
                          df_vertices = NULL,
                          source_name = "source",
                          exit_name = "exit",
                          flow_text_side = "left",
                          flow_text_distance = 0.12) {
  # Set variables ----
  df_vertices <- get_coord(df_edges)
  df_vertices$snap <- 0
  df_vertices$remaining_time <- 0
  df_vertices$occupied <- df_vertices$vertice == source_name
  df_vertices$vertex_color <- as.numeric(df_vertices$vertice == source_name)
  df_vertices$state <- "Empty"
  df_vertices$state[df_vertices$vertice == source_name] <- "Full"

  if (is.na(timestep)) {
    timestep <- do.call(gcd, as.list(unique(df_edges$weight)))
  }

  # Handle events ----
  if ("list" %in% class(l_events)) {
    events <- purrr::map_dbl(l_events,
                             .f = ~ .x$snap) %>%
      unique()

    df_events <- purrr::map_df(l_events,
                                .f = ~ as.data.frame(.x) %>%
                                  dplyr::mutate(state = as.character(state)))
  } else if (is.null(l_events)) {
    events <- NULL
  } else {
    warning("l_events class not recognized. Events will not be used.")
    events <- NULL
  }

  # Prepare variables for simulation ----
  l_res <- list()
  l_flow <- list()
  counter <- 1
  people_out <- 0
  people_in <- input_people
  ind_exit <- which(df_vertices$vertice == exit_name)
  ind_source <- which(df_vertices$vertice == source_name)

  df_flow <- data.frame(
    snap = 0,
    level = unique(df_vertices$level),
    people_out = 0
  )

  # Save 0th step state ----
  l_res[[counter]] <- df_vertices
  l_flow[[counter]] <- df_flow
  counter <- counter + 1
  current_timestep <- 0 + timestep

  # Compute simulation ----
  while (people_out < input_people) {
    # Check events ----
    if (current_timestep %in% events) {
      ind <- which(df_events$snap == current_timestep)
      for (i in ind) {
        if (df_events$table[i] == "vertice") {
          ind_vertice <- df_vertices$vertice == df_events$vertices[i]
          df_vertices[ind_vertice,
                    df_events$column[i]] <- df_events$value[i]
          if (!is.na(df_events$vertex_color[i])) {
            df_vertices[ind_vertice,
                      "vertex_color"] <- df_events$vertex_color[i]
          }
          if (!is.na(df_events$state[i])) {
            df_vertices[ind_vertice,
                      "state"] <- as.character(df_events$state[i])
          }
        } else if (df_events$table[i] == "edge") {

          if (!is.na(df_events$from[i]) | !is.na(df_events$to[i])) {
            ind_edges <- rep(TRUE, nrow(df_edges))
            if (!is.na(df_events$from[i])) {
              ind_edges <- ind_edges & (df_edges$from == df_events$from[i])
            }

            if (!is.na(df_events$to[i])) {
              ind_edges <- ind_edges & (df_edges$to == df_events$to[i])
            }
            df_edges[ind_edges,
                        df_events$column[i]] <- df_events$value[i]
          } else {
            warning("Selected edge table, but no from and to defined.")
          }
        } else {
          warning("Unknown table selected in events. Please choose from vertice or edge when creating events.")
        }
      }
    }
    # Update to new timestep ----
    # Update data.frames to new timestep

    df_vertices$snap <- df_vertices$snap + timestep
    df_vertices$remaining_time <- pmax(0, df_vertices$remaining_time - timestep)

    df_flow$snap <- df_flow$snap + timestep
    df_flow$people_out <- 0

    # Find full vertices with 0 remaining time
    full_vertices <-
      df_vertices$vertice[df_vertices$occupied == TRUE &
                          df_vertices$remaining_time == 0]
    # Find empty vertices
    empty_vertices <-
      df_vertices$vertice[df_vertices$occupied == FALSE &
                          df_vertices$remaining_time == 0]

    # Iterate over full vertices ----
    for (vertice in rev(full_vertices)) {
      next_vertices <- df_edges$to[df_edges$from == vertice]
      fill_vertices <- empty_vertices[empty_vertices %in% next_vertices]

      if (length(fill_vertices) != 0) {
        # Handle source vertex
        if (vertice == source_name) {
          # This variable makes sure there are no more people in the system, than given by input_people
          length_fill <- min(people_in, length(fill_vertices))

          # Get indeces of empty vertices connected to source in vertex definition and edges definition data.frames
          ind <- which(df_vertices$vertice %in% fill_vertices[1:length_fill])
          ind_edges <- which(df_edges$from %in% vertice &
                               df_edges$to %in% fill_vertices[1:length_fill])
          # Fill empty vertices
          df_vertices$remaining_time[ind] <-
            df_edges$weight[ind_edges]
          df_vertices$occupied[ind] <- TRUE
          df_vertices$vertex_color[ind] <- 1
          df_vertices$state[ind] <- "Full"
          empty_vertices <-
            c(empty_vertices[!(empty_vertices %in% fill_vertices)], vertice)
          # Update people in network
          people_in <- people_in - length(ind)
          df_flow$people_out[1] <- length(ind)
          if (people_in <= 0) {
            df_vertices$occupied[ind_source] <- FALSE
            df_vertices$vertex_color[ind_source] <- 0
            df_vertices$state[ind] <- "Empty"
          }
        } else {
          # Handle general vertices

          # Get indeces of the first empty vertices connected to given vertex in vertex definition and edges definition data.frames
          ind <- which(df_vertices$vertice == fill_vertices[1])
          ind2 <- which(df_vertices$vertice == vertice)
          ind_edges <- which(df_edges$from %in% vertice &
                               df_edges$to %in% fill_vertices[1])
          # Fill empty vertex
          df_vertices$remaining_time[ind] <-
            df_edges$weight[ind_edges] * max(1, stats::rnorm(1, 0, random))
          df_vertices$occupied[ind] <- TRUE
          df_vertices$vertex_color[ind] <- 1
          df_vertices$state[ind] <- "Full"
          df_vertices$occupied[ind2] <- FALSE
          df_vertices$vertex_color[ind2] <- 0
          df_vertices$state[ind2] <- "Empty"
          empty_vertices <-
            c(empty_vertices[!(empty_vertices == fill_vertices[1])], vertice)
          ind_flow <-
            df_flow$level == df_vertices$level[df_vertices$vertice == vertice]

          # Update people in network
          df_flow$people_out[ind_flow] <-
            df_flow$people_out[ind_flow] + 1

          # Handle exit vertex
          if (fill_vertices[1] == exit_name) {
            people_out <- people_out + 1
            empty_vertices <- c(empty_vertices, exit_name)
            df_vertices$occupied[ind_exit] <- FALSE
            df_vertices$vertex_color[ind_exit] <- 0
            df_vertices$state[ind_exit] <- "Empty"
          }
        }
      }
    }
    # Store state of current timestep into lists ----
    l_flow[[counter]] <- df_flow
    l_res[[counter]] <- df_vertices
    counter <- counter + 1
    current_timestep <- current_timestep + timestep
  }

  # Final data cleanup ----
  # Expand results into data.frames
  df_simulation <- do.call(rbind, l_res)
  df_flow <- do.call(rbind, l_flow)

  # Add x and y coordinates to df_flow
  max_distance <- df_vertices %>%
    dplyr::group_by(.data$level) %>%
    dplyr::summarize(n = dplyr::n(),
                     .groups = "drop") %>%
    dplyr::pull(n) %>% max

  x <- get_x(df_simulation,
             side = flow_text_side,
             distance = max_distance * flow_text_distance)
  y <- get_y(df_simulation)

  names(df_flow)[names(df_flow) == "people_out"] <- "text"
  df_flow <- df_flow %>%
    flow_coord(x,y) %>%
    filter(.data$level != max(.data$level))

  res <- list(simulation = df_simulation,
              flow = df_flow)

  class(res) <- c(class(res), "CVCS_simulation")

  return(res)
}

#' Add coordinates to flow
#'
#' @param df_flow data.frame with flow generated in `simulate_flow`
#' @param df_x data.frame created by function `get_x`
#' @param df_y data.frame created by function `get_y`
#'
#' @returns data.frame ready for the `animate_simulation` df_text parameter.
#'
#' @importFrom dplyr left_join
flow_coord <- function(df_flow,
                       df_x,
                       df_y){
  res <- df_flow %>%
    dplyr::left_join(df_x, by = "level") %>%
    dplyr::left_join(df_y, by = "level")

  return(res)
}

#' Get y coordinates for levels
#'
#' @param df_simulation data.frame created in the `simulate_flow` function.
#'
#' @returns data.frame with columns `level` and `y`.
#'
#' @importFrom dplyr select distinct
#' @importFrom rlang .data
get_y <- function(df_simulation){
  res <- df_simulation %>%
    dplyr::select(.data$level,
                  .data$y) %>%
    dplyr::distinct()

  return(res)
}

#' Get x coordinates
#'
#' @param df_simulation data.frame created in the `simulate_flow` function.
#' @param x numeric vector, it is possible to provide x coordinates manually.
#' @param side string, it should be either 'left' or 'right' values to get coordinates to the left or right of the plot.
#' @param distance numeric, the distance from the plot.
#'
#' @returns data.frame with columns `level` and `x`.
#'
#' @importFrom dplyr mutate distinct
#' @importFrom rlang .data
get_x <- function(df_simulation,
                  x = NULL,
                  side = "left",
                  distance = 1.05){

  if (is.null(x)) {
    if (side == "left") {
      x <- min(df_simulation$x) - distance
    } else if (side == "right") {
      x <- max(df_simulation$x) + distance
    } else {
      stop("ERROR: side should have value 'left', 'right'")
    }
  }

  res <- df_simulation %>%
    dplyr::distinct(.data$level) %>%
    dplyr::mutate(x = x)

  return(res)
}


#' Find greatest common divisor
#'
#' @description This function finds the greatest common divisor.
#' The solution was taken from \url{https://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r#21504113}.
#' Thanks very much to Matthew Lundberg and Moody_Mudskipper.
#'
#' @param ... list of integers
#'
#' @noRd

gcd <- function(...){
  Reduce(function(x,y)
    ifelse(y, Recall(y, x %% y), x), list(...))
}
