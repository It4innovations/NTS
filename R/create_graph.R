#' Create edges from source
#'
#' @description
#' This function creates data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`
#' starting from the "source" vertice. It is an initial function to create a network.
#'
#' @param set integer vector, each number represents a number of vertices in a set
#' @param weight integer vector, \itemize{
#' \item{}{if single number is provided the same weight is set for all the edges,}
#' \item{}{if the number of elements in a vector equals to the `length(set)` then the weight is the same for each set,}
#' \item{}{if the number of elements equals to the `sum(set)` then each edge has unique weight.}}
#' @param name_from string, name of the source vertice. Default is "source".
#' @param name_to string, prefix for the names of vertices on the next level. Each vertice will have number appended to it for the uniqueness of names. Default is "1_".
#'
#' @details
#'
#' ## Weights:
#' Weights are set to the edges in a direction from the left to the right.
#'
#' @return data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @export
#'
#' @examples
#'
#' network <- create_source(set = c(2,1),
#'                          weight = c(5,2))
#' plot_network(network)
#'
#' network2 <- create_source(set = c(2,1),
#'                           weight = 1)
#' plot_network(network2)
#'
#' network3 <- create_source(set = c(2,1),
#'                           weight = c(1,5,10),
#'                           name_from = "entrance",
#'                           name_to = "registration")
#' plot_network(network3)
#'
create_source <- function(set,
                          weight,
                          name_from = "source",
                          name_to = "1_") {

  # Add weights to the edges
  if (length(weight) == 1) {
  } else if (length(weight) == length(set)) {
    weight <- make_weight_list(set,
                               weight)
  } else if (length(weight) != sum(set)) {
   stop("ERROR: Inconsistent number of weights to the edges!")
  }

  # Return created network from source to first level
  df_out <- data.frame(
    from = name_from,
    to = paste0((name_to),(1:sum(set))),
    weight = weight,
    level = 1,
    set = make_set_list(set)
  )
  return(df_out)
}

#' Add fully connected level to an existing network
#'
#' @description
#' The function adds new level of vertices to an existing network. These vertices are fully connected
#' to the vertices of the last level before addition.
#'
#' @param df_edges data.frame of existing network with columns `from`, `to`, `weight`, `level`, `set`.
#' @param set integer vector, each number represents a number of vertices in a set.
#' @param weight integer vector, \itemize{
#' \item{}{if single number is provided the same weight is set for all the edges,}
#' \item{}{if the number of elements in a vector equals to the `length(set)` then the weight is the same for each set,}
#' \item{}{if the number of elements equals to the `sum(set)` then each edge to a given vertice has unique weight,}
#' \item{}{if the number of elements equals to the `n_from*sum(set)` then all edges has unique weight.}}
#' @param name_to string, prefix for the names of vertices on the next level. Each vertice will have number appended to it for the uniqueness of names. Default is "1_".
#'
#' @details
#'
#' ## Weights:
#' Weights are set to the edges in a direction from the left to the right from the side of the newly added vertices.
#'
#' @return data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @export
#'
#' @examples
#'
#'
#' network <- create_source(set = c(3,3), weight = 1) %>%
#'    add_fully_connected(set = 4,
#'                        weight = 3)
#' plot_network(network)
#'
#' network2 <- create_source(set = c(2,3), weight = 1) %>%
#'   add_fully_connected(set = 2,
#'                       weight = c(3,2))
#' plot_network(network2)
#'
#' network3 <- create_source(set = c(1,1), weight = 1) %>%
#'   add_fully_connected(set = 2,
#'                       weight = c(1,2,3,4))
#' plot_network(network3)
#'
#'
#' @importFrom dplyr bind_rows `%>%`
#' @importFrom rlang .data
add_fully_connected <- function(df_edges,
                                set,
                                weight,
                                name_to = "") {
  # Determine the last level in the existing network
  max_level <- max(df_edges$level)
  # pick up number of vertices from the last existing level
  n_from <- length(unique(df_edges$to[df_edges$level == max_level]))
  # prepare list with indexes of sets
  set_ind <- rep(make_set_list(set), each = n_from)

  # if there is number of weights equal to number of sets then each edge will
  # have common weight within given set
  # else each edge will have unique weight
  if (length(weight) == 1) {
  } else if (length(weight) == length(set)) {
    weight <- rep(make_weight_list(set, weight), each = n_from)
  } else if (length(weight) == sum(set)) {
    weight <- rep(weight, each = n_from)
  } else if (length(weight) != n_from*sum(set)) {
    stop("ERROR: Inconsistent number of weights to the edges!")
  }

  # if there is no specified name, the default naming will be used
  if (name_to == "") {
    name_to <- paste0((max_level + 1), "_")
  }

  # add set number to the existing name
  l_to <- rep(paste0((name_to),(1:sum(set))), each = n_from)

  # add full connected level to the existing network
  df_edges <- df_edges %>%
    dplyr::bind_rows(
      data.frame(
        from = df_edges %>%
          filter(.data$level == max_level) %>%
          pull(.data$to) %>%
          unique() %>%
          rep(sum(set)),
        to = l_to,
        weight = weight,
        level = max_level + 1,
        set = set_ind
      )
    )
  return(df_edges)
}

#' Add a new level of vertices to an existing network
#'
#' @description
#' This function adds new data to an existing data.frame with a definition
#'
#' @param df_edges data.frame of existing network with columns `from`, `to`, `weight`, `level`, `set`.
#' @param previous_set integer vector/list, each number represents a number of a set in the last level.
#' @param set integer vector, each number represents a number of vertices in a set.
#' @param weight integer vector/list, \itemize{
#' \item{}{if single number is provided the same weight is set for all the edges}
#' \item{}{if the number of elements in a vector equals to the `length(set)` then the weight is the same for each set}
#' \item{}{if the number of elements equals to the `sum(set)` then each edge has unique weight}
#' }
#' @param name_to string, prefix for the names of vertices on the next level. Each vertice will have number appended to it for the uniqueness of names. Default is "1_".
#' @param check_previous boolean, should connection to all previous sets be checked? FALSE is used for visualization of partial setup. It is recommended to have TRUE in normal usage. Default TRUE.
#'
#' @details
#'
#' ## Weights:
#' If you want to add an unique weight for each edge, please, use vector notation. E.g. if there will be 4 edges from the
#' previous sets, use weight = c(1,2,3,4) for example.
#' Weights are set to the edges in a direction from the left to the right from the side of the newly added vertices.
#'
#' ## weight and previous_set as list:
#' Here is a detailed example how to use lists correctly. This code is from the examples section, see below.
#'
#' add_level_set(list(`1` = c(2,8), `2` = 4),
#'               list(`1` = c(1,3), `2` = 2),
#'               c(   `1` = 2,      `2` = 1))
#'
#' Numbers in ` ` are the number of sets. `1` = c(1,3) in the second line means that we want to map
#' set no.1 and set no.3 in the last level to the 2 vertices of set no. 1 in the new level (third line: `1` = 2)
#' with weight 2 from the last set no.1 to the new set no.1 and weight 8 from the last set no.3 to the new set no.1 (first line: `1` = c(2,8)).
#' Second column `2` is in similar way. We want to map last set no.2 to the new set no.2 (containing one vertice)
#' with weight 4.
#'
#' @return data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @export
#'
#' @examples
#'
#' network <- create_source(set = c(2,2,1),
#'                          weight = c(1,2,1)) %>%
#'   add_level_set(weight = list(c(1,2),4),
#'                 previous_set = list(c(3, 1), 2),
#'                 set = c(1,3))
#' plot_network(network)
#'
#' network2 <- create_source(set = c(2,2),
#'                           weight = c(1,2)) %>%
#'   add_level_set(weight = c(1,2,3,4),
#'                 previous_set = list(2,1),
#'                 set = c(1,1))
#' plot_network(network2)
#'
#' network3 <- create_source(set = c(2,2),
#'                           weight = c(1,2)) %>%
#'   add_level_set(weight = c(1),
#'                 previous_set = list(2,1),
#'                 set = c(1,1))
#' plot_network(network3)
#'
#' @importFrom dplyr bind_rows filter pull `%>%`
#' @importFrom purrr map_int map map2
#' @importFrom rlang .data
add_level_set <- function(df_edges,
                          previous_set,
                          set,
                          weight,
                          name_to = "",
                          check_previous = TRUE) {

  # Determine the last level in the existing network
  max_level <- max(df_edges$level)

  # Pick up sets numbering of the last level in the existing network
  last_level_sets <- df_edges %>%
    dplyr::filter(.data$level == max_level) %>%
    dplyr::pull(.data$set) %>%
    unique()

  # Pick up lengths of the sets in the last level
  # last_level_sets_lengths <- purrr::map_int(unlist(previous_set),
  #                                    operation_over_set_distinct,
  #                                    df_edges,
  #                                    max_level,
  #                                    nrow)

  # Check if all sets of last level are in previous set
  if (check_previous) {
    if (!all(last_level_sets %in% unlist(previous_set))) {
      stop("ERROR: There are missing sets in previous_set!")
    } else if (!all(unlist(previous_set) %in% last_level_sets)) {
      stop("ERROR: There are unknown sets in previous_set!")
    }

    # Previous set and new set must have same length
    if (length(previous_set) != length(set)) {
      stop("ERROR: previous_set and set must have equal length!")
    }
  }

  # If there is no specified prefix for vertices, use the default one
  if (name_to == "") {
    name_to <- paste0((max_level + 1), "_")
  }

  # name_from
  df_edges_max_level <- df_edges %>%
    dplyr::select(.data$level, .data$set, .data$to) %>%
    dplyr::filter(.data$level == max_level) %>%
    dplyr::distinct()

  # list of vertices from
  l_from <- NULL
  for (i in 1:length(previous_set)) {
    if (is.list(previous_set[[i]])) {
      previous_set[[i]] <- unlist(previous_set[[i]])
    }
    for (j in 1:length(previous_set[[i]])) {
      temp <- df_edges_max_level$to[df_edges_max_level$set == previous_set[[i]][j]]
      l_from <- c(l_from, rep(temp, set[i]))
    }
  }

  # Add postfix to name_to
  l_to <- NULL
  counter <- 0
  for (i in 1:length(previous_set)) {
    for (j in 1:length(previous_set[[i]])) {
      ind <- 1:set[i] + counter
      previous_set_count <- sum(df_edges_max_level$set == previous_set[[i]][j])
      l_to <- c(l_to,
                rep(paste0(name_to, ind),
                    each = previous_set_count))
    }
    counter <- counter + set[i]
  }

  # Prepare set indices
  set_ind <- NULL
  for (i in 1:length(previous_set)) {
    for (j in 1:length(previous_set[[i]])) {
      previous_set_count <- sum(df_edges_max_level$set == previous_set[[i]][j])
      set_ind <- c(set_ind,
                   rep(rep(i, set[i]),
                       each = previous_set_count))
    }
  }

  # Check length of weight
  if (length(weight) == 1) {

  } else if (length(weight) == length(set)) {
    weight_temp <- NULL
    for (i in 1:length(previous_set)) {
      for (j in 1:length(previous_set[[i]])) {
        previous_set_count <- sum(df_edges_max_level$set == previous_set[[i]][j])
        if (length(weight[[i]]) == length(previous_set[[i]])) {
          weight_temp <- c(weight_temp,
                           rep(rep(weight[[i]][j], set[i]),
                               each = previous_set_count))
        } else {
          weight_temp <- c(weight_temp,
                           rep(rep(weight[[i]], set[i]),
                               each = previous_set_count))
        }
      }
    }
    weight <- weight_temp
  }

  # Return existing network with added level
  df_edges <- df_edges %>%
    dplyr::bind_rows(
      data.frame(
        from = l_from,
        to = l_to,
        weight = weight,
        level = max_level + 1,
        set = set_ind
      )
    )

  return(df_edges)
}


#' Add an exit vertice to an existing network
#'
#' @description
#' The function adds exit vertice to an existing network with specified edges.
#'
#' @param df_edges data.frame of existing network with columns `from`, `to`, `weight`, `level`, `set`.
#' @param name_to string, name of the exit/sink vertice. Default is "exit".
#'
#' @details
#'
#' ## Weights:
#' Weights are set to the edges in a direction from the left to the right from the side of the newly added vertices.
#'
#' @return data.frame of existing network with columns `from`, `to`, `weight`, `level`, `set`.
#' @export
#'
#' @examples
#'
#' network <- create_source(set = c(2,2),
#'                          weight = c(1)) %>%
#'  add_exit(name_to="LAST VERTICE")
#' plot_network(network)
#'
#' network2 <- create_source(c(2,2),
#'                           c(1,2)) %>%
#'  add_fully_connected(2,
#'                      1) %>%
#'  add_exit()
#' plot_network(network2)
#'
#' @importFrom dplyr filter bind_rows `%>%` pull
#' @importFrom rlang .data
add_exit <- function(df_edges,
                     name_to = "exit"){

  # Pick up last level in the existing network
  max_level <- max(df_edges$level)

  # name_from
  l_from <- df_edges %>%
    dplyr::filter(.data$level == max_level) %>%
    dplyr::pull(.data$to) %>%
    unique

  # return existing network with added exit vertice
  df_out <- df_edges %>%
  dplyr::bind_rows(
    data.frame(
      from = l_from,
      to = name_to,
      weight = 1, # weight is 1 so it is visible on plot, but has no real meaning for the computation
      level = max_level + 1,
      set = 0
    )
  )
  return(df_out)
}
