#' Expand set list
#'
#' @param set numeric vector with set definition.
#' @param last_level_sets_lengths numeric vector with the number of vertices of previous sets.
#'
make_set_list <- function(set,
                          last_level_sets_lengths = NULL) {
  set_list <- NULL
  if (is.null(last_level_sets_lengths)) {
    for (i in seq_along(set)) {
      set_list <- c(set_list, rep(i, set[i]))
    }
  } else {
    for (i in seq_along(set)) {
      set_list <- c(set_list, rep(i, set[i] * last_level_sets_lengths[i]))
    }
  }
  return(set_list)
}

#' Expand weights
#'
#' This function prepares
#'
#' @param set numeric vector with set definition.
#' @param weight numeric vector with a weight definition.
#' @param last_level_sets_lengths numeric vector with the number of vertices of previous sets.
#'
make_weight_list <- function(set,
                             weight,
                             last_level_sets_lengths = NULL) {
  set_list <- NULL
  if (is.null(last_level_sets_lengths)) {
    for (i in seq_along(set)) {
      set_list <- c(set_list, rep(weight[i], set[i]))
    }
  } else {
    for (i in seq_along(set)) {
      set_list <- c(set_list, rep(weight[i], set[i] * last_level_sets_lengths[i]))
    }
  }
  return(set_list)
}

#' Do operation over level of input data.frame
#'
#' This function subset the vertices from the max level of the network and then do a operation on them defined by `fnc`.
#'
#' @param current_set numeric, set id which should be worked on
#' @param df_edges data.frame, edge description of the network
#' @param max_level numeric, level which should be subsetted
#' @param fnc function, this is a function which should be made over the subset.
#' @param ... additional arguments for the `fnc`
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
operation_over_set <- function(current_set,
                               df_edges,
                               max_level,
                               fnc,
                               ...) {

  res <- df_edges %>%
    dplyr::filter(.data$level == max_level,
                  .data$set == current_set) %>%
    fnc(...)
  return(res)
}

#' Do operation over distinct subset of input data.frame
#'
#' This function subset the vertices from the max level with distinct `to` column of the network and then do a operation on them defined by `fnc`.
#'
#' @param current_set numeric, set id which should be worked on
#' @param df_edges data.frame, edge description of the network
#' @param max_level numeric, level which should be used for subset
#' @param fnc function, this is a function which should be made over the subset.
#' @param ... additional arguments for the `fnc`
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
operation_over_set_distinct <- function(current_set,
                               df_edges,
                               max_level,
                               fnc,
                               ...) {
  res <- df_edges %>%
    dplyr::filter(.data$level == max_level,
                  .data$set == current_set) %>%
    distinct(.data$to) %>%
    fnc(...)
  return(res)
}
