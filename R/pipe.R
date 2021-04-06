#' Pipe
#'
#' Like dplyr, CVCS also uses the pipe function, \code{\%>\%} to turn
#' function composition into a series of imperative statements.
#' This import is copied from ggvis package and example is modified for CVCS.
#'
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
#' @param lhs,rhs A visualisation and a function to apply to it
#' @examples
#' # Instead of
#' add_exit(add_fully_connected(create_source(1,2), 1,2))
#' # you can write
#' create_source(1,2) %>%
#'   add_fully_connected(1,2) %>%
#'   add_exit()
NULL
