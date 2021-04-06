#' Make/add event
#'
#' Make or add event
#'
#' @param event_list list with event description created previously by this function, default is empty list and it does not need to be filled when creating first event.
#' @param value value which shall be replaced.
#' @param column which column of a table should be affected.
#' @param snap numeric vector, this  (snaps).
#' @param table string, table name which should be affected. It should be 'vertice' for changing vertice description and 'edge' for changing the network architecture.
#' @param vertices when table is 'vertice' a list of vertices which shall be affected.
#' @param state when table is 'vertice' this variable affect its state column. This is a convenient variable to facilitate changing several columns with one command.
#' @param vertex_color when table is 'vertice' this variable affect its color column. This is a convenient variable to facilitate changing several columns with one command.
#' @param from string, when table is 'edge' a `from` is used to subset the edges by the from column. If not used the subsetting is done solely on the to variable.
#' @param to string, when table is 'edge' a `to` is used to subset the edges by the to column. If not used the subsetting is done solely on the from variable.
#'
#' @return list with events definition.
#' @export
#'
#' @examples
#'
#'
#' df_edges <- create_source(set = c(1,2,1),
#'                             weight = c(5,2,10)) %>%
#'  add_level_set(weight = c(2,1,1),
#'                previous_set = list(1,2,3),
#'                set = c(2,2,1)) %>%
#'  add_level_set(weight = c(4,2),
#'                previous_set = list(c(1,2), 3),
#'                set = c(2,2)) %>%
#'  add_fully_connected(set = c(1,1,1),
#'                      weight = 2) %>%
#'  add_level_set(weight = c(4,2,1),
#'                previous_set = list(1,2,3),
#'                set = c(2,1,1)) %>%
#'  add_exit()
#'
#' l_events <- make_event(value = 300000,
#'                       table = "vertice",
#'                        column = "remaining_time",
#'                       vertices = c("3_3", "3_4"),
#'                       vertex_color = 2,
#'                       state = "off",
#'                       snap = 20) %>%
#'  make_event(value = 1000,
#'             table = "edge",
#'             column = "weight",
#'             # from = "2_2",
#'             to = "3_1",
#'             snap = 10) %>%
#'  make_event(value = 1,
#'             column = "remaining_time",
#'             vertices = c("3_3", "3_4"),
#'             vertex_color = 1,
#'             state = "on",
#'             snap = 40) %>%
#'  make_event(value = 1,
#'             column = "remaining_time",
#'             vertices = c("3_1"),
#'             vertex_color = 1,
#'             state = "on",
#'             snap = 25) %>%
#'  make_event(value = 1,
#'             table = "edge",
#'             column = "weight",
#'             # from = "2_2",
#'             to = "3_1",
#'             snap = 20)
#'
#' res <- simulate_flow(df_edges = df_edges,
#'                      l_events = l_events,
#'                      timestep = 1,
#'                      input_people = 40)
#'
make_event <- function(event_list = list(),
                       value,
                       column,
                       snap,
                       table = "vertice",
                       vertices = NA,
                       state = NA,
                       vertex_color = NA,
                       from = NA,
                       to = NA) {

  event_list[[length(event_list) + 1]] <- list(value = value,
                                               column = column,
                                               snap = snap,
                                               table = table,
                                               vertices = vertices,
                                               vertex_color = vertex_color,
                                               state = state,
                                               from = from,
                                               to = to)

 return(event_list)
}
