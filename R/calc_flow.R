#' Calculate the load of nodes in the network
#'
#' This function calculates the maximum possible input and output
#' from the nodes and its load in this flow using the edges in the network.
#'
#' @param network data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`.
#' @param constrain bool deciding on constraining the maximum possible flow in a level to the maximum flow in the previous nodes.
#'
#' @return nodes data.frame with the definition of nodes by the columns `id`, `input_people`, `output_people`, `node_load`
#'
#' @examples
#' network <- create_source(set = c(3,2,5), weight = 2.5) %>%
#' add_level_set(weight = c(2,4,1), previous_set = c(3,2,1),set = c(2,3, 1)) %>%
#'   add_level_set(weight = c(2,2,2),previous_set = c(1,2,3),set = c(1,1,1)) %>%
#'   add_fully_connected(weight = 5,set = 4) %>%
#'   add_exit()
#'
#' node_load <- get_node_load(network)
#'
#' @export
get_node_load <- function(network, constrain = TRUE){
  ids <- unique(c(network$from,network$to))
  
  nodes <- data.frame(id = numeric(length(ids)),
                      input_people = numeric(length(ids)),
                      output_people = numeric(length(ids)),
                      node_load = numeric(length(ids))
  )
  
  nodes$id <- ids
  
  nodes$output_people[which(nodes$id == 'source')] <- sum(1/network$weight[which(network$from == 'source')])
  nodes$input_people[which(nodes$id == 'source')] <- nodes$output_people[which(nodes$id == 'source')]
  nodes$node_load[which(nodes$id == 'source')] <- 1
  
  for (a in 2:length(unique(network$level))) {
    nodes_in_level <- unique(network$from[which(network$level == a)])
    for (b in 1:length(nodes_in_level)) {
      # get nodes which have output to this nodes
      #from_nodes <- network$from[which(network$to == nodes_in_level[a])]
      from_nodes <- network$from[which(network$to == nodes_in_level[b])]
      
      var_ <- 0
      for (c in 1:length(from_nodes)) { # sum of all nodes coming into nodes_in_level[b] weighted by edges coming to node
        flow2node_by_edge <- 1/network$weight[which(network$from == from_nodes[c] & network$to == nodes_in_level[b])]
        overal_flow_from_node_by_edge <- sum(1/network$weight[which(network$from == from_nodes[c])])
        var_ <- var_ + (nodes$output_people[which(nodes$id == from_nodes[c])])*(flow2node_by_edge/overal_flow_from_node_by_edge)
        
        #var_ <- var_ + (nodes$output_people[which(nodes$id == from_nodes[c])])/length(which(network$from == from_nodes[c]))
      }
      nodes$input_people[which(nodes$id == nodes_in_level[b])] <- var_
      
      if(constrain){
        max_output <- max(1/network$weight[which(network$from == nodes_in_level[b])])
        nodes$output_people[which(nodes$id == nodes_in_level[b])] <- min(max_output,var_)
        nodes$node_load[which(nodes$id == nodes_in_level[b])] <- var_/max_output
      } else {
        max_output <- max(1/network$weight[which(network$from == nodes_in_level[b])])
        nodes$output_people[which(nodes$id == nodes_in_level[b])] <- max_output
        nodes$node_load[which(nodes$id == nodes_in_level[b])] <- var_/max_output
      }
    }
  }
  

  from_nodes <- network$from[which(network$to == 'exit')]
  var_ <- 0
  for (c in 1:length(from_nodes)) { # sum of all nodes coming into nodes_in_level[b] divided by no of edges from node
    var_ <- var_ + (nodes$output_people[which(nodes$id == from_nodes[c])])
  }
  nodes$input_people[which(nodes$id == 'exit')] <- var_
  
  
  nodes$output_people[which(nodes$id == 'exit')] <- nodes$input_people[which(nodes$id == 'exit')]
  nodes$node_load[which(nodes$id == 'exit')] <- 1
  
  
  return(nodes)
}


#' Calculate the total flow through the individual levels of the network.
#'
#' This function calculates the total flow through the individual
#' network levels in the defined network.
#'
#' @param network data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`
#' @param constrain bool deciding on constraining the maximum possible flow in a level to the maximum flow in the previous level.
#'
#' @return levels_throughtput data.frame containing the maximum flow in network levels with the columns `level_id`, `throughtput`
#'
#' @examples
#' network <- create_source(set = c(3,2,5), weight = 2.5) %>%
#' add_level_set(weight = c(2,4,1), previous_set = c(3,2,1),set = c(2,3, 1)) %>%
#'   add_level_set(weight = c(2,2,2),previous_set = c(1,2,3),set = c(1,1,1)) %>%
#'   add_fully_connected(weight = 5,set = 4) %>%
#'   add_exit()
#'
#' level_throughput <- get_level_throughput(network)
#'
#' @export
get_level_throughput <- function(network, constrain = TRUE) {
  
  nodes <- get_node_load(network, constrain)
  
  levels_throughtput <- data.frame(level_id = numeric(length(unique(network$level))),
                                   throughtput = numeric(length(unique(network$level)))
  )
  
  levels_throughtput$level_id <- unique(network$level)
  for (a in 1:dim(levels_throughtput)[1]) { # for all levels
    
    inv_level <- levels_throughtput$level_id[a] # level which is investigated
    levels_in_network <- which(network$level == inv_level) # find which levels in network df are investigated
    edges_from <- unique(network$from[levels_in_network]) # from which nodes are edges in investigated level coming from (unique)
    edges_from_nodes <- which(nodes$id %in% edges_from) # get nodes from which investigated edges come from
    
    levels_throughtput$throughtput[a] <- sum(nodes$output_people[edges_from_nodes])
    # levels_throughtput$throughtput[a] <- max(nodes$output_people[edges_from_nodes])
  }
  
  return(levels_throughtput)
  
}


#' Add flow through each network level to the simulation animation.
#'
#' This function writes the maximum flow through each level
#' to the left side of the animation graph.
#'
#' @param network data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`
#' @param level_throughput data.frame containing the maximum flow in network levels with the columns `level_id`, `throughtput`
#' @param res list containing the simulated data needed to animate the simulation
#' @param x_coord numeric, this is the `x` coordinate.
#' @param text_prefix string, prefix that should be added in front of values. Default is "Flow: ".
#' @param adjustment numeric, throughput is multiplied by this value. It can be used in case that the definition of network is in seconds and desired througput should be in hours.
#'
#' @return list containing the simulated data needed to animate the simulation
#'
#' @examples
#' network <- create_source(set = c(3,2,5), weight = 2.5) %>%
#' add_level_set(weight = c(2,4,1), previous_set = c(3,2,1),set = c(2,3, 1)) %>%
#'   add_level_set(weight = c(2,2,2),previous_set = c(1,2,3),set = c(1,1,1)) %>%
#'   add_fully_connected(weight = 5,set = 4) %>%
#'   add_exit()
#'
#' level_throughput <- get_level_throughput(network, constrain = FALSE)
#' res <- simulate_flow(network)
#' res <- write_level_flow_simulation(network,
#'                         level_throughput,
#'                         res,
#'                         -6.5)
#' animate_simulation(network,
#'                    res[[1]],
#'                    res[[2]],
#'                   snap = 0)
#'
#' @export
write_level_flow_simulation <- function(network,
                                        level_throughput,
                                        res,
                                        x_coord,
                                        text_prefix = "Flow: ",
                                        adjustment = 1){
  net_coord <- get_network_levels_coord(network)
  
  text_df <- data.frame(x = x_coord,
                        y = net_coord[2:(length(net_coord))],
                        text = paste0(text_prefix, round(level_throughput$throughtput * adjustment, 2))
  )
  res <- add_text(res,
                  text_df)
  return(res)
}


#' Add flow info of each node to the simulation animation.
#'
#' This function writes the maximum input, output, and node load of each node
#' into the graph during animation.
#'
#' @param network data.frame with the definition of edges by the columns `from`, `to`, `weight`, `level`, `set`
#' @param node_load data.frame with the definition of nodes by the columns `id`, `input_people`, `output_people`, `node_load`
#' @param res list containing the simulated data needed to animate the simulation
#' @param input_people bool indicating whether to write maximum possible input to the node  into the graph
#' @param output_people bool indicating whether to write maximum possible output from the node into the graph
#' @param node_flow bool indicating whether to write node load at maximum input and output into the graph
#'
#' @return list containing the simulated data needed to animate the simulation
#'
#' @examples
#' network <- create_source(set = c(3,2,5), weight = 2.5) %>%
#'   add_level_set(weight = c(2,4,1), previous_set = c(3,2,1),set = c(2,3, 1)) %>%
#'   add_level_set(weight = c(2,2,2),previous_set = c(1,2,3),set = c(1,1,1)) %>%
#'   add_fully_connected(weight = 5,set = 4) %>%
#'   add_exit()
#'
#' node_load <- get_node_load(network)
#' res <- simulate_flow(network)
#' res <- write_node_flow(network, node_load, res)
#' animate_simulation(network,
#'                    res[[1]],
#'                    res[[2]],
#'                   snap = 0)
#' @export
write_node_flow <- function(network,
                            node_load,
                            res,
                            input_people = TRUE,
                            output_people = TRUE,
                            node_flow = TRUE){
  node_coords <- get_coord(network)
  
  node_coords$people_in <- NA
  node_coords$people_out <- NA
  node_coords$load <- NA
  
  for (a in 1:dim(node_coords)[1]) {
    node_coords$people_in[a] <- node_load[which(node_load$id == node_coords$vertice[a]), "input_people"]
    node_coords$people_out[a] <- node_load[which(node_load$id == node_coords$vertice[a]), "output_people"]
    node_coords$load[a] <- node_load[which(node_load$id == node_coords$vertice[a]), "node_load"]
  }
  
  text_df <- data.frame(x = numeric(0),
                        y = numeric(0),
                        text = character(0)
  )
  
  if (input_people) {
    text_df <- rbind(text_df, data.frame(x = node_coords$x - 0.0,
                                         y = node_coords$y + 0.1,
                                         text = paste('in:', round(node_coords$people_in, digits = 2))
    ))
  }
  
  if (output_people) {
    text_df <- rbind(text_df, data.frame(x = node_coords$x - 0.0,
                                         y = node_coords$y - 0.1,
                                         text = paste('out:', round(node_coords$people_out, digits = 2))
    ))
  }
  
  if (node_flow) {
    text_df <- rbind(text_df, data.frame(x = node_coords$x + 0.0,
                                         y = node_coords$y,
                                         text = paste('load:', round(node_coords$load, digits = 2))
    ))
  }
  
  res <- add_text(res, text_df)
  return(res)
  
}

