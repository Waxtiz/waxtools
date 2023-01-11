#' Sample a starting node from a graph (for loops)
#'
#' This function selects a node at random from a graph and returns it.
#'
#' @param g an object of class sfnetwork
#' @return the randomly selected node
#'
#' @importFrom igraph V
#'
#' @export
#' @examples
#' \dontrun{
#' sample_one_node(g)
#' }
sample_one_node <- function(g) {
  # select a random node
  start_node <- unlist(as.list(sample(V(g), size = 1)))
  return(start_node)
}

#' Sample a start and end point on the network (for paths)
#'
#' This function selects two nodes at random from a graph and returns it.
#'
#' @param g an object of class sfnetwork
#' @param nb_nodes nb_nodes is the number of total points (start and finish). The starting point is always equal to 1.
#' Only the arrival points can be more than 1
#'
#' @importFrom sf st_as_sf
#'
#' @return a list with the starting point (from) and the ending point(s) (to)
sample_two_nodes <- function(g, nb_nodes = 2) {

  n_nodes <- g %>% st_as_sf("nodes") %>% nrow()         # take nodes numbers
  from_node <- sample(1:n_nodes, size = 1)                # sample 1 node into all nodes, it's the start of path
  to_nodes <- sample(1:n_nodes[n_nodes != from_node],
                     size = nb_nodes-1,
                     replace = F)                         # samples x node in all nodes by deleting the starting node

  # Put the result in a list
  res <- list(from_node, to_nodes)
  names(res) <- c("from", "to")

  return(res)
}



#' Filter a graph by distance from a starting node
#'
#' This function takes a graph and a start node as input, and returns a subgraph consisting of the nodes at a certain
#' distance from the start node.
#'
#' @param g an object of class sfnetwork
#' @param start_node the starting node
#' @param dist the maximum distance to consider
#'
#' @import sf
#' @import dplyr
#' @import sfnetworks
#' @importFrom magrittr `%>%`
#' @importFrom tidygraph node_distance_from
#'
#' @return the filtered graph
g_filter <- function(g, start_node, dist) {

  suppressMessages(sf_use_s2(FALSE))

  # find all nodes within distance dist of the start node
  iso_min <- g %>%
    activate("nodes") %>%
    filter(node_distance_from(start_node, weights = weight) <= dist*.8) %>%
    st_geometry() %>%
    st_combine() %>%
    st_convex_hull()

  iso_max <- g %>%
    activate("nodes") %>%
    filter(node_distance_from(start_node, weights = weight) <= dist*1.2) %>%
    st_geometry() %>%
    st_combine() %>%
    st_convex_hull()

  suppressMessages({
  iso <- st_difference(iso_max, iso_min)

  g_filtered <- g %>%
    activate("nodes") %>%
    st_filter(., iso, .pred = st_intersects)
  })

  suppressMessages(sf_use_s2(TRUE))

  return(g_filtered)
}


#' Filter a graph by distance from a starting node
#'
#' This function takes a graph and a subgraph as input, as well as a starting node, and selects a number of nodes close
#' to the starting node in the subgraph.
#'
#' @param g an network of class sfnetwork
#' @param g_filtered is a g network filtred by distance
#' @param start_node is the node from which the distance is calculated.
#' @param n_nodes is the number of nodes used to create a loop. 3 + start_node (default) is recommanded.
#'
#' @import sf
#' @import dplyr
#' @import sfnetworks
#' @importFrom magrittr `%>%`
#'
#' @return the filtered graph
select_nearby_nodes <- function(g, g_filtered, start_node, n_nodes = 3) {

  # Extract the coordinates of each node contained in g_filtred
  coords_nodes_in_g_filtered <- g_filtered %>%
    st_as_sf() %>%
    st_coordinates() %>%
    as.data.frame()

    # select 3 at random
    sample_rowname <- sample(nrow(coords_nodes_in_g_filtered), n_nodes, replace = F)
    # extrait les 3 lignes selectionnees aleatoirements
    coords_select <- coords_nodes_in_g_filtered %>%
      filter(rownames(.) %in% sample_rowname)


  ids_nodes <- c(start_node)
  for (i in 1:nrow(coords_select)) {
    # create an sf point for the coordinate
    pt <- st_point(c(coords_select[i, ]$X, coords_select[i, ]$Y)) %>%
      st_sfc() %>%
      st_set_crs(4326)
    # retrieve the index of the point on the graph g and store it after the list
    ids_nodes[i+1] <- st_nearest_feature(pt, g %>% activate("nodes"))
  }
  return(ids_nodes)
}

#' Find the shortest paths between the nodes of a graph, as a loop
#'
#' This function takes a graph and a list of nodes as input, and returns a data frame containing the shortest paths
#' between each pair of nodes in the list.
#'
#' @param g an object of class sfnetwork
#' @param selected_nodes a vector of nodes
#'
#' @importFrom dplyr bind_rows
#' @import sfnetworks
#'
#' @return a data frame containing the shortests paths
#' @export
find_shortest_loops <- function(g, selected_nodes) {
  # create an empty list to store the shortest paths
  shortest_paths_sf <- list()
  g <- g %>% activate("nodes")

  suppressWarnings({
    # find the shortest path between nodes
    for (i in 1:(length(selected_nodes) - 1)) {

      path_sf <- st_network_paths(g,
                                  from = selected_nodes[i],
                                  to = selected_nodes[i + 1],
                                  weights = E(g)$weight)
      shortest_paths_sf[[i]] <- path_sf
    }
  })

  # find the shortest path between the last and the first node
  path_sf <- st_network_paths(g,
                              from = selected_nodes[length(selected_nodes)],
                              to = selected_nodes[1],
                              weights = E(g)$weight)

  # Add it to the list
  shortest_paths_sf[[length(selected_nodes)+1]] <- path_sf

  # And bind to df format
  shortest_paths_df <- bind_rows(shortest_paths_sf)

  return(shortest_paths_df)
}


#' Transform shortest paths into sf format
#'
#' This function takes a graph and a data frame of paths as input, and returns an object of class sf containing the
#' paths of the data frame.
#'
#' @param g an object of class sfnetwork
#' @param shortest_paths_df a data frame containing the shortest paths
#'
#' @import sf
#' @import dplyr
#' @import sfnetworks
#' @importFrom magrittr `%>%`
#'
#' @return an sf object representing the shortest paths
shortest_loops_to_sf <- function(g, shortest_paths_df) {

  # Selects the nodes of the circuit
  node_path <- shortest_paths_df  %>%
    pull(node_paths) %>%
    unlist()

  # Extract the previous nodes on the original network to keep the curves
  path_sf <- g %>%
    activate(nodes) %>%
    slice(node_path) %>%
    st_as_sf("edges")

  return(path_sf)
}

#' Clean a loop
#'
#' This function cleans a loop by eliminating any round trips.
#'
#' @param path_sf An sf object representing a loop.
#' @param clean Logical. If TRUE, the function will clean the loop. If FALSE, the function will return the loop without cleaning it.
#'
#' @import sf
#' @importFrom magrittr `%>%`
#' @importFrom dplyr select
#'
#' @return An sf object representing a cleaned loop.
clean_loop_sampled <- function(path_sf, clean) {
  path_sf %>% select(geometry)
  if (clean) {
    # Cleaning of the circuit to avoid round trips
    tryCatch(
      path_sf %>%
        st_union() %>%
        st_polygonize() %>%
        st_make_valid() %>%
        st_cast("MULTILINESTRING") %>%
        st_as_sf(),
      error = function(e) {
        message("An error occurred during the cleaning output, it will be returned uncleaned.")
        path_sf
      }
    )
  } else {
    path_sf
  }
}


#' Sample and return a loop from a graph
#'
#' This function takes a graph and a distance as input, and returns a random path of length close to that distance by
#' selecting a starting node at random, filtering the graph to keep only nodes close to the starting node, and finding
#' the shortest path between three such nodes.
#'
#' @param g an object of class sfnetwork
#' @param clean logical indicating whether to clean the path or not
#' @param dist the maximum distance to consider when sampling the starting node
#' @param nodes the number of nodes to select
#'
#'
#' @return an sf object representing the sampled path
sampling_loop <- function(g, clean, dist, nodes = 3) {

  start_node <- sample_one_node(g)
  g_filtered <- g_filter(g, start_node, dist)

  attempt <- 0
  selected_nodes <- NULL
  while(is.null(selected_nodes) && attempt <= 10) {
    attempt <- attempt + 1
    try(
      selected_nodes <- select_nearby_nodes(g, g_filtered, start_node, nodes)
    )
  }
  if (attempt >= 10) {
    stop("The random selection has failed multiple times. Maybe increase the difference between min_length and max_length to adjust them to the network.")
  }

  selected_nodes <- select_nearby_nodes(g, g_filtered, start_node, nodes)
  shortest_loops_df <- find_shortest_loops(g, selected_nodes)
  loop_sf <- shortest_loops_to_sf(g, shortest_loops_df)
  loop_sf_clean <- clean_loop_sampled(loop_sf, clean)

  return(loop_sf_clean)
}

#' Sample and return a path from a graph
#'
#' @param g an object of class sfnetwork
#'
#' @import sfnetworks
#' @import dplyr
#'
#' @return an sf object representing the sampled path
sampling_path <- function(g) {

  sample_nodes <- sample_two_nodes(g)

  # Calculat the shortest path
  paths <- st_network_paths(g,
                            from = sample_nodes$from,
                            to = sample_nodes$to,
                            weights = "weight")

  # extract the node IDs
  node_path <- paths %>%
    pull(node_paths) %>%
    unlist()

  # only keep the network for these nodeIDs
  path_sf <- g %>%
    activate(nodes) %>%
    slice(node_path) %>%
    st_as_sf("edges")                                     # reformat edges into sf object

  return(path_sf)
}

#' Sample paths or loops from a graph
#'
#' This function calls the sampling_loop function several times in a row to produce several random paths paths,
#' checking that these paths have no common parts and that their length is within a specified range.
#'
#' @param g an object of class sfnetwork
#' @param n the number of paths to sample
#' @param min_length the minimum length for a path
#' @param max_length the maximum length for a path
#' @param clean logical indicating whether to clean the paths or not
#' @param overlap_check logical indicating whether to check for overlap between paths
#' @param length_check logical indicating whether to check the length of the paths
#' @param loop logical indicating whether the generated path should be looped or on its way.
#'
#' @import sf
#'
#' @return a data frame containing the sampled paths
multiple_sample_in_net <- function(g, n, min_length, max_length, clean, overlap_check, length_check, loop) {

  dist <- mean(min_length, max_length)

  start_time <- Sys.time()

  i <- 0
  number_of_try <- 0
  while (i != n) {
    while (TRUE) {
      number_of_try <- number_of_try + 1
      if (loop) {
        path <- sampling_loop(g = g, clean = clean, dist = dist)
      } else {
        path <- sampling_path(g = g)
      }
      current_length <- as.integer(sum(st_length(path)))
      if (length_check != TRUE || (current_length > min_length && current_length < max_length)) {
        if (overlap_check != TRUE || (i == 0 || sum(st_intersects(path, paths_df, sparse = F)) == 0)) {
          break
        }
      }

      # Check if the time limit has been exceeded
      if (difftime(Sys.time(), start_time) > 30) { # limit are fixend at 30 secondes
        if (i == 0) {
          # If it is the first path, start the loop again without incrementing i
          message("The parameters seem to be very strict. It is complicated to find a loop. Please wait or shut-down the function and increase the difference between min_length and max_length.")
          start_time <- Sys.time()
          next
        } else {
          # If it is not the first path, delete the first path of the dataframe and start
          # the loop again without incrementing i
          message("The loops seem to be in conflict. The first loop is deleted to try to speed up the process.")
          start_time <- Sys.time() # resets the time counter
          paths_df <- paths_df[-1,]
          break
        }
      }
      message(paste0("Attempt number  : ", number_of_try, "."))
    }
    i <- i + 1
    path <- mutate(path, id_path = as.integer(i))
    if (i == 1) {                                         # check if this is the first path
      paths_df <- path                                    # if yes, creat dataframe of paths with first path
    } else if (i > 1) {
      paths_df <- rbind(paths_df, path)                   # Then, add row of new path into exsiting dataframe of paths
      }
    message(paste0("Loop number ", i ," has been found"))
  }
  return(paths_df)
}


#' Sample one or multiple paths or loops from a graph
#'
#' This function samples one or more ways (loop or path) of a graph by selecting random nodes as starting points and
#' finding the shortest way between them. The length of the way is then checked to ensure that it is within the
#' specified range, and the resulting path can also be cleaned up to remove round trips. In
#' addition, the function can check for overlap between ways, if desired.
#'
#' @param g an object of class sfnetwork
#' @param n the number of loops to sample, default is 1
#' @param min_length the minimum length for a way (in units of crs)
#' @param max_length the maximum length for a way (in units of crs)
#' @param overlap_check logical indicating whether to check for overlap between ways
#' @param clean logical indicating whether to clean the loops or not (only work for loop)
#' @param length_check logical indicating whether to check the length of the ways
#' @param loop logical indicating whether the generated way should be looped or from point A to point B.
#'
#' @return a data frame containing the sampled loops
#' @export
sample_in_network <- function(g, n = 1, min_length, max_length, overlap_check = FALSE,
                                   clean = TRUE, length_check = TRUE, loop = FALSE) {
  # Checks
  stopifnot(inherits(g, "sfnetwork"))
  stopifnot(is.numeric(n))
  stopifnot(is.numeric(min_length))
  stopifnot(is.numeric(max_length))
  stopifnot(clean %in% c(TRUE, FALSE))
  stopifnot(overlap_check %in% c(TRUE, FALSE))
  stopifnot(length_check %in% c(TRUE, FALSE))
  stopifnot(loop %in% c(TRUE, FALSE))

  if (loop == FALSE && clean == TRUE) {
    message("Please note that the cleaning option is only valid for paths and not for loops.")
    clean <- FALSE
    message("The cleaning option has been automatically cancelled.")
  }

  paths <- multiple_sample_in_net(g = g, n = n, min_length = min_length,
                                       max_length = max_length, clean = clean,
                                       overlap_check = overlap_check,
                                       length_check = length_check, loop = loop)

  return(paths)
}
