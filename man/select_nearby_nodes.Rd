% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_network.R
\name{select_nearby_nodes}
\alias{select_nearby_nodes}
\title{Filter a graph by distance from a starting node}
\usage{
select_nearby_nodes(g, g_filtered, start_node, n_nodes = 3)
}
\arguments{
\item{g}{an network of class sfnetwork}

\item{g_filtered}{is a g network filtred by distance}

\item{start_node}{is the node from which the distance is calculated.}

\item{n_nodes}{is the number of nodes used to create a loop. 3 + start_node (default) is recommanded.}
}
\value{
the filtered graph
}
\description{
This function takes a graph and a subgraph as input, as well as a starting node, and selects a number of nodes close
to the starting node in the subgraph.
}
