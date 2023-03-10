% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sampling_network.R
\name{sampling_loop}
\alias{sampling_loop}
\title{Sample and return a loop from a graph}
\usage{
sampling_loop(g, clean, dist, nodes = 3)
}
\arguments{
\item{g}{an object of class sfnetwork}

\item{clean}{logical indicating whether to clean the path or not}

\item{dist}{the maximum distance to consider when sampling the starting node}

\item{nodes}{the number of nodes to select}
}
\value{
an sf object representing the sampled path
}
\description{
This function takes a graph and a distance as input, and returns a random path of length close to that distance by
selecting a starting node at random, filtering the graph to keep only nodes close to the starting node, and finding
the shortest path between three such nodes.
}
