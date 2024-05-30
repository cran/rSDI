#' Not for explicit use.
#'
#' @param g the graph
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a vector of vertex SDI values
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraphWithSDI <- unweightedAllVerticesSDI(toyGraph)
unweightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {unweightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(igraph::V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g the graph
#'
#' @return a numerical SDI value
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraphWithSDI <- unweightedNetworkSDI(toyGraph)
unweightedNetworkSDI <- function(g) {
  edges <- igraph::E(g)
  sum(edges$distance)/length(edges)
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param v the vertex
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a numerical SDI value
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraph <- dist_calc(toyGraph)
#' toyGraphWithSDI <- unweightedSingleVertexSDI(toyGraph,igraph::V(toyGraph)[1])
unweightedSingleVertexSDI <- function(g, v, mode="all") {
  vedges<-igraph::incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$distance)/length(vedges)
  } else {
    NA
  }
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a vector of vertex SDI values
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraphWithSDI <- weightedAllVerticesSDI(toyGraph)
weightedAllVerticesSDI <- function (g, mode="all") {
  sdi <- function(v) {weightedSingleVertexSDI(g,v,mode=mode)}
  unlist(  lapply(igraph::V(g), sdi  )  )
}

#' Not for explicit use.
#'
#' @param g the graph
#'
#' @return a numerical SDI value
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraphWithSDI <- weightedNetworkSDI(toyGraph)
weightedNetworkSDI <- function(g) {
  edges <- igraph::E(g)
  sum(edges$weight*edges$distance)/sum(edges$weight)
}

#' Not for explicit use.
#'
#' @param g the graph
#' @param v the vertex
#' @param mode directionality 'undirected', 'in', 'out', or 'all'
#'
#' @return a numerical SDI value
#' @export
#'
#' @examples
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#' toyGraph <- igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#' toyGraphWithSDI <- weightedSingleVertexSDI(toyGraph,igraph::V(toyGraph)[1])
weightedSingleVertexSDI <- function(g, v, mode="all") {
  vedges<-igraph::incident(g,v,mode=mode)
  if (length(vedges)>0) {
    sum(vedges$weight*vedges$distance)/sum(vedges$weight)
  } else {
    NA
  }
}
