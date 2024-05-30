#' Distance Calculation for Graph Nodes
#'
#' @description
#' This function calculates distances between each pair of nodes in the graph.
#' It supports both Haversine and Euclidean formula. The function automatically selects the formula based on the availabe vertex attributes: 'x' and 'y' for Euclidean distances, 'latitude' and 'longitude' for Haversine distances.
#'
#' @param g An igraph object, with nodes that have a combination of 'latitude' and 'longitude', or 'x' and 'y' vertices attributes.
#' @param formula Optional parameter to specify the distance calculation formula to use, either 'Haversine' or 'Euclidean'. By default `formula = NULL` and if not specified the otherwise, the function automatically determines the formula based on the available vertex attributes. [x, y] => Euclidean, [latitude,longitude] => Haversine. Note that the `g` must have one of this set of vertex attributes.
#'
#' @return An igraph object with an additional edge attribute 'distance' that contains the calculated distances between each pair of nodes.
#'
#' @examples
#' # Assuming 'g' is a graph object with latitude and longitude or x and y attributes for each node.
#' # an overall example
#' flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
#'
#' # user provides x and y vertices
#' nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
#'
#' g<-igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#'
#'
#' dist_calc(g) # eucl. dist. calculated
#' dist_calc(g, formula = 'Euclidean') # calculates euc when asked
#' #dist_calc(g, formula = 'Haversine') # error
#'
#' # user provides latitude and longitude vertices instead of x&y
#' nodes<-data.frame(id=c("A","B","C","D"),latitude=c(0,4,0,4),longitude=c(3,0,0,3))
#'
#' g<-igraph::graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
#'
#' dist_calc(g) # haversine dist calculated
#' dist_calc(g, formula = 'Haversine') # calculated hav when asked specificallly
#' #dist_calc(g, formula = 'Euclidean') # error
#'
#' @export
dist_calc <- function(g, formula = NULL) {
  if (!requireNamespace("igraph", quietly = TRUE)) {stop("igraph is required")}
  # get edges
  edges_g <- igraph::get.edgelist(g) #TODO: deprecated
  # columns may include lower-Capital case letters
  # lower them, change them in g to lowercase
  # then revert this change before returning g
  vertexAttrsOld <-igraph::vertex_attr_names(g)
  vertexAttrsNew <- tolower(igraph::vertex_attr_names(g))
  names(igraph::vertex_attr(g)) <- vertexAttrsNew


  if(is.null(formula) ){
    if (('x' %in% vertexAttrsNew) && ('y' %in% vertexAttrsNew)){
      formula <- 'Euclidean'
    } else if (('latitude' %in% vertexAttrsNew) && ('longitude' %in% vertexAttrsNew)){
      formula <- 'Haversine'
    } else {
      stop(paste("The igraph object must have a combination of 'x' and 'y',
           or 'latitude' and 'longitude' vertex attributes for distance to
           be calculated. Available attribute names are:",vertexAttrsNew))
    }
  } else {
    if(formula == 'Euclidean'){
      if(!('x' %in% vertexAttrsNew) || !('y' %in% vertexAttrsNew)){
        stop("The igraph object must have 'x' and 'y' vertices for distances to be calculated.")
      }
    } else if (formula == 'Haversine'){
      if(!('latitude' %in% vertexAttrsNew) || !('longitude' %in% vertexAttrsNew)){
        stop("The igraph object must have 'latitude' and 'longitude' vertices
             for distances to be calculated.")
      }
    } else {
      stop("The formula must be either 'Haversine' or 'Euclidean'.")
    }
  }

  # check types of vertices
  if (formula == 'Euclidean'){
    if (!is.numeric(igraph::V(g)$x) || !is.numeric(igraph::V(g)$y)){
      stop("The 'x' and 'y' vertices must be numeric.")
    }
  } else if (formula == 'Haversine'){
    if (!is.numeric(igraph::V(g)$latitude) || !is.numeric(igraph::V(g)$longitude)){
      stop("The 'latitude' and 'longitude' vertices must be numeric.")
    }
  }

  # initiate dist values
  distH <- vector(length = nrow(edges_g), mode = 'numeric')
  distV <- vector(length = nrow(edges_g),mode = 'numeric')
  distance <- vector(length = nrow(edges_g), mode = 'numeric')

  # check formula
  if (formula == 'Euclidean'){
    distV <- igraph::V(g)$x
    distH <- igraph::V(g)$y
    # this is needed to accurately calculate distances
    names(distH) <- igraph::V(g)$name
    names(distV) <- igraph::V(g)$name

    for (i in seq_len(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- euclidean(
        distV[node1], distH[node1],
        distV[node2], distH[node2]
      )
    }
  } else if (formula == 'Haversine'){
    distV <- igraph::V(g)$longitude
    distH <- igraph::V(g)$latitude
    # this is needed to accurately calculate distances
    names(distH) <- igraph::V(g)$name
    names(distV) <- igraph::V(g)$name
    for (i in seq_len(nrow(edges_g))) {
      node1 <- edges_g[i, 1]
      node2 <- edges_g[i, 2]
      distance[i] <- haversine(
        distV[node1], distH[node1],
        distV[node2], distH[node2]
      )
    }
  }
  names(igraph::vertex_attr(g)) <- vertexAttrsOld
  igraph::E(g)$distance <- distance
  return(g)
}

#' Calculate Euclidean Distance Between Two Points
#'
#' @description
#' This function calculates the Euclidean distance between two points. The Euclidean is the 'straight line' distance
#' between two points in a two-dimensional space. This function takes the coordinates of two points (longitude and latitude)
#' and calculates the straight distance between them, assuming flat Earth approximation.
#'
#' @param x1 X-coordinate of the first point.
#' @param y1 Y-coordinate of the first point.
#' @param x2 X-coordinate of the second point.
#' @param y2 Y-coordinate of the second point.
#'
#' @return The Euclidean distance between the two points.
#'
#' @examples
#' euclidean(1, 2, 4, 6)
#' # Euclidean distance between points (1, 2) and (4, 6).
#'
#' @export
euclidean <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

#' Calculate Haversine Distance Between Two Points on Earth
#'
#' @description
#' This function calculates the great-circle distance between two points on the Earth's surface,
#' given their longitude and latitude in decimal degrees.
#' It uses the Haversine formula, which accounts for the Earth's curvature.
#'
#' @param lon1 Longitude of the first point in decimal degrees.
#' @param lat1 Latitude of the first point in decimal degrees.
#' @param lon2 Longitude of the second point in decimal degrees.
#' @param lat2 Latitude of the second point in decimal degrees.
#' @param R The radius of the Earth in kilometers.
#'
#' @return The distance between the two points in kilometers.

#' @examples
#' haversine(-73.9851, 40.7580, -0.1278, 51.5074)
#' # Distance between NY City and London.
#'
#' @export
haversine <- function(lon1,lat1,lon2,lat2, R = 6371){
  # degrees to radians
  lon1 <- lon1 * pi/180
  lat1 <- lat1 * pi/180
  lon2 <- lon2 * pi/180
  lat2 <- lat2 * pi/180

  # H formula
  dlon <- lon2 - lon1
  dlat <- lat2 - lat1
  a <- sin(dlat /2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  H <- 2 * R * asin(sqrt(a))
  H
}
