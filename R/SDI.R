#' Computes graph or vertex level Spatial Dispersion Index(ces).
#'
#' If 'flows' is an igraph object then one can avoid supplying the nodes parameter.
#' Alternatively one can supply flows as a data frame and nodes as another.
#'
#' To have an SDI computed you can must provide level, weight.use, and directionality parameters.
#' Alternatively the 'variant' parameter can be specified which allows short-codes to indicate all of these three parameters.
#' For example a value of "vui" for variant means a **v**ertex level, **u**ndirected, and **i**nward directed SDI calculation.
#' See the description of these three parameters to figure out possible short codes in a similar fashion.
#'
#' The function returns an igraph object. If a network level calculation is requested the object will have an
#' 'SDI_...' attribute whose name follows the short codes explained above. If a vertex level calculation is requested
#' each vertex will have a similarly named attribute. For example the graph will have an 'SDI_nuw' attribute if variant is
#' "nuw" (network level, undirected, and weighted). If variant is "vwu" each vertex will have an "SDI_vwu" attribute containing
#' weighted undirected SDI for the vertex. If variant is a vector each value indicates a separate variant to be computed.
#'
#' @param flows A data frame or an igraph object
#' @param nodes if flows are data frame, nodes must be supplied as a data frame. If flows are igraph object then not required
#' @param distance.calculation optional method for distance calculation. 'Haversine' or 'Euclidean'. If not provided and edge distances are not available, distances are calculated by the SDI function.
#' @param level The level to calculate the SDI. 'network' or 'vertex'
#' @param weight.use 'weighted', 'unweighted', or 'generalized'
#' @param directionality 'undirected', 'in', 'out', or 'all'
#' @param variant Optional. Instead of specifying the level, directionality, and weight separately, the user can just supply a short-code of initial letters of each in that order to this argument, e.g. "vuw" for vertex level, undirected and weighted SDI. If it is a vector each value is treated separetely and multiple indices are computed.
#' @param alpha Optional parameter used for generalized SDI calculations.
#'
#' @return An igraph object with SDI attributes added. The class of the object includes 'SDI'.
#' @examples
#' SDI(TurkiyeMigration.flows, TurkiyeMigration.nodes, variant="vuw")
#'
#' @export
SDI <- function (flows, nodes = NULL,  distance.calculation = NULL, level = "vertex",
                          weight.use = "weighted", directionality = "undirected",
                          variant = NULL, alpha = NULL) {
  #library(igraph)
  # check the type of inputs
  # if data frames => crete an igraph obj
  # flows are data frame if nodes are given, else an igraph
  if ("igraph" %in% class(flows)){
    g <- flows
    if (igraph::gorder(g)==0) {stop("Call on an empty graph")}
  } else if (!("data.frame" %in% class(flows) && "data.frame" %in% class(nodes))){ #TODO: This should be 'or'
    stop("flows and nodes should be data frames or flows should be an igraph object.")
  } else if ("data.frame" %in% class(flows) && "data.frame" %in% class(nodes)){
    g <- igraph::graph_from_data_frame(flows, directed = T, vertices = nodes)
  }

  # Distance calculation
  if (!is.null(distance.calculation)) {
    g <- dist_calc(g, formula = distance.calculation)
  } else {
    if (is.null(igraph::E(g)$distance)) {
      g <- dist_calc(g)
    }
  }
  # Check if a variant is provided
  if (!is.null(variant) && length(variant) > 0) {
    if (!(is.character(variant))){stop('The variant argument must be a character or a vector of characters.')}
    for (v in variant) {
      if (nchar(v) != 3){stop('The variant argument must be a string of exactly 3 characters.')}
      if (!endsWith(v, 'g')){
        parsedVariant <- variantParser(v)
        if ((parsedVariant$weight.use == 'weighted') & (is.null(igraph::E(g)$weight))) {stop('To calculate a weighted index you need to provide "weights" attribute.')}
        g <- SDIcomputer(g, parsedVariant$level, parsedVariant$weight.use,
                         parsedVariant$directionality)
        } else {
                 # generalized variant
                 parsedVariant <- variantParser(v)
              if (is.null(igraph::E(g)$weight)){stop('To calculate a generalized index you need to provide "weights" attribute.')}
                 if (is.null(alpha)){stop('To calculate a generalized index you need to provide a "alpha" in SDI().')}
                 if (alpha < 0 | alpha > 1){stop('Alpha must be between 0 and 1.')}
                 sdiWeighted <- SDIcomputer(g, level = parsedVariant$level, directionality = parsedVariant$directionality,
                                         weight.use = 'weighted', return.value = TRUE)

                 sdiUnweighted <- SDIcomputer(g, level = parsedVariant$level, directionality = parsedVariant$directionality,
                                           weight.use = 'unweighted', return.value = TRUE)

                 calculatedSDI <- sdiUnweighted^alpha + sdiWeighted^(1-alpha)

                 if (parsedVariant$level == 'network'){
                   g <- igraph::set_graph_attr(g, name = paste0('SDI_n', substr(parsedVariant$directionality,1,1),'g'),
                                       value = calculatedSDI)
                 } else if (parsedVariant$level== 'vertex')
                   g <- igraph::set_vertex_attr(g, name = paste0('SDI_v', substr(parsedVariant$directionality,1,1), 'g'),
                                        value = calculatedSDI)
               }
    }
  } else {
    # Use the provided level, weight.use, and directionality
    if ((weight.use == 'weighted') & (is.null(igraph::E(g)$weight))){stop('To calculate a weighted index you need to provide "weights" attribute.')}

    if (!weight.use == 'generalized'){
    g <- SDIcomputer(g, level, weight.use, directionality)
    } else {
      if (is.null(alpha)){stop('To calculate a generalized index you need to provide a "alpha" in SDI().')}
      sdiWeighted <- SDIcomputer(g, level = level, directionality = directionality, weight.use = 'weighted', return.value = TRUE)
      sdiUnweighted <- SDIcomputer(g, level = level, directionality = directionality , weight.use = 'unweighted', return.value = TRUE)
      calculatedSDI <- sdiUnweighted^alpha + sdiWeighted^(1-alpha)

      if (level == 'network'){
        g <- igraph::set_graph_attr(g, name = paste0('SDI_n', substr(directionality,1,1),'g'),
                            value = calculatedSDI)
      } else if (level== 'vertex')
        g <- igraph::set_vertex_attr(g, name = paste0('SDI_v', substr(directionality,1,1), 'g'),
                             value = calculatedSDI)
    }
  }
  class(g) <- c("SDI", class(g))
  return(g)
}



## Functions used
# SDIhelpers are set of functions that simplfy the SDI.R file

#' SDIcomputer() is a helper function to compute given SDI variant for the given graph object.
#' Not intended for explicit use. Called automatically by the `SDI()` function.
#'
#' @param g An igraph object.
#' @param level The level to calculate the SDI. 'network' or 'vertex'.
#' @param weight.use 'weighted' or 'unweighted'.
#' @param directionality 'undirected', 'in', 'out', or 'all'.
#' @param return.value Logical. If TRUE, return the computed SDI value instead of modifying the graph object.
#'
#' @return If return.value is TRUE, returns the computed SDI value. Otherwise, returns the modified graph object.
#' @export
#'
#' @examples
#' TMgraph <- igraph::graph_from_data_frame(TurkiyeMigration.flows,
#'       directed=TRUE, TurkiyeMigration.nodes)
#' SDIcomputer(TMgraph,"vertex","weighted","in")
SDIcomputer <- function(g, level, weight.use, directionality,
                        return.value = FALSE) {
  if (level=="network") {
    if (weight.use=="weighted") {
      SDI_value <-  weightedNetworkSDI(g)
      if (return.value){return(SDI_value)}
      g <- igraph::set_graph_attr(g, name = 'SDI_nuw', value = SDI_value)
      return(g)
    }
    else if (weight.use=="unweighted"){
      SDI_value <- unweightedNetworkSDI(g)
      if (return.value){return(SDI_value)}
      g <- igraph::set_graph_attr(g, name = 'SDI_nuu', value = SDI_value)
      return(g)
    }
    else stop("Invalid 'weight.use' argument")
  } else if (level=="vertex") {
    if (weight.use=="weighted") {
      SDIname <-paste0('SDI_','v',substr(directionality,1,1),'w')
      SDI_value <- weightedAllVerticesSDI(g, mode = if (directionality == 'undirected') {'all'} else {directionality})
      if (return.value){return(SDI_value)}
      g <- igraph::set_vertex_attr(g, name = SDIname, value = SDI_value )
      return(g)
    }
    else if (weight.use=="unweighted") {
      SDIname <- paste0('SDI_','v',substr(directionality,1,1),'u')
      SDI_value <- unweightedAllVerticesSDI(g, mode=if (directionality == 'undirected') {'all'} else {directionality})
      if (return.value){return(SDI_value)}
      g <- igraph::set_vertex_attr(g, name = SDIname, value = SDI_value )
      return(g)
    }
    else stop("Invalid 'weight.use' argument")
  } else {
    stop("Invalid 'level' argument")
  }
}


#' variantParser for SDI variant short-codes.
#' This is a helper function and not intended for explicit use.
#'
#' @param variant a three letter short code for level, weight, and direction of SDI calculation.
#'
#' @return A list of explicit level, weight.use and directionality parameters
#' @export
#'
#' @examples
#' variantParser("vuw")
variantParser <- function(variant){
  levels <- c('network', 'vertex')
  weights <- c('weighted', 'unweighted', 'generalized')
  directions <- c('undirected', 'in', 'out', 'all')

  givenLevel <- substr(variant,1,1)
  givenDirection <- substr(variant,2,2)
  givenWeight <- substr(variant,3,3)

  if (!givenLevel %in% substr(levels,1,1)){
    stop('Invalid first letter for the variant "level": can be either "v"(vertex), or "n"(network).')
    } else if(!givenDirection %in% substr(directions,1,1)){
    stop('Invalid second letter for the variant "direction": can be "u"(undirected), "i"(in), or "o"(out)')
  } else if (!givenWeight %in% substr(weights,1,1)){
    stop('Invalid third letter entry for the variant "weight": can be either "w"(weighted),"u"(unweighted), or "g"(generalized)' )
  }

  level <- levels[startsWith(levels, givenLevel)]
  weight.use <- if (!givenWeight == 'g'){weights[startsWith(weights, givenWeight)]} else {NA}
  directionality <- directions[startsWith(directions, givenDirection)]

  return(list(
    level = level, weight.use = weight.use,
    directionality = directionality
    ))
}
