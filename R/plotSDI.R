#' plotSDI generates a plot of the network and SDI metrics on a geographic map.
#'
#' @param g The igraph object to be plotted, whose vertices have attributes corresponding to SDI metrices.
#' @param variant The SDI variant with a prefix, such as "SDI_vuw", etc.
#' @param circle.size.scale Increase of decrease the size of circles drawn on nodes to represent SDI metric
#' @param circle.color Change color of circles
#' @param edges Whether to draw edges or not
#' @param edge.width.range If edges are to be drawn give a custom range of edge widths
#'
#' @return returns a ggplot2 plot
#'
#' @examples
#' TMSDI <- SDI(TurkiyeMigration.flows, TurkiyeMigration.nodes, variant="vuw")
#' plotSDI(TMSDI, variant="vuw", circle.size.scale=1)
#'
#' @export
plotSDI <- function(g, variant="", circle.size.scale=1, circle.color="red", edges=FALSE, edge.width.range=c(0.01, 0.5)) {
  if (!requireNamespace("igraph", quietly = TRUE)) {stop("igraph is required")}
  if (!requireNamespace("ggplot2", quietly = TRUE)) {stop("ggplot2 is required")}
  if (!requireNamespace("ggraph", quietly = TRUE)) {stop("ggraph is required")}
  #library(ggraph)
  world_map <- ggplot2::map_data("world")
  z<-igraph::get.vertex.attribute(g,paste0("SDI_",variant))
  normalsize <- 7*z/mean(z,na.rm=TRUE)
  weight<-igraph::E(g)$weight
  lay <- ggraph::create_layout(g, 'manual', x=igraph::V(g)$longitude, y=igraph::V(g)$latitude)
  p<- ggraph::ggraph(lay) +
    ggplot2::geom_path(ggplot2::aes(x = world_map$long, y = world_map$lat, group = world_map$group), data = world_map,  color = "black", na.rm=TRUE) +
    ggplot2::scale_color_identity() +
    ggplot2::scale_x_continuous(limits = c(min(igraph::V(g)$longitude)-2, max(igraph::V(g)$longitude)+2))+
    ggplot2::scale_y_continuous(limits = c(min(igraph::V(g)$latitude)-2, max(igraph::V(g)$latitude)+2))+
    ggraph::geom_node_text(label=igraph::V(g)$label, size=2)+
    ggplot2::xlab("") +
    ggplot2::ylab("")
  if (edges) {
    p<- p + ggraph::geom_edge_link(ggplot2::aes(width = weight/max(weight)),arrow = ggplot2::arrow(length = ggplot2::unit(5, 'mm')))+
      ggraph::scale_edge_width(range = edge.width.range)
  }
  p+
    ggraph::geom_node_point(size = normalsize*circle.size.scale, ggplot2::aes(color=circle.color),alpha=0.4)+
    ggplot2::theme(legend.position = "none")
  #p+coord_fixed()
}

