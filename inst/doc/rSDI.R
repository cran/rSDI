## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo=F,
  warning = F,
  message = F
)

## ----setup--------------------------------------------------------------------
library(rSDI)
#devtools::load_all()

## ----echo=TRUE, eval=FALSE----------------------------------------------------
#  library(rSDI)
#  SDI(TurkiyeMigration.flows, TurkiyeMigration.nodes, variant="vow") %>% plotSDI(variant="vow")

## -----------------------------------------------------------------------------
flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
flowsWithDistances<-flows
flowsWithDistances$distance <- c(5,5,3)
library(igraph)
g<-graph_from_data_frame(flows, directed=TRUE, vertices=nodes)
library(ggplot2)
library(ggraph)
library(ggimage)
url<-"https://static.wikia.nocookie.net/lotr/images/5/59/Middle-earth.jpg/revision/latest?cb=20060726004750"

lay <- create_layout(g, 'manual', x=V(g)$x, y=V(g)$y)
p<-ggraph(lay) +
  geom_edge_bend(aes(label=E(g)$weight), label_size=10,strength=0.4,edge_width=3,alpha=0.3,arrow = arrow(length = unit(10, 'mm')))+
  #geom_node_point(size = 10, aes(color="yellow"),alpha=0.4) +
  geom_node_point(size=10)+
  geom_node_text(label=V(g)$name, size=10, vjust=-0.7,hjust=1)+
  xlim(-3,5)+ylim(-2,4)
p<-ggbackground(p, url) #see https://stackoverflow.com/questions/51255832/how-to-add-an-image-on-ggplot-background-not-the-panel

## ----fig.show="hold", out.width="50%"-----------------------------------------
knitr::kable(list(flows,nodes),booktabs = TRUE, valign = 't',caption = 'Data frames providing flows (left) and nodes (right) for an imaginary spatial network')

## ----fig.width=7, fig.height=5------------------------------------------------
p

## -----------------------------------------------------------------------------
knitr::kable(flowsWithDistances,booktabs = TRUE, valign = 't',caption = 'The flow data with distances between the source and target location of  flows added')

## ----echo=TRUE----------------------------------------------------------------
flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
library(igraph)
toyGraph <- graph_from_data_frame(flows, directed=TRUE, vertices=nodes)

## -----------------------------------------------------------------------------
edge_attr_names(toyGraph)

## ----SDI.with.defaults, eval=T, echo=TRUE-------------------------------------
toyGraphWithSDI <- SDI(toyGraph) #same as SDI(toyGraph, level="vertex", directionality="undirected", weight.use="weighted")
edge_attr_names(toyGraphWithSDI)
vertex_attr_names(toyGraphWithSDI)

## ----eval=T, echo=TRUE--------------------------------------------------------
vertex_attr(toyGraphWithSDI, "SDI_vuw")

## ----SDI.at.network.level, eval=T, echo=TRUE----------------------------------
toyGraphWithNetworkSDI <- SDI(toyGraph, level="network", directionality="undirected", weight.use="weighted")
graph_attr_names(toyGraphWithNetworkSDI) 
graph_attr(toyGraphWithNetworkSDI,"SDI_nuw")

## ----eval=T, echo=TRUE--------------------------------------------------------
toyGraphWithNetworkSDI <- SDI(toyGraph, variant="nuw")

## ----multi.metric.example, eval=T, echo=TRUE----------------------------------
toyGraph %>% 
  SDI(variant="nuw") %>%
  SDI(variant="niu") %>% # nuu?
  SDI(variant="vuw") %>%
  SDI(variant="vuu") -> toyGraphWithSeveralSDI
graph_attr_names(toyGraphWithSeveralSDI)
vertex_attr_names(toyGraphWithSeveralSDI)

## ----multi.metric.example2, eval=T, echo=TRUE---------------------------------
toyGraphWithSeveralSDI <- SDI(toyGraph, variant=c("nuw","niu","vuw","vuu"))
graph_attr_names(toyGraphWithSeveralSDI)
vertex_attr_names(toyGraphWithSeveralSDI)

## ----generalized.SDI.example, eval=T, echo=TRUE-------------------------------
toyGraphWithGeneralizedSDI <- SDI(toyGraph, variant="vug", alpha=0.5) 
vertex_attr_names(toyGraphWithGeneralizedSDI) 
vertex_attr(toyGraphWithGeneralizedSDI,"SDI_vug")

## ----eval=T, echo=TRUE--------------------------------------------------------
toyGraphWithDistances <- dist_calc(toyGraph)
edge_attr_names(toyGraphWithDistances)

## ----eval=T, echo=TRUE--------------------------------------------------------
edge_attr(toyGraphWithDistances, "distance")

## ----eval=T, echo=TRUE--------------------------------------------------------
head(TurkiyeMigration.flows)
head(TurkiyeMigration.nodes)

## ----echo=T, eval=T-----------------------------------------------------------
TMSDI <- SDI(TurkiyeMigration.flows, TurkiyeMigration.nodes, variant="vuw")
#   -- OR --
library(igraph)
TMgraph <- graph_from_data_frame(TurkiyeMigration.flows, directed=TRUE, TurkiyeMigration.nodes)
TMSDI <- SDI(TMgraph, variant="vuw")

## ----echo=T, eval=T-----------------------------------------------------------
plotSDI(TMSDI, variant="vuw", circle.size.scale=1)

## ----echo=T, eval=T-----------------------------------------------------------
plotSDI(TMSDI, variant="vuw", edges=TRUE)

## ----echo=T-------------------------------------------------------------------
flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
g <- SDI(flows,nodes, variant="vuw")
library(ggplot2)
library(ggraph)
library(ggimage)
url<-"https://static.wikia.nocookie.net/lotr/images/5/59/Middle-earth.jpg/revision/latest?cb=20060726004750"

lay <- create_layout(g, 'manual', x=V(g)$x, y=V(g)$y)
p<-ggraph(lay) +
  geom_edge_bend(aes(label=E(g)$weight), label_size=10,strength=0.4,edge_width=3,alpha=0.3,arrow = arrow(length = unit(10, 'mm')))+
  #geom_node_point(size = 10, aes(color="yellow"),alpha=0.4) +
  geom_node_point(aes(size=V(g)$SDI_vuw),color="red")+
  geom_node_text(label=V(g)$name, size=10, vjust=-0.7,hjust=1)+
  xlim(-3,5)+ylim(-2,4)
p<-ggbackground(p, url) 
p

