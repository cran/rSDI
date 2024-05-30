test_that("SDI call with missing nodes", {
  expect_error(SDI(data.frame()), "flows and nodes should be data frames or flows should be an igraph object.")
})

test_that("SDI call with empty graph", {
  tg<-igraph::make_empty_graph(n = 0, directed = TRUE)
  expect_error(SDI(tg), "Call on an empty graph")
})

flows<-data.frame(from=c("A","B","A"), to=c("B","A","C"), weight=c(10,20,5))
nodes<-data.frame(id=c("A","B","C","D"),x=c(0,4,0,4),y=c(3,0,0,3))
library(igraph)
g<-graph_from_data_frame(flows, directed=TRUE, vertices=nodes)

test_that("distance calc", {
  # 'formula' is a special name in R, don't use
  expect_equal(E(dist_calc(g))$distance, c(5,5,3))
})

test_that("SDI-vwa", {
  expect_equal(V(SDI(g))$SDI_vuw, c(4.71428571428571, 5, 3,NA))
})
