# rSDI: Spatial Dispersion Index (SDI) for R

This package provides functions for calculating Spatial Dispersion Index ([Gençer, 2023](https://dx.doi.org/10.1007/s12061-023-09545-8)) for R, and its visualization.

Spatial Dispersion Index (SDI) is a family of indices to measure spatial range of relations in a spatial/geographic network. The index was developed as part of a large-scale socio-economic study on flows between cities, and has proven itself useful for evaluating spatial range of flows for the whole networ or its nodes.

This package provides all variants of SDI as described in the paper (Gençer, 2023) and also provides functions to visualize index measurements on a network visualization plotted as a graph on a geographic map or a spatial base plate of your choosing.

## Installation

Install latest release from CRAN:

```{r}
install.packages("rSDI")
```

or if you want to install the development version:
```{r}
install_github("ehengirmen/rSDI")
```

## Usage

To use rSDI you will need either (i) two data frames, one containing flows between vertices of a geographic/spatial network and another containing coordinates and labels of vertices, or (i) an igraph object having appropriate vertex and edge attributes for rSDI computation. For vertices one needs either (x,y) or (longitude,latitude) attributes and for edges a weight attribute representing flows. 

The package provides a dataset of migration between Turkish cities containing two data frames:

```{r}
head(TurkiyeMigration.flows)
head(TurkiyeMigration.nodes)
```

You can use these in tow ways described above as:
```{r echo=T, eval=T}
TMSDI <- SDI(TurkiyeMigration.flows, TurkiyeMigration.nodes, variant="vuw")
#   -- OR --
library(igraph)
TMgraph <- graph_from_data_frame(TurkiyeMigration.flows, directed=TRUE, TurkiyeMigration.nodes)
TMSDI <- SDI(TMgraph, variant="vuw")
```
Refer to documentation for calculating different SDI variants

If you want to use visualization features of rSDI package make sure you have installed ggraph and maps packages, then invoke plotSDI as follows to visualize a particular SDI variant computed previously:

```{r echo=T, eval=T}
plotSDI(TMSDI, variant="vuw")
```

Please refer to plot.SDI documentation for parameters to tweak visualization features.
