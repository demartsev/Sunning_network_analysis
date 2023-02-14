#this is a script for calculating sunning social association and minimal path length

#not sure if all those are actually needed but I lost track at what function cones from where at this point.
#dont think it creates conflicts so will just leave everything here
library(gtools)
library(asnipe)
library(igraph)
library(CINNA)
library(reshape2)
library(sna)
library(ANTs)


simple_net <- cbind(c(0, 1, 0.5, 0.25), c(1, 0, 0, 0.5), c( 0.5, 0, 0, 0), c(0.25, 0.5, 0, 0))
  

##  #make network for igraph
net_simp <- graph_from_adjacency_matrix(simple_net  , mode = "upper", weighted = T)
met.geodesic(
  simple_net,
  weighted = T,
  shortest.weight = F, #considers the highest met.strength as the shortest path.
  normalization = T,
  directed = F,
  out = F
)


#plot
weights <- edge_attr(net_simp)$weight
plot.igraph(net_simp, main = "simple_net", edge.width = 10*(edge_attr(net_simp)$weight), 
            edge.labels = weights)
 



met.geodesic(
  simple_net,
  weighted = TRUE,
  shortest.weight = F, #considers the highest met.strength as the shortest path.
  normalization = T,
  directed = F,
  out = F
)


