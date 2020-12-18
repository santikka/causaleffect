observed.graph <- function(G) {
  obs.edges <- setdiff(igraph::E(G), igraph::E(G)[which(igraph::edge.attributes(G)$description == "U")])
  G.obs <- igraph::subgraph.edges(G, igraph::E(G)[obs.edges], delete.vertices = FALSE)
  return(G.obs)
}
