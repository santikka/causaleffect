c.components <- function(G, topo) {
  A <- as.matrix(igraph::get.adjacency(G))
  v <- igraph::get.vertex.attribute(G, "name")
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  bidirected <- NULL
  e <- igraph::E(G)
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) {
      e[v[x[1]] %->% v[x[2]]]
    }))
  }
  G.bidirected <- igraph::subgraph.edges(G, bidirected, delete.vertices = FALSE)
  subgraphs <- igraph::decompose.graph(G.bidirected)
  cc <- lapply(subgraphs, function(x) {
    v.sub <- igraph::get.vertex.attribute(x, "name")
    return(v.sub %ts% topo)
  })
  cc.rank <- order(sapply(cc, function(x) {
    sum(which(topo %in% x))
  }), decreasing = TRUE)
  return(cc[cc.rank])
}
