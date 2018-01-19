c.components <- function(G, topo) {
  A <- as.matrix(get.adjacency(G))
  v <- get.vertex.attribute(G, "name")
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  bidirected <- NULL
  e <- E(G)
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) {
      e[v[x[1]] %->% v[x[2]]]
    }))
  }
  G.bidirected <- subgraph.edges(G, bidirected, delete.vertices = FALSE)
  subgraphs <- decompose.graph(G.bidirected)
  cc <- lapply(subgraphs, function(x) {
    v.sub <- get.vertex.attribute(x, "name")
    return(v.sub %ts% topo)
  })
  cc.rank <- order(sapply(cc, function(x) {
    sum(which(topo %in% x))
  }), decreasing = TRUE)
  return(cc[cc.rank])
}
