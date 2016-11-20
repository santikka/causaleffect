c.components <- function(G, to) {
  A <- as.matrix(get.adjacency(G))
  v <- get.vertex.attribute(G, "name")
  e <- E(G)
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  bidirected <- e[v[indices[ ,1]] %->% v[indices[ ,2]]]
  G.bidirected <- subgraph.edges(G, bidirected, delete.vertices = FALSE)
  subgraphs <- decompose.graph(G.bidirected)
  cc <- lapply(subgraphs, function(x) {
    v.sub <- get.vertex.attribute(x, "name")
    return(to[which(to %in% v.sub)])
  })
  return(cc)
}
