sc.components <- function(D, topo) {
  .from <- NULL
  A <- as.matrix(igraph::get.adjacency(D))
  v <- igraph::get.vertex.attribute(D, "name")
  s <- v[which(igraph::vertex.attributes(D)$description == "S")]
  e <- igraph::E(D)
  bidirected <- NULL
  selection <- e[.from(s)]
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) {
      e[v[x[1]] %->% v[x[2]]]
    }))
  }
  D.bidirected <- igraph::subgraph.edges(D, union(bidirected, selection), delete.vertices = FALSE)
  subgraphs <- igraph::decompose.graph(D.bidirected)
  cc.s <- lapply(subgraphs, function(x) {
    v.sub <- igraph::get.vertex.attribute(x, "name")
    return(topo[which(topo %in% v.sub)])
  })
  cc.s.rank <- order(sapply(cc.s, function(x) {
    sum(which(topo %in% x))
  }), decreasing = TRUE)
  return(cc.s[cc.s.rank])
}
