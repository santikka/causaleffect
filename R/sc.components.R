sc.components <- function(D, to) {
  from <- NULL
  A <- as.matrix(get.adjacency(D))
  v <- get.vertex.attribute(D, "name")
  s <- v[which(vertex.attributes(D)$description == "S")]
  e <- E(D)
  bidirected <- NULL
  selection <- e[from(s)]
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) {
      e[v[x[1]] %->% v[x[2]]]
    }))
  }
  D.bidirected <- subgraph.edges(D, union(bidirected, selection), delete.vertices = FALSE)
  subgraphs <- decompose.graph(D.bidirected)
  cc.s <- lapply(subgraphs, function(x) {
    v.sub <- get.vertex.attribute(x, "name")
    return(to[which(to %in% v.sub)])
  })
  return(cc.s)
}
