sc.components <- function(D, to) {
  from <- NULL
  A <- as.matrix(get.adjacency(D))
  v <- get.vertex.attribute(D, "name")
  s <- v[which(vertex.attributes(D)$description == "S")]
  e <- E(D)
  selection <- E(D)[from(s)]
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  bidirected <- e[v[indices[ ,1]] %->% v[indices[ ,2]]]
  D.bidirected <- subgraph.edges(D, union(bidirected, selection), delete.vertices = FALSE)
  subgraphs <- decompose.graph(D.bidirected)
  cc.s <- lapply(subgraphs, function(x) {
    v.sub <- get.vertex.attribute(x, "name")
    return(to[which(to %in% v.sub)])
  })
  return(cc.s)
}
