c_components <- function(G, topo) {
  if (caugi::is_caugi(G)) {
    cc <- lapply(caugi::districts(G), function(x) x %ts% topo)
    # Component ties follow vertex order, as in igraph::decompose().
    v <- caugi::nodes(G)$name
    cc <- cc[order(vapply(cc, function(x) min(match(x, v)), numeric(1)))]
    rank <- vapply(cc, function(x) sum(which(topo %in% x)), numeric(1))
    return(cc[order(rank, decreasing = TRUE)])
  }
  A <- as.matrix(igraph::as_adjacency_matrix(G))
  v <- igraph::vertex_attr(G, "name")
  indices <- which(A >= 1 & t(A) >= 1, arr.ind = TRUE)
  bidirected <- NULL
  e <- igraph::E(G)
  if (nrow(indices) > 0) {
    bidirected <- unlist(apply(indices, 1, function(x) e[v[x[1]] %->% v[x[2]]]))
  }
  G.bidirected <- igraph::subgraph_from_edges(
    G,
    bidirected,
    delete.vertices = FALSE
  )
  subgraphs <- igraph::decompose(G.bidirected)
  cc <- lapply(subgraphs, function(x) {
    v.sub <- igraph::vertex_attr(x, "name")
    return(v.sub %ts% topo)
  })
  cc.rank <- order(
    sapply(cc, function(x) sum(which(topo %in% x))),
    decreasing = TRUE
  )
  return(cc[cc.rank])
}
