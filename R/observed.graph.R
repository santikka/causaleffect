observed.graph <- function(G) {
  if (caugi::is_caugi(G)) {
    e <- as.data.frame(caugi::edges(G))
    e <- e[e$edge == "<->", , drop = FALSE]
    if (nrow(e) == 0L) {
      return(G)
    }
    return(caugi::remove_edges(G, from = e$from, edge = e$edge, to = e$to))
  }
  obs.edges <- setdiff(
    igraph::E(G),
    igraph::E(G)[which(igraph::edge_attr(G)$description == "U")]
  )
  G.obs <- igraph::subgraph_from_edges(
    G,
    igraph::E(G)[obs.edges],
    delete.vertices = FALSE
  )
  return(G.obs)
}
