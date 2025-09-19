unobserved.graph <- function(G) {
  unobs.edges <- which(igraph::edge.attributes(G)$description == "U")
  u <- length(unobs.edges)
  if (u > 0) {
    e <- igraph::get.edges(G, unobs.edges)
    e <- e[e[ ,1] > e[ ,2], , drop = FALSE]
    e.len <- nrow(e)
    new.nodes <- paste0("u_{", 1:e.len, "}")
    G <- G + igraph::vertices(new.nodes, description = rep("U", e.len))
    v <- igraph::vertex_attr(G, "name")
    G <- G + igraph::edges(c(rbind(new.nodes, v[e[ ,1]]), rbind(new.nodes, v[e[ ,2]])))
    obs.edges <- setdiff(igraph::E(G), igraph::E(G)[unobs.edges])
    G.unobs <- igraph::subgraph_from_edges(G, igraph::E(G)[obs.edges], delete.vertices = FALSE)
    return(G.unobs)
  }
  return(G)
}
