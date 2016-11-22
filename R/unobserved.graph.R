unobserved.graph <- function(G) {
  unobs.edges <- which(edge.attributes(G)$description == "U")
  u <- length(unobs.edges)
  if (u > 0) {
    e <- get.edges(G, unobs.edges)
    e <- e[e[ ,1] > e[ ,2], , drop = FALSE]
    e.len <- nrow(e)
    new.nodes <- paste0("u_{", 1:e.len, "}")
    G <- G + vertices(new.nodes, description = rep("U", e.len))
    v <- get.vertex.attribute(G, "name")
    G <- G + edges(c(rbind(new.nodes, v[e[ ,1]]), rbind(new.nodes, v[e[ ,2]])))
    obs.edges <- setdiff(E(G), E(G)[unobs.edges])
    G.unobs <- subgraph.edges(G, E(G)[obs.edges], delete.vertices = FALSE)
    return(G.unobs)
  }
  return(G)
}
