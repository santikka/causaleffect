verma.constraints <- function(G) {
  if (length(igraph::edge.attributes(G)) == 0) {
    G <- igraph::set.edge.attribute(G, "description", 1:length(igraph::E(G)), NA)
  }
  G.obs <- observed.graph(G)
  G.unobs <- unobserved.graph(G)
  G.adj <- as.matrix(igraph::get.adjacency(G.unobs))
  topo <- igraph::topological.sort(G.obs)
  topo.u <- igraph::topological.sort(G.unobs)
  v <- igraph::get.vertex.attribute(G, "name")[topo]
  v.unobs <- igraph::get.vertex.attribute(G.unobs, "name")[topo.u]
  constraints <- list()
  s <- NULL
  for (i in 1:length(v)) {
    vi <- v[1:i]
    G.vi <- igraph::induced.subgraph(G, vi)
    cc <- c.components(G.vi, v)
    if (length(cc) > 1) s <- Find(function(x) v[i] %in% x, cc)
    else s <- cc[[1]]
    constraints <- c(constraints, q.constraints(s, v[i], G, G.obs, G.unobs, v, v.unobs, list()))
  }
  return(constraints)
}