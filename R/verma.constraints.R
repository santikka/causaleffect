verma.constraints <- function(G) {
  if (length(igraph::edge_attr(G)) == 0) {
    G <- igraph::set_edge_attr(G, "description", 1:length(igraph::E(G)), NA)
  }
  G.obs <- observed.graph(G)
  G.unobs <- unobserved.graph(G)
  G.adj <- as.matrix(igraph::get.adjacency(G.unobs))
  topo <- igraph::topo_sort(G.obs)
  topo.u <- igraph::topo_sort(G.unobs)
  v <- igraph::vertex_attr(G, "name")[topo]
  v.unobs <- igraph::vertex_attr(G.unobs, "name")[topo.u]
  constraints <- list()
  s <- NULL
  for (i in 1:length(v)) {
    vi <- v[1:i]
    G.vi <- igraph::induced_subgraph(G, vi)
    cc <- c_components(G.vi, v)
    if (length(cc) > 1) s <- Find(function(x) v[i] %in% x, cc)
    else s <- cc[[1]]
    constraints <- c(constraints, q.constraints(s, v[i], G, G.obs, G.unobs, v, v.unobs, list()))
  }
  return(constraints)
}
