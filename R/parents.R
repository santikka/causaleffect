parents <- function(node, G.obs, topo) {
  if (caugi::is_caugi(G.obs)) {
    return(caugi.neighborhood(G.obs, node, "parents") %ts% topo)
  }
  pa.ind <- unique(unlist(igraph::neighborhood(
    G.obs,
    order = 1,
    nodes = node,
    mode = "in"
  )))
  pa <- igraph::V(G.obs)[pa.ind]$name
  pa <- pa %ts% topo
  return(pa)
}

parents_unsrt <- function(node, G.obs) {
  if (caugi::is_caugi(G.obs)) {
    return(caugi.neighborhood(G.obs, node, "parents"))
  }
  pa.ind <- unique(unlist(igraph::neighborhood(
    G.obs,
    order = 1,
    nodes = node,
    mode = "in"
  )))
  pa <- igraph::V(G.obs)[pa.ind]$name
  return(pa)
}
