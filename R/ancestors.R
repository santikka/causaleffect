ancestors <- function(node, G, topo) {
  if (caugi::is_caugi(G)) {
    return(caugi.neighborhood(G, node, "ancestors") %ts% topo)
  }
  an.ind <- unique(unlist(igraph::neighborhood(
    G,
    order = igraph::vcount(G),
    nodes = node,
    mode = "in"
  )))
  an <- igraph::V(G)[an.ind]$name
  an <- an %ts% topo
  return(an)
}

ancestors_unsrt <- function(node, G) {
  if (caugi::is_caugi(G)) {
    return(caugi.neighborhood(G, node, "ancestors"))
  }
  an.ind <- unique(unlist(igraph::neighborhood(
    G,
    order = igraph::vcount(G),
    nodes = node,
    mode = "in"
  )))
  an <- igraph::V(G)[an.ind]$name
  return(an)
}
