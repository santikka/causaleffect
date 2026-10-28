children <- function(node, G, topo) {
  if (caugi::is_caugi(G)) {
    return(caugi.neighborhood(G, node, "children") %ts% topo)
  }
  ch.ind <- unique(unlist(igraph::neighborhood(
    G,
    order = 1,
    nodes = node,
    mode = "out"
  )))
  ch <- igraph::V(G)[ch.ind]$name
  ch <- ch %ts% topo
  return(ch)
}

children_unsrt <- function(node, G) {
  if (caugi::is_caugi(G)) {
    return(caugi.neighborhood(G, node, "children"))
  }
  ch.ind <- unique(unlist(igraph::neighborhood(
    G,
    order = 1,
    nodes = node,
    mode = "out"
  )))
  ch <- igraph::V(G)[ch.ind]$name
  return(ch)
}
