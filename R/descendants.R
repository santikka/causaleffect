descendants <- function(node, G, topo) {
  if (caugi::is_caugi(G)) {
    return(caugi.neighborhood(G, node, "descendants") %ts% topo)
  }
  de.ind <- unique(unlist(igraph::neighborhood(
    G,
    order = igraph::vcount(G),
    nodes = node,
    mode = "out"
  )))
  de <- igraph::V(G)[de.ind]$name
  de <- de %ts% topo
  return(de)
}
