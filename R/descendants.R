descendants <- function(node, G, topo) {
  de.ind <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "out")))
  de <- igraph::V(G)[de.ind]$name
  de <- de %ts% topo
  return(de)
}
