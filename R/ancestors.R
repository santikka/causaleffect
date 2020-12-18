ancestors <- function(node, G, topo) {
  an.ind <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "in")))
  an <- igraph::V(G)[an.ind]$name
  an <- an %ts% topo
  return(an)
}

ancestors_unsrt <- function(node, G) {
  an.ind <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = node, mode = "in")))
  an <- igraph::V(G)[an.ind]$name
  return(an)
}
