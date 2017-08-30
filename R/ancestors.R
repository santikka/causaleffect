ancestors <- function(node, G, topo) {
  an.ind <- unique(unlist(neighborhood(G, order = vcount(G), nodes = node, mode = "in")))
  an <- V(G)[an.ind]$name
  an <- an %ts% topo
  return (an)
}
