ancestors <- function(node, G.obs, topo) {
  an.ind <- unique(unlist(neighborhood(G.obs, order = vcount(G.obs), nodes = node, mode = "in")))
  an <- V(G.obs)[an.ind]$name
  an <- an %ts% topo
  return (an)
}
