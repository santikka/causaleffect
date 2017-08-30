children <- function(node, G, topo) {
  ch.ind <- unique(unlist(neighborhood(G, order = 1, nodes = node, mode = "out")))
  ch <- V(G)[ch.ind]$name
  ch <- ch %ts% topo
  return(ch)
}
