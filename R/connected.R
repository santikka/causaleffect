connected <- function(y, G, topo) {
  connected <- unique(unlist(igraph::neighborhood(G, order = igraph::vcount(G), nodes = y, mode = "all")))
  co <- igraph::V(G)[connected]$name
  co <- co %ts% topo
  return(co)
}
