connected <- function(y, G, topo) {
  connected <- unique(unlist(neighborhood(G, order = vcount(G), nodes = y, mode = "all")))
  co <- V(G)[connected]$name
  co <- co %ts% topo
  return(co)
}
