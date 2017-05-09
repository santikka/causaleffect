connected <- function(w, y, G.xbar, topo) {
  connected <- unique(unlist(neighborhood(G.xbar, order = vcount(G.xbar), nodes = y, mode = "all")))
  co <- intersect(w, V(G.xbar)[connected]$name)
  co <- co %ts% topo
  return(co)
}
