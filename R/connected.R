connected <- function(w, y, G.xbar, to) {
  connected <- unique(unlist(neighborhood(G.xbar, order = vcount(G.xbar), nodes = y, mode = "all")))
  co <- intersect(w, V(G.xbar)[connected]$name)
  co <- co %ts% to
  return(co)
}
