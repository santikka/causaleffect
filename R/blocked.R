blocked <- function(w, y, G.xbar, to) {
  nv <- vcount(G.xbar)
  connected <- unique(unlist(neighborhood(G.xbar, order = nv, nodes = y, mode = "all")))
  r <- V(G.xbar)[setdiff(1:nv, connected)]$name
  r <- r %ts% to
  return (r)
}
