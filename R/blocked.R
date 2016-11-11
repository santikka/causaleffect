blocked <- function(w, y, G.x.overbar, to) {
  nv <- vcount(G.x.overbar)
  connected <- unique(unlist(neighborhood(G.x.overbar, order = nv, nodes = y, mode = "in")))
  r <- V(G.x.overbar)[setdiff(1:nv, connected)]$name
  r <- to[which(to %in% r)]
  return (r)
}
