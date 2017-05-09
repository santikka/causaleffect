descendants <- function(node, G.obs, topo) {
  de.ind <- unique(unlist(neighborhood(G.obs, order = vcount(G.obs), nodes = node, mode = "out")))
  de <- V(G.obs)[de.ind]$name
  de <- topo[which(topo %in% de)]
  return(de)
}
