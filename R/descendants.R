descendants <- function(node, G, topo) {
  de.ind <- unique(unlist(neighborhood(G, order = vcount(G), nodes = node, mode = "out")))
  de <- V(G)[de.ind]$name
  de <- topo[which(topo %in% de)]
  return(de)
}
