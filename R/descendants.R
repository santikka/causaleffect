descendants <- function(node, G.obs, to) { 
  de.ind <- unique(unlist(neighborhood(G.obs, order = vcount(G.obs), nodes = node, mode = "out")))
  de <- V(G.obs)[de.ind]$name
  de <- to[which(to %in% de)]
  return(de)
}
