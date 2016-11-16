ancestors <- function(node, G.obs, to) { 
  an.ind <- unique(unlist(neighborhood(G.obs, order = vcount(G.obs), nodes = node, mode = "in")))
  an <- V(G.obs)[an.ind]$name
  an <- to[which(to %in% an)]
  return(an)
}
