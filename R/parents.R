parents <-
function(y, G, to) { 
  parents <- unique(unlist(neighborhood(G, order = 1, nodes = y, mode = "in")))
  v <- setdiff(V(G)[parents]$name, y)
  v <- to[which(to %in% v)]
  return(v)
}
