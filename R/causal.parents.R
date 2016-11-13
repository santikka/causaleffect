causal.parents <-
function(y, vi, G, G.obs, to) { 
  G.vi <- induced.subgraph(G, vi)
  cc <- c.components(G.vi, to)
  t <- Find(function(x) all(y %in% x), cc)
  parents <- unique(unlist(neighborhood(G.obs, order = 1, nodes = t, mode = "in")))
  pa.nodes <- setdiff(V(G.obs)[parents]$name, y)
  pa.nodes <- to[which(to %in% pa.nodes)]
  return(pa.nodes)
}
