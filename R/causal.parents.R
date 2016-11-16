causal.parents <- function(node, vi, G, G.obs, to) { 
  G.vi <- induced.subgraph(G, vi)
  cc <- c.components(G.vi, to)
  t <- Find(function(x) node %in% x, cc)
  pa.t <- parents(t, G.obs)
  pa <- setdiff(pa.t, node)
  pa <- to[which(to %in% pa)]
  return(pa)
}
