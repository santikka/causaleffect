causal.parents <- function(node, vi, G, G.obs, topo) {
  G.vi <- induced.subgraph(G, vi)
  cc <- c.components(G.vi, topo)
  t <- Find(function(x) node %in% x, cc)
  pa.t <- parents(t, G.obs)
  pa <- setdiff(pa.t, node)
  pa <- topo[which(topo %in% pa)]
  return(pa)
}
