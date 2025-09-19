tr.target <- function(Z, W, G.obs, G, topo.obs) {
  .to <- NULL
  .from <- NULL
  description <- NULL
  target <- setdiff(descendants(Z, G.obs, topo.obs), W)
  G.xbar <- igraph::subgraph_from_edges(G, igraph::E(G)[!(.to(Z) | (.from(Z) & (description == "U" & !is.na(description))))], delete.vertices = FALSE) # remove id nonid
  G.xbar.obs <- observed.graph(G.xbar)
  nontarget <- setdiff(ancestors(W, G.xbar.obs, topo.obs), target)
  cc <- c_components(G, topo.obs)
  for (Wi in W) {
    target <- union(target, Find(function(x) Wi %in% x, cc))
  }
  target <- setdiff(target, nontarget)
  return(target %ts% topo.obs)
}
