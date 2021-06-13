idc <- function(y, x, z, P, G, G.obs, v, topo, tree, prune) {
  to <- NULL
  if (length(P$var) == 0) tree$call <- list(y = y, x = x, z = z, P = probability(var = v), G = G, line = "", v = v, id = FALSE)
  else tree$call <- list(y = y, x = x, z = z, P = P, G = G, line = "", v = v, id = FALSE)
  tree$branch <- list()
  offset <- (prune) * 2
  from <- NULL
  G.u <- unobserved.graph(G)
  edges.to.x <- igraph::E(G.u)[to(x)]
  nxt <- list()
  for (node in z) {
    cond <- setdiff(z, node)
    edges.from.node <- igraph::E(G.u)[from(node)]
    G.xz <- igraph::subgraph.edges(G.u, igraph::E(G.u)[setdiff(igraph::E(G.u), union(edges.to.x, edges.from.node))], delete.vertices = FALSE)
    if (wrap.dSep(G.xz, y, node, union(x, cond))) {
      tree$call$line <- 9 + offset
      tree$call$z.prime <- node
      nxt <- idc(y, union(x, node) %ts% topo, cond, P, G, G.obs, v, topo, list(), prune)
      tree$branch[[1]] <- nxt$tree
      tree$call$id <- nxt$tree$call$id
      return(list(P = nxt$P, tree = tree))
    }
  }
  if (prune) nxt <- pid(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list())
  else nxt <- id(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list())
  tree$call$line <- 10 + offset
  tree$call$id <- nxt$tree$call$id
  tree$branch[[1]] <- nxt$tree
  return(list(P = nxt$P, tree = tree))
}
