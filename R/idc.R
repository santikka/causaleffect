idc <- function(y, x, z, P, G, G.obs, v, topo, tree, prune) {
  to <- NULL
  if (length(P$var) == 0) tree$call <- list(y = y, x = x, z = z, P = probability(var = v), G = G, line = "", v = v, id = FALSE)
  else tree$call <- list(y = y, x = x, z = z, P = P, G = G, line = "", v = v, id = FALSE)
  tree$branch <- list()
  offset <- (prune) * 2
  from <- NULL
  G.xz <- unobserved.graph(G)
  edges.to.x <- igraph::E(G.xz)[to(x)]
  edges.from.z <- igraph::E(G.xz)[from(z)]
  G.xz <- igraph::subgraph.edges(G.xz, igraph::E(G.xz)[setdiff(igraph::E(G.xz), union(edges.to.x, edges.from.z))], delete.vertices = FALSE)
  nxt <- list()
  for (node in z) {
    cond <- setdiff(z, node)
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
