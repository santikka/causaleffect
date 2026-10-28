idc <- function(y, x, z, P, G, G.obs, v, topo, tree, prune) {
  if (length(P$var) == 0) {
    tree$call <- list(
      y = y,
      x = x,
      z = z,
      P = probability(var = v),
      G = G,
      line = "",
      v = v,
      id = FALSE
    )
  } else {
    tree$call <- list(
      y = y,
      x = x,
      z = z,
      P = P,
      G = G,
      line = "",
      v = v,
      id = FALSE
    )
  }
  tree$branch <- list()
  offset <- (prune) * 2
  G.xbar <- intervene.graph(G, incoming = x)
  nxt <- list()
  for (node in z) {
    cond <- setdiff(z, node)
    G.xz <- intervene.graph(G.xbar, outgoing = node)
    if (wrap.dSep(G.xz, y, node, union(x, cond))) {
      tree$call$line <- 9 + offset
      tree$call$z.prime <- node
      nxt <- idc(
        y,
        union(x, node) %ts% topo,
        cond,
        P,
        G,
        G.obs,
        v,
        topo,
        list(),
        prune
      )
      tree$branch[[1]] <- nxt$tree
      tree$call$id <- nxt$tree$call$id
      return(list(P = nxt$P, tree = tree))
    }
  }
  if (prune) {
    nxt <- pid(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list())
  } else {
    nxt <- id(union(y, z) %ts% topo, x, P, G, G.obs, v, topo, list())
  }
  tree$call$line <- 10 + offset
  tree$call$id <- nxt$tree$call$id
  tree$branch[[1]] <- nxt$tree
  return(list(P = nxt$P, tree = tree))
}
