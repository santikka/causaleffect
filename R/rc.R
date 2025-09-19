rc <- function(D, P, G, topo, tree) {
  v.s <- igraph::vertex_attr(G, "name")
  s <- v.s[which(igraph::vertex.attributes(G)$description == "S")]
  G.causal <- igraph::induced_subgraph(G, v.s[!(v.s %in% s)])
  v <- igraph::vertex_attr(G.causal, "name")
  v <- v %ts% topo
  G.obs <- observed.graph(G.causal)
  G.s.obs <- observed.graph(G)
  anc.d.obs <- ancestors(D, G.obs, topo)
  anc.d <- ancestors(D, G.s.obs, topo)
  tree$call <- list(y = D, x = setdiff(v, D), P = activate.selection.variable(P, s), G = G, line = "", v = v.s, alg = "RC", id = FALSE)

  # line 1
  anc.s <- ancestors(s, G.s.obs, topo)
  anc.union <- union(anc.s, anc.d) %ts% topo
  if (length(setdiff(v.s, anc.union)) != 0) {
    if (P$product | P$fraction) {
      P$sumset <- setdiff(v.s, anc.union) %ts% topo
    } else {
      P$var <- anc.union
    }
    nxt <- rc(D, P, igraph::induced_subgraph(G, anc.union), topo, list())
    tree$call$line <- 2
    tree$call$id <- nxt$tree$call$id
    tree$call$anc.d <- anc.d
    tree$call$and.s <- anc.s
    tree$branch[[1]] <- nxt$tree
    return(list(P = nxt$P, tree = tree))
  }

  # line 2
  cc <- c.components(G.causal, topo)
  cg <- length(cc)
  c.set <- c()
  c.ind <- c()
  for (i in 1:cg) {
    if (length(intersect(anc.s, cc[[i]])) == 0) {
      c.set <- union(c.set, cc[[i]]) %ts% topo
      c.ind <- c(c.ind, i)
      # line 4
      if (all(D %in% cc[[i]])) {
        nxt <- identify(D, cc[[i]], compute.c.factor(cc[[i]], v, P, topo), G, topo, list())
        tree$call$line <- 5
        tree$call$id <- nxt$tree$call$id
        tree$call$c.i <- cc[[i]]
        tree$branch[[1]] <- nxt$tree
        return(list(P = nxt$P, tree = tree))
      }
    }
  } 

  # line 3
  if (length(c.set) == 0) {
    tree$call$line <- 4
    tree$call$id <- FALSE
    tree$root <- P
    return(list(P = P, tree = tree))
  }

  # line 5
  c.len <- length(c.ind)
  product.list <- vector(mode = "list", length = c.len)
  ind <- 1
  for (i in c.ind) {
    product.list[[ind]] <- compute.c.factor(cc[[i]], v, P, topo)
    ind <- ind + 1
  }
  P.new <- probability(fraction = TRUE)
  P.new$num <- P
  if (c.len > 1) {
    P.new$den <- probability(product = TRUE, children = product.list)
  } else {
    P.new$den <- product.list[[1]]
  }
  nxt <- rc(D, P.new, igraph::induced_subgraph(G, setdiff(v.s, c.set)), topo, list())
  tree$call$line <- 6
  tree$call$id <- nxt$tree$call$id
  tree$call$c.set <- c.set
  tree$branch[[1]] <- nxt$tree
  return(list(P = nxt$P, tree = tree))
}
