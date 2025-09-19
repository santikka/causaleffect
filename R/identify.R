identify <- function(C, T, Q, G, topo, tree) {
  v <- igraph::vertex_attr(G, "name")
  s <- v[which(igraph::vertex_attr(G)$description == "S")]
  v <- v %ts% topo
  G.obs <- observed.graph(G)
  G.T <- igraph::induced_subgraph(G, T)
  G.T.obs <- observed.graph(G.T)
  tree$call <- list(y = C, x = setdiff(v, C), C = C, T = T, P = activate.selection.variable(Q, s), G = G.T, line = "", v = v, alg = "Identify", id = FALSE)
  anc.c <- ancestors(C, G.T.obs, topo)
  A <- intersect(anc.c, T)
  tree$call$A <- A

  # i)
  if (identical(A, C)) {
    if (Q$product | Q$fraction) {
      Q$sumset <- union(setdiff(T, C), Q$sumset) %ts% topo
    } else {
      Q$var <- C
    }
    tree$call$line <- 9
    tree$call$id <- TRUE
    tree$call$anc.c <- anc.c
    tree$root <- Q
    return(list(P = Q, tree = tree))
  }

  # ii)
  if (identical(A, T)) {
    tree$call$line <- 10
    tree$call$id <- FALSE
    return(list(P = Q, tree = tree))
  }

  # iii)
  if (all(C %in% A) && all(A %in% T)) {
    G.A <- igraph::induced_subgraph(G, A)
    cc <- c_components(G.A, topo)
    T.prime <- Find(function(x) all(C %in% x), cc)
    T.one <- intersect(T.prime, A)
    Q.A <- Q
    if (Q.A$product | Q.A$fraction) {
      Q.A$sumset <- union(setdiff(T, A), Q.A$sumset) %ts% topo
    } else {
      Q.A$var <- A
    }
    Q.T.one <- compute.c.factor(T.one, A, Q.A, topo)
    nxt <- identify(C, T.one, Q.T.one, G, topo, list())
    tree$call$line <- 11
    tree$call$id <- nxt$tree$call$id
    tree$call$T.prime <- T.prime
    tree$call$T.one <- T.one
    tree$branch[[1]] <- nxt$tree
    return(list(P = nxt$P, tree = tree))
  }
}
