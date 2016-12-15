identify <- function(C, T, Q, G, to, tree) {
  v <- get.vertex.attribute(G, "name")
  s <- v[which(vertex.attributes(G)$description == "S")]
  v <- v %ts% to
  G.obs <- observed.graph(G)
  G.T <- induced.subgraph(G, T)
  G.T.obs <- observed.graph(G.T)
  tree$call <- list(y = C, x = setdiff(v, C), C = C, T = T, P = activate.selection.variable(Q, s), G = G.T, line = "", v = v, alg = "Identify")
  anc.c <- ancestors(C, G.T.obs, to)
  A <- intersect(anc.c, T)
  tree$call$A <- A

  # i)
  if (identical(A, C)) {
    if (Q$product | Q$fraction) {
      Q$sumset <- union(setdiff(T, C), Q$sumset) %ts% to
    } else {
      Q$var <- C
    }
    tree$call$line <- 9
    tree$call$anc.c <- anc.c
    tree$root <- Q
    return(list(P = Q, tree = tree))
  }

  # ii)
  if (identical(A, T)) stop("Cannot compute Q[C].", call. = FALSE)

  # iii)
  if (all(C %in% A) && all(A %in% T)) {
    G.A <- induced.subgraph(G, A)
    cc <- c.components(G.A, to)
    T.prime <- Find(function(x) all(C %in% x), cc)
    T.one <- intersect(T.prime, A)
    Q.A <- Q
    if (Q.A$product | Q.A$fraction) {
      Q.A$sumset <- union(setdiff(T, A), Q.A$sumset) %ts% to
    } else {
      Q.A$var <- A
    }
    Q.T.one <- compute.c.factor(T.one, A, Q.A, to)
    nxt <- identify(C, T.one, Q.T.one, G, to, list())
    tree$call$line <- 11
    tree$call$T.prime <- T.prime
    tree$call$T.one <- T.one
    tree$branch[[1]] <- nxt$tree
    return(list(P = nxt$P, tree = tree))
  }
}