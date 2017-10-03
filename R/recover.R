recover <- function(y, x, G, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE) {
  if (length(edge.attributes(G)) == 0) {
    G <- set.edge.attribute(G, "description", 1:length(E(G)), NA)
  }
  if (!is.dag(observed.graph(G))) stop("Graph 'G' is not a DAG")
  sel <- which(vertex.attributes(G)$description == "S")
  if (length(sel) == 0) stop("No selection variables present in the diagram.")
  if (length(sel) > 1) stop("Multiple selection variables are not supported.")
  G.obs <- observed.graph(G)
  topo <- topological.sort(G.obs)
  v.s <- get.vertex.attribute(G, "name")
  topo <- v.s[topo]
  if (length(setdiff(y, topo)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, topo)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint. ")
  s <- v.s[sel]
  s <- topo[which(topo %in% s)]
  G.causal <- induced.subgraph(G, v.s[!(v.s %in% s)])
  v <- get.vertex.attribute(G.causal, "name")
  v <- topo[which(topo %in% v)]
  T.prime <- setdiff(v, x)
  G.T.prime <- induced.subgraph(G.causal, T.prime)
  G.T.prime.obs <- observed.graph(G.T.prime)
  D <- ancestors(y, G.T.prime.obs, topo)
  G.D <- induced.subgraph(G.causal, D)
  cc <- c.components(G.D, topo)
  cg <- length(cc)
  res <- probability()
  tree <- list()
  tree$call <- list(y = y, x = x, d = D, T.prime = T.prime, 
    P = probability(var = v, cond = paste0(s, " = 1")), G = G, line = 1, v = v)
  product.list <- list()
  nxt.list <- list()
  for (i in 1:cg) {
    nxt.list[[i]] <- rc(cc[[i]], probability(var = v, cond = s), G, topo, tree)
    product.list[[i]] <- nxt.list[[i]]$P
    tree$branch[[i]] <- nxt.list[[i]]$tree
  }
  if (length(product.list) > 1) {
    res$sumset <- setdiff(D, y)
    res$product <- TRUE
    res$children <- product.list
  } else {
    res <- product.list[[1]]
    res$sumset <- union(res$sumset, setdiff(D, y))
  }
  res <- activate.selection.variable(res, s)
  attr(res, "algorithm") <- "rc"
  attr(res, "query") <- list(y = y, x = x, s = s)
  if (expr) res <- get.expression(res, primes)
  if (steps) return(list(P = res, steps = tree))
  return(res)
}