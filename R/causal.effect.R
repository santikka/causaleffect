causal.effect <- function(y, x, z = NULL, G, expr = TRUE, simp = FALSE, steps = FALSE, primes = FALSE, prune = FALSE) {
  if (length(edge.attributes(G)) == 0) {
    G <- set.edge.attribute(G, "description", 1:length(E(G)), NA)
  }
  G.obs <- observed.graph(G)
  if (!is.dag(G.obs)) stop("Graph 'G' is not a DAG")
  topo <- topological.sort(G.obs)
  topo <- get.vertex.attribute(G, "name")[topo]
  if (length(setdiff(y, topo)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, topo)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(z) > 0 && !identical(z, "")) {
    if (length(setdiff(z, topo)) > 0) stop("Set 'z' contains variables not present in the graph.")
  }
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint.")
  if (length(intersect(y, z)) > 0) stop("Sets 'y' and 'z' are not disjoint.")
  if (length(intersect(x, z)) > 0) stop("Sets 'x' and 'z' are not disjoint.")
  res <- list()
  algo <- ""
  res.prob <- probability()
  if (is.null(z) || identical(z, "") || identical(z, character(0))) {
    if (prune) res <- pid(y, x, probability(), G, G.obs, topo, topo, list())
    else res <- id(y, x, probability(), G, G.obs, topo, topo, list())
    res.prob <- res$P
    algo <- "id"
    #res.prob <- organize.terms(res$P, topo)
  } else {
    res <- idc(y, x, z, probability(), G, G.obs, topo, topo, list(), prune)
    res.num <- res$P
    #res.num <- organize.terms(res$P, topo)
    res.den <- res.num
    res.den$sumset <- union(res.den$sumset, y)
    res.prob$fraction <- TRUE
    res.prob$num <- res.num
    res.prob$den <- res.den
    algo <- "idc"
  }
  res.tree <- res$tree
  if (simp) {
    G.unobs <- unobserved.graph(G)
    G.adj <- as.matrix(get.adjacency(G.unobs))
    topo.u <- topological.sort(G.unobs)
    topo.u <- get.vertex.attribute(G.unobs, "name")[topo.u]
    res.prob <- deconstruct(res.prob, probability(), topo)
    # cat(get.expression(res.prob), "\n")
    res.prob <- parse.expression(res.prob, topo, G.adj, G, G.obs)
    # cat(get.expression(res.prob), "\n")
    res.prob <- deconstruct(res.prob, probability(), topo)
    # cat(get.expression(res.prob), "\n")
    res.prob <- parse.deconstruct(res.prob)
    # cat(get.expression(res.prob), "\n")
  }
  attr(res.prob, "algorithm") <- algo
  attr(res.prob, "query") <- list(y = y, x = x, z = z)
  if (expr) res.prob <- get.expression(res.prob, primes)
  if (steps) return(list(P = res.prob, steps = res.tree))
  return (res.prob)
}
