causal.effect <-
function(y, x, z = NULL, G, expr = TRUE, simp = TRUE, steps = FALSE) {
  G.obs <- observed.graph(G)
  if (!is.dag(G.obs)) stop("Graph 'G' is not a DAG")
  to <- topological.sort(G.obs)
  to <- get.vertex.attribute(G, "name")[to]
  if (length(setdiff(y, to)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, to)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(z) > 0 && z != "") {
    if (length(setdiff(z, to)) > 0) stop("Set 'z' contains variables not present in the graph.")
  }
  res <- list()
  res.prob <- probability()
  if (is.null(z) || z == "" || identical(z, character(0))) { 
    res <- id(y, x, probability(), G, to, list())
    res.prob <- organize.terms(res$P, to)
  } else { 
    res <- idc(y, x, z, probability(), G, to, list())
    res.num <- organize.terms(res$P, to)
    res.den <- res.num
    res.den$sumset <- union(res.den$sumset, y)
    res.prob$fraction <- TRUE
    res.prob$num <- res.num
    res.prob$den <- res.den
  }
  res.tree <- res$tree
  if (simp) {
    G.unobs <- unobserved.graph(G)
    G.adj <- as.matrix(get.adjacency(G.unobs))
    to.u <- topological.sort(G.unobs)
    to.u <- get.vertex.attribute(G.unobs, "name")[to.u]
    res.prob <- deconstruct(res.prob, probability())
    res.prob <- parse.expression(res.prob, to, G.adj, G, G.obs)
    res.prob <- deconstruct(res.prob, probability())
    res.prob <- parse.deconstruct(res.prob)
    # final cancellations here
  }
  if (expr) res.prob <- get.expression(res.prob)
  if (steps) return(list(P = res.prob, steps = res.tree))
  return(res.prob)
}
