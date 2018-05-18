surrogate.outcome <- function(y, x, G, I, expr = TRUE, steps = FALSE, primes = FALSE) {
  if (length(edge.attributes(G)) == 0) {
    G <- set.edge.attribute(G, "description", 1:length(E(G)), NA)
  }
  G.obs <- observed.graph(G)
  if (!is.dag(G.obs)) stop("Graph 'G' is not a DAG")
  topo <- topological.sort(G.obs)
  topo <- get.vertex.attribute(G, "name")[topo]
  if (length(setdiff(y, topo)) > 0) stop("Set 'y' contains variables not present in the graph.")
  if (length(setdiff(x, topo)) > 0) stop("Set 'x' contains variables not present in the graph.")
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint.")
  res <- soid(y, x, probability(var = topo), G, G.obs, I, c(), topo, topo, list())
  res.prob <- res$P
  res.tree <- res$tree
  attr(res.prob, "algorithm") <- "soid"
  attr(res.prob, "query") <- list(y = y, x = x)
  if (expr) res.prob <- get.expression(res.prob, primes)
  if (steps) return(list(P = res.prob, steps = res.tree))
  return (res.prob)
}
