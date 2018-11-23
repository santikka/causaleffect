surrogate.outcome <- function(y, x, S, G, expr = TRUE, steps = FALSE, primes = FALSE, stop_on_nonid = TRUE) {
  d <- length(S) + 1
  v <- get.vertex.attribute(G, "name")
  if (length(intersect(x, y)) > 0) stop("Sets 'x' and 'y' are not disjoint.")
  D <- vector(mode = "list", length = d)
  Z <- vector(mode = "list", length = d)
  D[[1]] <- G
  G.obs <- observed.graph(G)
  topo.obs <- topological.sort(G.obs)
  Z[[1]] <- character(0)
  for (i in 1:(d-1)) {
    Di <- G
    Zi <- S[[i]]$Z
    Wi <- S[[i]]$W
    target <- tr.target(Zi, Wi, G.obs, G, topo.obs)
    if ((tr.len <- length(target)) > 0) {
      for (j in 1:tr.len) {
        tr.node <- paste0(c("S_{",i,",",j,"}"), collapse = "")
        Di <- Di + vertex(tr.node, description = "S")
        Di <- Di + edge(tr.node, target[j])
      }
    }
    D[[i+1]] <- Di
    Z[[i+1]] <- Zi
  }
  topo <- lapply(D, function(k) topological.sort(observed.graph(k)))
  topo <- lapply(1:d, function(k) get.vertex.attribute(D[[k]], "name")[topo[[k]]])
  D <- lapply(D, function(k) {
    if (length(edge.attributes(k)) == 0) {
      k <- set.edge.attribute(k, "description", 1:length(E(k)), NA)
    }
    return(k)
  })
  for (i in 1:d) {
    if (!is.dag(observed.graph(D[[i]]))) {
      if (i > 1) stop("Selection diagram 'D[", i, "]' is not a DAG.")
      else stop("Causal diagram 'D[", i, "]' is not a DAG.")
    }
    if (length(setdiff(y, topo[[i]])) > 0) stop("Set 'y' contains variables not present in diagram 'D[", i, "]'.")
    if (length(setdiff(x, topo[[i]])) > 0) stop("Set 'x' contains variables not present in diagram 'D[", i, "]'.")
    if (length(setdiff(Z[[i]], topo[[i]])) > 0) stop("Set 'Z[", i, "]' contains variables not present in diagram 'D[", i, "]'.")
  }
  res <- trso(y, x, probability(var = v, domain = 1), c(), 1, D, Z, topo, list())
  if (res$tree$call$id) {
    res.prob <- so.factorize(res$P, S, topo.obs)
    attr(res.prob, "algorithm") <- "trso"
    attr(res.prob, "query") <- list(y = y, x = x)
    attr(res.prob, "sources") <- d - 1
    if (expr) res.prob <- get.expression(res.prob, primes)
    if (steps) return(list(P = res.prob, steps = res$tree, id = TRUE))
    return(res.prob)
  } else {
    if (stop_on_nonid) stop("Not surrogate outcome identifiable.", call. = FALSE)
    res.prob <- probability()
    attr(res.prob, "algorithm") <- "trso"
    attr(res.prob, "query") <- list(y = y, x = x)
    attr(res.prob, "sources") <- d - 1
    if (steps) return(list(P = res.prob, steps = res$tree, id = FALSE))
    if (expr) return("")
    return(NULL)
  }
}