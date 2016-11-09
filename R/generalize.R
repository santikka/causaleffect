generalize <-
function(y, x, Z, D, expr = TRUE, simp = TRUE, steps = FALSE) {
  d <- length(D)
  z <- length(Z)
  v <- get.vertex.attribute(D[[1]], "name")
  s <- v[which(vertex.attributes(D[[1]])$description == "S")]
  if (length(s) > 0) stop("The causal diagram cannot contain selection variables.")
  if (d != z) stop("Number of available experiments does not match number of domains.")
  to <- lapply(D, function(x) topological.sort(observed.graph(x)))
  to <- lapply(1:d, function(x) get.vertex.attribute(D[[x]], "name")[to[[x]]])
  for (i in 1:d) {
    if (!is.dag(observed.graph(D[[i]]))) {
      if (i > 1) stop("Selection diagram 'D[", i, "]' is not a DAG.")
      else stop("Causal diagram 'D[", i, "]' is not a DAG.")
    } 
    if (length(setdiff(y, to[[i]])) > 0) stop("Set 'y' contains variables not present in diagram 'D[", i, "]'.")
    if (length(setdiff(x, to[[i]])) > 0) stop("Set 'x' contains variables not present in diagram 'D[", i, "]'.")
    if (length(setdiff(Z[[i]], to[[i]])) > 0) stop("Set 'Z[", i, "]' contains variables not present in diagram 'D[", i, "]'.")
  }
  res <- trmz(y, x, probability(domain = 1), c(), 1, 1, D, Z, to, list())
  if (expr) res$P <- get.expression(res$P)
  if (steps) return(list(P = res$P, steps = res$tree))
  return(res$P)
}
