transport <- function(y, x, z = NULL, D, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE, stop_on_nonid = TRUE) {
  if (length(edge.attributes(D)) == 0) {
    D <- set.edge.attribute(D, "description", 1:length(E(D)), NA)
  }
  v <- get.vertex.attribute(D, "name")
  s <- v[which(vertex.attributes(D)$description == "S")]
  if (is.null(z)) z <- setdiff(v, union(y, s))
  D.causal <- induced.subgraph(D, v[!(v %in% s)])
  res <- generalize(y = y, x = x, Z = list(character(0), z), D = list(D.causal, D), expr = FALSE, simp = simp, steps = TRUE, primes = primes, stop_on_nonid = stop_on_nonid)
  res.prob <- res$P
  attr(res.prob, "algorithm") <- "trz"
  if (res$id) {
    if (expr) res.prob <- get.expression(res.prob, primes)
    if (steps) return(list(P = res.prob, steps = res$steps, id = TRUE))
    return(res.prob)
  } else {
    if (expr) return("")
    if (steps) return(list(P = res.prob, steps = res$steps, id = FALSE))
    return(NULL)
  }
}
