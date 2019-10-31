aux.effect <- function(y, x, z, G, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE, stop_on_nonid = TRUE) {
  if (length(edge.attributes(G)) == 0) {
    G <- set.edge.attribute(G, "description", 1:length(E(G)), NA)
  }
  res <- generalize(y = y, x = x, Z = list(z), D = list(G), expr = FALSE, simp = simp, steps = TRUE, primes = primes, stop_on_nonid = stop_on_nonid)
  res.prob <- res$P
  attr(res.prob, "algorithm") <- "zid"
  if (res$id) {
    if (expr) res.prob <- get.expression(res.prob, primes)
    if (steps) return(list(P = res.prob, steps = res$steps, id = TRUE))
    return(res.prob)
  } else {
    res.prob <- probability()
    if (expr) res.prob <- ""
    if (steps) return(list(P = res.prob, steps = res$steps, id = FALSE))
    return(res.prob)
  }
}
