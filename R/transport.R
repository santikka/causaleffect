transport <-
function(y, x, z = NULL, D, expr = TRUE, simp = TRUE, steps = FALSE, primes = primes) {
  v <- get.vertex.attribute(D, "name")
  s <- v[which(vertex.attributes(D)$description == "S")]
  if (is.null(z)) z <- setdiff(v, union(y, s))
  D.causal <- induced.subgraph(D, v[!(v %in% s)])
  res <- generalize(y = y, x = x, Z = list(character(0), z), D = list(D.causal, D), expr = FALSE, simp = simp, steps = steps, primes = primes)
  if (class(res) == "probability") {
    attr(res, "algorithm") <- "trz"
    if (expr) res <- get.expression(res)
  } else {
    res.prob <- res$P
    attr(res.prob, "algorithm") <- "trz"
    if (expr) res.prob <- get.expression(res.prob)
    res$P <- res.prob
  }
  return (res)
}
