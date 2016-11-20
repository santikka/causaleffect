aux.effect <- function(y, x, z, G, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE) {
  res <- generalize(y = y, x = x, Z = list(z), D = list(G), expr = FALSE, simp = simp, steps = steps, primes = primes)
  if (class(res) == "probability") {
    attr(res, "algorithm") <- "zid"
    if (expr) res <- get.expression(res)
  } else {
    res.prob <- res$P
    attr(res.prob, "algorithm") <- "zid"
    if (expr) res.prob <- get.expression(res.prob)
    res$P <- res.prob
  }
  return (res)
}
