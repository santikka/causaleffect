meta.transport <-
function(y, x, D, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE) {
  v <- get.vertex.attribute(D[[1]], "name")
  s <- v[which(vertex.attributes(D[[1]])$description == "S")]
  interventions <- setdiff(v, union(y, s))
  D.causal <- induced.subgraph(D[[1]], v[!(v %in% s)])
  D.all <- list()
  D.all[[1]] <- D.causal
  D.all[2:(length(D)+1)] <- D
  Z.all <- list()
  Z.all[[1]] <- character(0)
  Z.all[2:(length(D)+1)] <- rep(list(interventions), length(D))
  res <- generalize(y = y, x = x, Z = Z.all, D = D.all, expr = FALSE, simp = simp, steps = steps, primes = primes)
  if (class(res) == "probability") {
    attr(res, "algorithm") <- "usid"
    if (expr) res <- get.expression(res)
  } else {
    res.prob <- res$P
    attr(res.prob, "algorithm") <- "usid"
    if (expr) res.prob <- get.expression(res.prob)
    res$P <- res.prob
  }
  return (res)
}
