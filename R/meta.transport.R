meta.transport <- function(y, x, D, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE, stop_on_nonid = TRUE) {
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
  res <- generalize(y = y, x = x, Z = Z.all, D = D.all, expr = FALSE, simp = simp, steps = TRUE, primes = primes, stop_on_nonid = stop_on_nonid)
  res.prob <- res$P
  attr(res.prob, "algorithm") <- "usid"
  if (res$id) {
    if (expr) res.prob <- get.expression(res.prob)
    if (steps) return(list(P = res.prob, steps = res$steps, id = TRUE))
    return(res.prob)
  } else {
    if (expr) return("")
    if (steps) return(list(P = res.prob, steps = res$steps, id = FALSE))
    return(NULL)
  }
}
