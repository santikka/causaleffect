meta.transport <-
function(y, x, D, expr = TRUE, simp = TRUE, steps = FALSE, primes = FALSE) {
  v <- get.vertex.attribute(D[[1]], "name")
  s <- v[which(vertex.attributes(D[[1]])$description == "S")]
  D.causal <- induced.subgraph(D[[1]], v[!(v %in% s)])
  D.all <- list()
  D.all[[1]] <- D.causal
  D.all[2:(length(D)+1)] <- D
  Z.all <- list()
  Z.all[[1]] <- character(0)
  Z.all[2:(length(D)+1)] <- rep(list(v), length(D))
  return(generalize(y = y, x = x, Z = Z.all, D = D.all, expr = expr, simp = simp, steps = steps, primes = primes))
}
