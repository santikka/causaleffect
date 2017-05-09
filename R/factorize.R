factorize <- function(J, D, P, topo, i) {
  P.factorize <- probability(sumset = P$sumset, product = TRUE, children = list())
  vars <- unlist(lapply(P$children, FUN = function(x) x$var))
  r <- which(J == vars[i])
  J <- J[-r]
  elems <- length(J)
  if (elems > 1) {
    for (k in 1:(elems-1)) {
      P.factorize$children[[k]] <- probability(var = J[k], cond = union(D, J[(k+1):elems]))
    }
  }
  if (elems > 0) {
    P.factorize$children[[elems]] <- probability(var = J[elems], cond = D)
  }
  l <- length(P$children)
  if (i < l) {
    P.factorize$children <- c(P.factorize$children, P$children[(i+1):l])
  }
  return(P.factorize)
}