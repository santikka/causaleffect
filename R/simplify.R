simplify <- function(P, to, G.adj, G, G.obs) {
  j <- 0
  while (j < length(P$sumset)) {
    P.orig <- P
    vars <- unlist(lapply(P$children, FUN = function(x) x$var))
    M <- to[!(to %in% vars)]
    O <- to[(to %in% vars)]
    R.var <- character()
    R.cond <- list()
    j <- j + 1
    i <- which(vars == P$sumset[j])
    k <- 1
    n <- 1
    J <- character()
    D <- character()
    while (k <= i) {
      joint <- join(J, D, P$children[[k]]$var, P$children[[k]]$cond, P$sumset[j], M, O, G.adj, G, G.obs, to)
      if (length(joint[[1]]) <= length(J)) {
        J <- character()
        D <- character()
        k <- 1
        break
      } else {
        J <- joint[[1]]
        D <- joint[[2]]
        if (length(joint) > 2) {
          R.var <- union(R.var, joint[[3]])
          R.cond <- c(R.cond, list(joint[[4]]))
          M <- setdiff(M, R.var)
        } else {
          k <- k + 1
        }
      }
    }
    if (k == i + 1) {
      P <- factorize(J, D, P, to, i)
      S <- P$sumset[j]
      P$sumset <- P$sumset[-j]
      if (length(R.var) > 0) {
        P.cancel <- cancel(P, R.var, R.cond, S)
        if (identical(P.cancel, P)) P <- P.orig
        else {
          P <- P.cancel
          j <- 0
        } 
      } else j <- 0
    } else P <- P.orig
  }
  return(P)
}