simplify <- function(P, topo, G.unobs, G, G.obs) {
  j <- 0
  while (j < length(P$sumset)) {
    P.orig <- P
    irl.len <- 0
    irrel <- NULL
    terms <- list()
    vars <- sapply(P$children, "[[", "var")
    j <- j + 1
    i <- which(vars == P$sumset[j])
    k <- 1
    R.var <- character()
    R.cond <- list()
    J <- character()
    D <- character()
    if (i > 1) {
      irrel <- irrelevant(P$children[1:(i-1)], P$sumset[j], P$sumset, G.unobs)
      irl.len <- length(irrel)
      if (irl.len > 0) {
        i <- i - irl.len
        terms <- P$children[irrel]
        P$children[irrel] <- NULL
        vars <- vars[-irrel]
      }
    }
    M <- topo[!(topo %in% vars)]
    O <- topo[(topo %in% vars)]
    while (k <= i) {
      joint <- join(J, D, P$children[[k]]$var, P$children[[k]]$cond, P$sumset[j], M, O, G.unobs, G, G.obs, topo)
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
      P <- factorize(J, D, P, topo, i)
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
      if (irl.len > 0) P$children <- c(terms, P$children)
    } else P <- P.orig
  }
  return(P)
}