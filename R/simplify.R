simplify <- function(P, topo, G.unobs, G, G.obs) {
# initialize j to 0
  j <- 0
# WHILE loop runs until all elements in P['sumset'] are processed
  while (j < length(P$sumset)) {
# make a copy of original expression P (P.orig) used to go back to original
# expression if simplification does not work
    P.orig <- P
    irl.len <- 0
    irrel <- NULL
    terms <- list()
    vars <- sapply(P$children, "[[", "var")
# initialize all other variables
    j <- j + 1
    i <- which(vars == P$sumset[j])
    k <- 1
    R.var <- character()
    R.cond <- list()
    J <- character()
    D <- character()
# remove irrelevant terms from expression to reduce complexity
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
# topological sorting - separate variables into Missing (M) and Observed (O)
    M <- topo[!(topo %in% vars)]
    O <- topo[(topo %in% vars)]
# perform join operation (join components of expression to simplify). If successful,
# update the components accordingly. If fails, break loop & reset expression.
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
# if simplification possible, factorize expression using intermediate sets and
# update sumset.Check for further elimination of redundant terms using cancel().
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
# return simplified expression, or the original if simplification was not possible.
  return(P)
}
