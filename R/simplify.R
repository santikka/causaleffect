#' Simplify
#'
#' This function algebraically simplifies probabilistic expressions given by the ID algorithm.
#' Always attempts to perform maximal simplification, meaning that as many
#' variables of the set are removed as possible. If the simplification in terms
#' of the entire set cannot be completed, the intermediate result with as many
#' variables simplified as possible should be returned.
#'
#'
#' P: Probabilistic expression that will be simplified
#' topo: Topological ordering of the vertices in graph G
#' G.unobs: Unobserved nodes in graph G
#' G: Graph G
#' G.obs: Observed nodes in graph G
#' Returns: Simplified atomic expression
#'
#'
#' Dependencies: irrelevant, wrap.dSep, dSep, join, ancestors, factorize,
#' parents, children, powerset
#'
#'


simplify <- function(P, topo, G.unobs, G, G.obs) {
  j <- 0 # initialize j to 0
  while (j < length(P$sumset)) { # WHILE loop runs until all elements in P[‘sumset’] are processed
    P.orig <- P # copy original expression P to go back to original expression if simplification does not work
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
    D <- character() # initialize all other variables
    if (i > 1) {
      irrel <- irrelevant(P$children[1:(i-1)], P$sumset[j], P$sumset, G.unobs)
      irl.len <- length(irrel)
      if (irl.len > 0) {
        i <- i - irl.len
        terms <- P$children[irrel]
        P$children[irrel] <- NULL
        vars <- vars[-irrel] # remove irrelevant terms from expression to reduce complexity
      }
    }
    M <- topo[!(topo %in% vars)]
    O <- topo[(topo %in% vars)] # separate variables into Missing (M) and Observed (O)
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
    } # perform join operation (join components of expression to simplify). If successful, update the components accordingly. If fails, break loop and reset expression.
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
  } # if simplification possible, factorize expression using intermediate sets and update sumset. Check for further elimination of redundant terms using cancel()
  return(P)
} # return simplified expression
