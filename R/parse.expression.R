parse.expression <- function(P, topo, G.unobs, G, G.obs) {
  if (P$fraction) {
    P <- cancel.out(P)
    if (P$fraction) {
      P$den <- parse.expression(P$den, topo, G.unobs, G, G.obs)
      if (length(P$den) == 0) {
        sum_p <- P$sumset
        P <- P$num
        P$sumset <- union(sum_p, P$sumset) %ts% topo
        if (P$product) {
          if (length(P$children) == 1) {
            sum_p <- P$sumset
            P <- P$children[[1]]
            P$sumset <- union(sum_p, P$sumset) %ts% topo
          }
        }
        return(P)
      }
      if (length(P$sumset) > 0 && length(P$den) > 0) {
        nodep <- setdiff(P$sumset, dependencies(P$den))
        if (length(nodep) > 0) {
          P$num$sumset <- union(P$num$sumset, nodep) %ts% topo
          P$sumset <- setdiff(P$sumset, nodep) %ts% topo
        }
      }
      P$num <- parse.expression(P$num, topo, G.unobs, G, G.obs)
      P <- cancel.out(P)
    }
    return(P)
  }
  simplify_terms <- TRUE
  if (P$product) {
    non_atomic <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
    if (sum(non_atomic) > 0) {
      parse_children <- P$children[non_atomic]
      P$children <- P$children[!non_atomic]
      for (i in 1:length(parse_children)) {
        P.parse <- parse.expression(parse_children[[i]], topo, G.unobs, G, G.obs)
        if (!is.null(P.parse$collapse)) {
          P$children <- c(P$children, P.parse$children)
        } else {
          P$children[[length(P$children) + 1]] <- P.parse
        }
      }
    }
    if (length(P$children) > 0) {
      non_atomic <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
      if (sum(non_atomic) > 0) simplify_terms <- FALSE
    } else return(NULL)
  }
  if (length(P$sumset) == 0) return(P)
  if (!P$product) {
    if (identical(P$sumset, P$var)) return(NULL)
    else return(P)
  }
  if (simplify_terms) {
    ord.children <- order(unlist(lapply(P$children, FUN = function(x) which(topo == x$var))), decreasing = TRUE)
    ord.sum <- order(sapply(P$sumset, FUN = function(x) which(topo == x)), decreasing = TRUE)
    P$children <- P$children[ord.children]
    P$sumset <- P$sumset[ord.sum]
    P <- simplify(P, topo, G.unobs, G, G.obs)
    if (length(P$children) == 0) return(NULL)
  }
  P.parse <- probability(product = TRUE, children = list())
  remove <- c()
  j <- 0
  if (length(P$sumset) > 0) {
    for (i in 1:length(P$children)) {
      dep <- dependencies(P$children[[i]])
      if (length(intersect(dep, P$sumset)) == 0) {
        remove <- c(remove, i)
        j <- j + 1
      }
    }
  } else return(P)
  if (j > 0) {
    P.parse$children <- P$children[remove]
    P.parse$collapse <- TRUE
    P$children <- P$children[-remove]
    if (length(P$sumset) > 0) {
      if (length(P$children) == 1) {
        sum_p <- P$sumset
        P <- P$children[[1]]
        P$sumset <- union(sum_p, P$sumset) %ts% topo
        P <- parse.expression(P, topo, G.unobs, G, G.obs)
      }
    }
    if (length(P$children) > 0) P.parse$children[[j + 1]] <- P
    return(P.parse)
  }
  return(P)
}

