parse.expression <- function(P, topo, G.adj, G, G.obs) {
  if (P$fraction) {
    P$den <- parse.expression(P$den, topo, G.adj, G, G.obs)
    if (is.null(P$den)) {
      sum.P <- P$sumset
      P <- P$num
      P$sumset <- union(sum.P, P$sumset) %ts% topo
    }
    if (length(P$sumset) > 0) {
      nodep <- setdiff(P$sumset, dependencies(P$den))
      P$num$sumset <- union(P$num$sumset, nodep) %ts% topo
      P$sumset <- setdiff(P$sumset, nodep) %ts% topo
    }
    P$num <- parse.expression(P$num, topo, G.adj, G, G.obs)
    return(P)
  }
  if (P$product) {
    parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction | x$sum))
    if (sum(parse.children) > 0) {
      for (i in which(parse.children)) {
        P$children[[i]] <- parse.expression(P$children[[i]], topo, G.adj, G, G.obs)
      }
    }
    if (length(P$children) > 0) {
      parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction | x$sum))
      if (sum(parse.children) > 0) return(P)
    } else return(NULL)
  }
  if (length(P$sumset) == 0) return(P)
  if (!P$product) {
    if (identical(P$sumset, P$var)) return (NULL)
    else return (P)
  }
  ord.children <- order(unlist(lapply(P$children, FUN = function(x) which(topo == x$var))), decreasing = TRUE)
  ord.sum <- order(sapply(P$sumset, FUN = function(x) which(topo == x)), decreasing = TRUE)
  P$children <- P$children[ord.children]
  P$sumset <- P$sumset[ord.sum]
  P <- simplify(P, topo, G.adj, G, G.obs)
  if (length(P$children) == 0) return (NULL)
  P.parse <- probability(product = TRUE, children = list())
  remove <- c()
  j <- 0
  if (length(P$sumset) > 0) {
    for (i in 1:length(P$children)) {
      if (length(intersect(P$children[[i]]$var, P$sumset)) == 0 && length(intersect(P$children[[i]]$cond, P$sumset)) == 0) {
        remove <- c(remove, i)
        j <- j + 1
      }
    }
  } else return (P)
  if (j > 0) {
    P.parse$children <- P$children[remove]
    P$children <- P$children[-remove]
    if (length(P$children) > 0) P.parse$children[[j + 1]] <- P
    return(P.parse)
  }
  return(P)
}

