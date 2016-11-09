parse.expression <- 
function(P, to, G.adj, G, G.obs) {
  if (P$fraction) {
    P$num <- parse.expression(P$num, to, G.adj, G, G.obs)
    P$den <- parse.expression(P$den, to, G.adj, G, G.obs)
    if (is.null(P$den)) {
      sum.P <- P$sumset
      P <- P$num
      P$sumset <- union(sum.P, P$sumset)
    }
    return(P)
  }

  if (P$product) {
    parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction | x$sum))
    if (sum(parse.children) > 0) {
      for (i in which(parse.children)) {
        P$children[[i]] <- parse.expression(P$children[[i]], to, G.adj, G, G.obs)
      }
    }
    if (length(P$children) > 0) {
      parse.children <- sapply(P$children, FUN = function(x) (x$product | length(x$sumset) > 0 | x$fraction | x$sum))
      if (sum(parse.children) > 0) return(P)
    } else return(NULL)
  }

  if (length(P$sumset) == 0) return(P)

  if (!P$product) { 
    if (P$sumset == P$var) return(NULL)
    else return(P)
  }

  ord.children <- order(unlist(lapply(P$children, FUN = function(x) which(to == x$var))), decreasing = TRUE)
  ord.sum <- order(sapply(P$sumset, FUN = function(x) which(to == x)), decreasing = TRUE)

  P$children <- P$children[ord.children]
  P$sumset <- P$sumset[ord.sum]
  P <- simplify(P, to, G.adj, G, G.obs)
  if (length(P$children) == 0) return(NULL)
  return(P)   

  # P.parse <- probability(product = TRUE, children = list())
  # remove <- c()
  # if (length(P.sum$sumset) > 0) {
  #   j <- 1
  #  for (i in 1:length(P.sum$children)) {
  #    if (length(intersect(P.sum$children[[i]]$var, P.sum$sumset)) == 0 & length(intersect(P.sum$children[[i]]$cond, P.sum$sumset)) == 0) {
  #      P.parse$children[[j]] <- P.sum$children[[i]]
  #      remove <- c(remove, i)
  #      j <- j + 1
  #   }
  #  }
  # }
  # 
  # P.sum$children[remove] <- NULL
  # if (length(P.sum$children) > 0) P.parse$children[[length(P.parse$children) + 1]] <- P.sum
  # if (length(P.parse$children) == 0) return(P.sum)
  # return(P.parse)  
  
}

