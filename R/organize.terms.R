organize.terms <-
function(P, to) {
  if (P$fraction) {
    P$num <- organize.terms(P$den, to)
    P$den <- organize.terms(P$den, to)
    return(P)
  } 
  if (P$product) {
    children.copy <- P$children
    P$children <- list()
    rec <- unlist(lapply(children.copy, FUN = function(x) x$product))
    children.rec <- children.copy[rec]
    if (length(children.rec) > 0) for(i in 1:length(children.rec)) P$children[[i]] <- organize.terms(children.rec[[i]], to)
    children.nonrec <- children.copy[!rec]
    if (length(children.nonrec) > 0) {
      ord <- order(unlist(lapply(children.nonrec, FUN = function(x) min(to %in% x$var))), decreasing = TRUE)
      P$children <- c(children.nonrec[ord], P$children) 
    } 
  }
  return(P)
}
