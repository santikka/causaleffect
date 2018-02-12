dependencies <- function(P) {
  dep <- c()
  if (P$fraction) return(union(dependencies(P$num), dependencies(P$den)))
  if (P$sum) {
    for (i in 1:length(P$children)) {
      dep <- union(dep, dependencies(P$children[[i]]))
    }
    return(dep)
  }
  if (P$product) {
    for (i in 1:length(P$children)) {
      dep <- union(dep, dependencies(P$children[[i]]))
    }
    return(dep)
  }
  return(unique(c(P$var, P$cond, P$do)))
}