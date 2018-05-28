so.factorize <- function(P, S, topo) {
  if (P$fraction) {
    P$num <- so.factorize(P$num, S, topo)
    P$den <- so.factorize(P$den, S, topo)
  }
  if (P$product) {
    for (i in 1:length(P$children)) {
      P$children[[i]] <- so.factorize(P$children[[i]], S, topo)
    } 
  }
  P$domain <- 0
  if (length(P$do) > 0) {
    for (s in S) {
      vars <- intersect(s$W, P$var)
      nvars <- length(vars)
      if (nvars > 0) {
        if (nvars < length(P$var)) {
          newcond <- setdiff(P$var, vars) %ts% topo
          Pw <- probability(var = vars, cond = union(P$cond, newcond) %ts% topo, do = P$do)
          Pc <- probability(var = newcond, cond = P$cond)
          return(probability(product = TRUE, children = list(Pw, Pc)))
        } else {
          return(P)
        }
      }
    }
    P$do <- character(0)
  }
  return(P)
}
