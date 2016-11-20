activate.selection.variable <- function(P, s) {
  if (P$fraction) {
    P$num <- activate.selection.variable(P$num, s)
    P$den <- activate.selection.variable(P$den, s)
    return(P)
  }
  if (P$product) {
    for (i in 1:length(P$children)) {
      P$children[[i]] <- activate.selection.variable(P$children[[i]], s)
    }
    return(P)
  }
  pos <- Position(function(x) x == s, P$cond, nomatch = 0)
  if (pos > 0) {
    P$cond <- c(P$cond[-pos], paste0(s, " = 1"))
  }
  return(P)
}