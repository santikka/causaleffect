cancel <- function(P, R.var, R.cond, S) {
  ind <- c()
  for (i in 1:length(P$children)) {
    ch <- P$children[[i]]
    if (ch$var %in% R.var) {
      mis <- which(R.var == ch$var)
      if (identical(setdiff(R.cond[[mis]], ch$cond), S)) {
        ind <- c(ind, i)
      } else {
        ind <- c()
        break
      }
    }
  }
  if (length(ind) > 0) P$children <- P$children[-ind]
  return (P)
}