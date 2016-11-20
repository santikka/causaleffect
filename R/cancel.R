cancel <- function(P, R.var, R.cond, S) {
  ind <- c()
  for (i in 1:length(P$children)) {
    ch <- P$children[[i]]
    if (ch$var %in% R.var) {
      mis <- which(R.var == ch$var)
      if (setdiff(R.cond[[mis]], ch$cond) == S) {
        ind <- c(ind, i)
      } else {
        ind <- c()
        break
      }
    }
  }
  P$children[[ind]] <- NULL
  return (P)
}