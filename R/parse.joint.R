parse.joint <- function(P, v, cond, var) {
  P.new <- probability()
  P.num <- P
  P.num$sumset <- c(union(P$sumset, setdiff(var, union(v, cond))))
  if (length(cond) > 0) {
    P.den <- P
    P.den$sumset <- c(union(P$sumset, setdiff(var, cond)))
    P.new$fraction <- TRUE
    P.new$num <- P.num
    P.new$den <- P.den
  } else P.new <- P.num
  return(P.new)
}
