parse.joint <- function(P, v, cond, var, to) {
  P.new <- probability()
  P.num <- P
  P.num$sumset <- union(P$sumset, setdiff(var, union(v, cond))) %ts% to
  if (length(cond) > 0) {
    P.den <- P
    P.den$sumset <- union(P$sumset, setdiff(var, cond)) %ts% to
    P.new$fraction <- TRUE
    P.new$num <- P.num
    P.new$den <- P.den
  } else P.new <- P.num
  return(P.new)
}
