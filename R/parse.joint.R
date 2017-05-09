parse.joint <- function(P, v, cond, var, topo) {
  P.new <- probability()
  P.num <- P
  P.num$sumset <- union(P$sumset, setdiff(var, union(v, cond))) %ts% topo
  if (length(cond) > 0) {
    P.den <- P
    P.den$sumset <- union(P$sumset, setdiff(var, cond)) %ts% topo
    P.new$fraction <- TRUE
    P.new$num <- P.num
    P.new$den <- P.den
  } else P.new <- P.num
  return(P.new)
}
