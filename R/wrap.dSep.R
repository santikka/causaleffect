wrap.dSep <- function(G, x, y, z) {
  if (identical(x, y)) return(FALSE)
  if (length(x) == 0 || length(y) == 0) {
    return(TRUE)
  }
  else return(dSep(G, x, y, z))
}