wrap.dSep <- function(G, x, y, z) {
  if (identical(x, y)) {
    return(FALSE)
  }
  if (length(x) == 0 || length(y) == 0) {
    return(TRUE)
  }
  if (caugi::is_caugi(G)) {
    return(caugi::m_separated(G, X = x, Y = y, Z = z))
  }
  return(dSep(G, x, y, z))
}
