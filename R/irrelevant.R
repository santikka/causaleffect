irrelevant <- function(children, vari, sumset, G.unobs) {
  n <- length(children)
  irrel <- NULL
  for (i in 1:n) {
    ch <- children[[i]]
    if (!(ch$var %in% sumset)) {
      if (wrap.dSep(G.unobs, ch$var, vari, setdiff(ch$cond, vari))) irrel <- c(irrel, i)
    }
  }
  return(irrel)
}