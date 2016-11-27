exclusion.restrictions <- function(G) {
  G.obs <- observed.graph(G)
  to <- topological.sort(G.obs)
  v <- get.vertex.attribute(G, "name")[to]
  ex <- lapply(v, function(y) {
    pa <- setdiff(parents(y, G.obs, to), y)
    Z <- setdiff(v, union(y, pa))
    if (length(Z) > 0) {
      Z.pow <- powerset(setdiff(v, union(y, pa)), nonempty = TRUE)
      return(list(pa = pa, Z = Z.pow))
    } else return(NULL)
  })
  ex.ind <- !vapply(ex, is.null, logical(1))
  v <- v[ex.ind]
  ex <- ex[ex.ind]
  return(setNames(ex, v))
}