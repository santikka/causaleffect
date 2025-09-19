exclusion.restrictions <- function(G) {
  G.obs <- observed.graph(G)
  topo <- igraph::topo_sort(G.obs)
  v <- igraph::vertex_attr(G, "name")[topo]
  ex <- lapply(v, function(y) {
    pa <- setdiff(parents(y, G.obs, topo), y)
    Z <- setdiff(v, union(y, pa))
    if (length(Z) > 0) {
      Z.pow <- powerset(setdiff(v, union(y, pa)))[-1]
      return(list(pa = pa, Z = Z.pow))
    } else return(NULL)
  })
  ex.ind <- !vapply(ex, is.null, logical(1))
  v <- v[ex.ind]
  ex <- ex[ex.ind]
  return(setNames(ex, v))
}
