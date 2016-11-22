parents <- function(node, G.obs, to) {
  pa.ind <- unique(unlist(neighborhood(G.obs, order = 1, nodes = node, mode = "in")))
  pa <- V(G.obs)[pa.ind]$name
  pa <- pa %ts% to
  return(pa)
}