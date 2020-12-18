descendent.sets <- function(node, s, G.s.obs, topo) {
  n.s <- length(s)
  roots <- vapply(s, function(x) {
    pa <- parents(x, G.s.obs, topo)
    if (length(pa) == 1) return(TRUE)
    return(FALSE)
  }, logical(1))
  n.roots <- sum(roots)
  if (n.roots == 0) return(list())
  desc <- lapply(which(roots), function(i) {
    de <- descendants(s[i], G.s.obs, topo)
    if (node %in% de) return(NULL)
    return(de)
  })
  desc <- desc[!vapply(desc, is.null, logical(1))]
  n.desc <- length(desc)
  if (n.desc > 0) {
    desc.pow <- powerset(1:n.desc)[-1]
    n.sets <- 2^n.desc - 1
    D <- vector(mode = "list", length = n.sets)
    for (i in 1:n.sets) {
      D[[i]] <- Reduce(union, desc[desc.pow[[i]]])
    }
    return(unique(D))
  }
  return(list())
}