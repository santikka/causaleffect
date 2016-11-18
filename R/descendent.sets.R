descendent.sets <- function(node, s, G.s.obs, to) {
  n.s <- length(s)
  roots <- vapply(s, function(x) {
    pa <- parents(x, G.s.obs, to)
    if (length(pa) == 1) return (TRUE)
    return (FALSE)
  }, logical(1))
  n.roots <- sum(roots)
  if (n.roots == 0) return (list())
  desc <- lapply(which(roots), function(i) {
    de <- descendants(s[i], G.s.obs, to)
    if (node %in% de) de <- setdiff(de, node)
    return (de)
  })
  desc <- Filter(function(x) length(x) > 0, desc)
  n.desc <- length(desc)
  if (n.desc > 0) {
    desc.pow <- powerset(1:n.desc, nonempty = TRUE)
    n.sets <- 2^n.desc - 1
    D <- vector(mode = "list", length = n.sets)
    for (i in 1:n.sets) {
      D[[i]] <- Reduce(union, desc[desc.pow[[i]]])
    }
    return (unique(D))
  }
  return (list())
}