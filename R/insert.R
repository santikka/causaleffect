insert <- function(J, D, M, cond, S, O, G.adj, G, G.obs, topo) {
  mis.ind <- which(M %in% D)
  if (length(mis.ind) == 0) return(list(J, D))
  mis <- M[mis.ind]
  M <- mis[length(mis)]
  if (M %in% cond) return(list(J, D))
  # non.mis <- min(which(J %in% O))
  # V.prev <- J[non.mis]
  # ind <- which(topo == V.prev)
  # V.pi <- topo[0:(ind-1)]
  # anc <- ancestors(M, G.obs, topo)
  # cond.diff <- setdiff(V.pi, anc)
  # ds <- powerset(cond.diff, nonempty = FALSE)
  J.min <- min(which(J %in% topo))
  V.prev <- J[J.min]
  ind <- which(topo == V.prev)
  V.pi <- topo[0:(ind-1)]
  ds <- powerset(setdiff(V.pi, M), nonempty = FALSE)
  n <- length(ds)
  for (i in 1:n) {
    # add <- union(anc, ds[[i]])
    # add.M <- setdiff(add, M)
    # a.set <- union(setdiff(add, D), setdiff(D, add))
    add <- union(ds[[i]], M)
    a.set <- union(setdiff(add, D), setdiff(D, add))
    if (wrap.dSep(G.adj, J, a.set, setdiff(D, a.set)) && 
        wrap.dSep(G.adj, M, S, setdiff(ds[[i]], S))) {
      J.new <- union(J, M)
      D.new <- ds[[i]]
      return(list(J.new, D.new, M, ds[[i]]))
    }
  }
  return(list(J, D))
}