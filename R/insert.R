insert <- function(J, D, M, cond, S, O, G.unobs, G, G.obs, topo) {
  mis.ind <- which(M %in% D)
  if (length(mis.ind) == 0) return(list(J, D))
  mis <- M[mis.ind]
  M <- mis[length(mis)]
  if (M %in% cond) return(list(J, D))
  J.min <- min(which(J %in% topo))
  V.prev <- J[J.min]
  ind <- which(topo == V.prev)
  V.pi <- topo[0:(ind-1)]
  ds <- powerset(setdiff(V.pi, M))
  n <- length(ds)
  for (i in 1:n) {
    add <- union(ds[[i]], M)
    a.set <- union(setdiff(add, D), setdiff(D, add))
    if (wrap.dSep(G.unobs, J, a.set, setdiff(D, a.set)) && 
        wrap.dSep(G.unobs, M, S, setdiff(ds[[i]], S))) {
      J.new <- union(J, M)
      D.new <- ds[[i]]
      return(list(J.new, D.new, M, ds[[i]]))
    }
  }
  return(list(J, D))
}