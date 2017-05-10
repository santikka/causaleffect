join <- function(J, D, vari, cond, S, M, O, G.adj, G, G.obs, topo) {
  J.new <- character()
  D.new <- character()
  if (length(J) == 0) {
    J.new <- vari
    D.new <- cond
    return(list(J.new, D.new))
  }
  # non.mis <- min(which(J %in% O))
  # V.prev <- J[non.mis]
  # ind <- which(topo == V.prev)
  # anc <- ancestors(vari, G.obs, topo)
  # cond.diff <- setdiff(V.pi, anc)
  # ds <- powerset(cond.diff, nonempty = FALSE)
  J.min <- min(which(J %in% topo))
  V.prev <- J[J.min]
  ind <- which(topo == V.prev)
  V.pi <- topo[0:(ind-1)]
  ds <- powerset(setdiff(V.pi, vari), nonempty = FALSE)
  n <- length(ds)
  for (i in 1:n) {
    # add <- union(anc, ds[[i]])
    # add.v <- setdiff(add, vari)
    # a.set <- union(setdiff(add, D), setdiff(D, add))
    # b.set <- union(setdiff(add.v, cond), setdiff(cond, add.v))
    add <- union(ds[[i]], vari)
    a.set <- union(setdiff(add, D), setdiff(D, add)) 
    b.set <- union(setdiff(ds[[i]], cond), setdiff(cond, ds[[i]]))
    if (wrap.dSep(G.adj, J, a.set, setdiff(D, a.set)) && 
        wrap.dSep(G.adj, vari, b.set, setdiff(cond, b.set))) {
      J.new <- union(J, vari)
      D.new <- ds[[i]]
      return(list(J.new, D.new))
    }
  }
  if (any(M %in% D)) {
    joint <- insert(J, D, M, cond, S, O, G.adj, G, G.obs, topo)
    if (length(joint[[1]]) > length(J)) {
      return(joint)
    }
  }
  return(list(J, D))
}
