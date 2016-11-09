insert <- 
function(J, D, M, cond, S, O, G.adj, G, G.obs, to) {
  mis <- M[which(M %in% D)]
  M <- M[length(mis)]
  if (M %in% cond) return(list(J, D))

  non.mis <- min(which(J %in% O))
  V.prev <- J[non.mis]
  ind <- which(to == V.prev)
  V.pi <- to[0:(ind-1)]
  anc <- ancestors(M, G.obs, to)

  cond.diff <- setdiff(union(M, setdiff(V.pi, anc)), J)

  ds <- powerset(cond.diff, nonempty = FALSE)
  n <- length(ds)
  for (i in 1:n) {
    add <- union(anc, ds[[i]])
    add.M <- setdiff(add, M)
    a.set <- union(setdiff(add, D), setdiff(D, add))
    if (wrap.dSep(G.adj, J, a.set, setdiff(D, a.set)) && 
        wrap.dSep(G.adj, M, S, setdiff(add.M, S))) {
      J.new <- union(J, M)
      D.new <- add.M
      return(list(J.new, D.new, M, add.M))
    }
  }

  return(list(J, D))

}