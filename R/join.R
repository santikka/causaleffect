join <- 
function(J, D, vari, cond, S, M, O, G.adj, G, G.obs, to) {
  J.new <- character()
  D.new <- character()
  if (length(J) == 0) {
    J.new <- vari
    D.new <- cond
    return(list(J.new, D.new))
  }

  non.mis <- min(which(J %in% O))
  V.prev <- J[non.mis]
  ind <- which(to == V.prev)
  V.pi <- to[0:(ind-1)]
    
  anc <- ancestors(vari, G.obs, to)
  cond.diff <- setdiff(union(vari, setdiff(V.pi, anc)), J)

  ds <- powerset(cond.diff, nonempty = FALSE)
  n <- length(ds)
  for (i in 1:n) {
    add <- union(anc, ds[[i]])
    add.v <- setdiff(add, vari)
    a.set <- union(setdiff(add, D), setdiff(D, add))
    b.set <- union(setdiff(add.v, cond), setdiff(cond, add.v))
    if (wrap.dSep(G.adj, J, a.set, setdiff(D, a.set)) && 
        wrap.dSep(G.adj, vari, b.set, setdiff(cond, b.set))) {
      J.new <- union(J, vari)
      D.new <- add.v
      return(list(J.new, D.new))
    }
  }
  if (any(M %in% D)) {
    joint <- insert(J, D, M, cond, S, O, G.adj, G, G.obs, to)
    if (length(joint[[1]]) > length(J)) {
      return(joint)
    }
  }

  return(list(J, D))

}
