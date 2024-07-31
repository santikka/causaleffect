insert <- function(J, D, M, cond, S, O, G.unobs, G, G.obs, topo) {
  # Identify which elements of M are in D
  mis.ind <- which(M %in% D)
  # If no elements of M are in D, return original J and D
  if (length(mis.ind) == 0) return(list(J, D))

  # Identify the missing variable to be inserted
  mis <- M[mis.ind]
  M <- mis[length(mis)]
  # If M is in cond, return original J and D
  if (M %in% cond) return(list(J, D))

  # Find the first element of J in the topological ordering 'topo'.
  # Set V.prev to this element
  # V.pi is set of vertices that are the ancestors of (precede) V.prev.
  J.min <- min(which(J %in% topo))
  V.prev <- J[J.min]
  ind <- which(topo == V.prev)
  # Get all vertices before V.prev in topological order
  V.pi <- topo[0:(ind-1)]

  # Compute the power set of V.pi excluding M.
  # n is the numbere of subsets in the power set.
  ds <- powerset(setdiff(V.pi, M))
  n <- length(ds)
  # Create the candidate set add
  for (i in 1:n) {
    add <- union(ds[[i]], M)
    # Compute the set A
    a.set <- union(setdiff(add, D), setdiff(D, add))

    # Check the d-separation criteria
    if (wrap.dSep(G.unobs, J, a.set, setdiff(D, a.set)) &&
        wrap.dSep(G.unobs, M, S, setdiff(ds[[i]], S))) {
      # Update J.new and D.new
      J.new <- union(J, M)
      D.new <- ds[[i]]
      return(list(J.new, D.new, M, ds[[i]]))
    }
  }
  # If no conditions were met, return original J and D
  return(list(J, D))
}
