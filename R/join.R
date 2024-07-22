# Join
#
# Attempts to combine 2 terms: the joint term P(J|D) obtained from simplify() and the
# term P (V|C) := P (Vk|Ck) of the current iteration step. The goal is to
# determine if these terms can be combined based on the d-separation criteria in the graph G.
#
#
# J: joint set P(J|D); already processed and included in joint distribution
# from previous simplify iteration. Initially, may be empty for starting point of
# joint distribution. vari is added to expand it if d-separation conditions are met.
# D: term P (V|C) := P (Vk|Ck); set of variables that condition the joint distribution
# Join checks and updates D as necessary to maintain validity of joint dist.
# when combined with vari.
# vari: current variable being considered for inclusion in the joint distribution
# cond: set of variables that condition the current variable vari. Join uses cond
# to evaluate conditional independence & determine if vari can be added to J.
# S: current summation variable
# M: missing variables (variables not contained within the expression)
# O: observed variables (variables contained within the expression)
# G.unobs: Unobserved nodes in graph G
# G: Graph G
# G.obs: Observed nodes in graph G
# topo: Topological ordering of the vertices in graph G
#
# Returns: joint result, or the original result if none of conditions for joining were met
#
#
# Causaleffect dependencies: powerset, wrap.dSep, insert




join <- function(J, D, vari, cond, S, M, O, G.unobs, G, G.obs, topo) {
# initialize J and D as empty character vectors
  J.new <- character()
  D.new <- character()
# check if J is empty. If it is, set J.new (the joint subset) to vari and
# D.new (the conditioning subset) to cond, and then return these values.
# This represents the simplest case where the first variable forms the joint distribution alone.
  if (length(J) == 0) {
    J.new <- vari
    D.new <- cond
    return(list(J.new, D.new))
  }
# Set up necessary variables for iteration. Calculate ancestors: find the
# of the first element of J in the topological order (the topo vector).
# V.prev is set to this element. Compute V.pi (aka: "G") as the set of
# vertices in topo that precede the first element of V.prev (aka: "J").
  J.min <- min(which(J %in% topo))
  V.prev <- J[J.min]
  ind <- which(topo == V.prev)
  V.pi <- topo[0:(ind-1)]
# The power set "ds" of V.pi, excluding elements in vari, is computed.
  ds <- powerset(setdiff(V.pi, vari))
  n <- length(ds)
# Iterate over the power set, forming candidate sets add, a.set, and b.set,
# sets used to characterize the changes needed in the conditioning sets to
# enable the combination of two probabilistic terms while preserving the
# required conditional independencies
# A represents necessary changes to the conditioning set D to combine the joint
# distribution term P(J|D) with the current term P(vari|cond)
# B represents necessary changes to the conditioning set cond to combine the
# joint term P(J|D) with the current P(vari|cond)
  for (i in 1:n) {
    add <- union(ds[[i]], vari)
    a.set <- union(setdiff(add, D), setdiff(D, add))
    b.set <- union(setdiff(ds[[i]], cond), setdiff(cond, ds[[i]]))
# Check if they meet conditional independence (d-separation) conditions using the wrap.dSep function.
    if (wrap.dSep(G.unobs, J, a.set, setdiff(D, a.set)) &&
        wrap.dSep(G.unobs, vari, b.set, setdiff(cond, b.set))) {
# If conditions are satisfied, update J.new and D.new and return
      J.new <- union(J, vari)
      D.new <- ds[[i]]
      return(list(J.new, D.new))
    }
  }
# If any element of M is in D, attempt to insert a missing variable from M into J
# and D using the insert and join functions.
  if (any(M %in% D)) {
    joint <- insert(J, D, M, cond, S, O, G.unobs, G, G.obs, topo)
# If the joint operation results in a larger J, return the joint result
    if (length(joint[[1]]) > length(J)) {
      return(joint)
    }
  }
# If no updates were made, return the original J and D
  return(list(J, D))
}
