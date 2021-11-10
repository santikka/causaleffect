# Implements relevant path separation (rp-separation) for testing d-separation. For details, see:
#
# Relevant Path Separation: A Faster Method for Testing Independencies in Bayesian Networks
# Cory J. Butz, Andre E. dos Santos, Jhonatan S. Oliveira;
# Proceedings of the Eighth International Conference on Probabilistic Graphical Models,
# PMLR 52:74-85, 2016.
#
# Note that the roles of Y and Z have been reversed from the paper, meaning that
# we are testing whether X is separated from Y given Z in G.

dSep <- function(G, x, y, z) {
  an_z <- ancestors_unsrt(z, G)
  an_xyz <- ancestors_unsrt(union(union(x, y), z), G)
  n <- length(igraph::V(G))
  v <- igraph::V(G)$name
  direction <- NA
  traverse_up <- logical(n)
  visited_up <- logical(n)
  traverse_down <- logical(n)
  visited_down <- logical(n)
  names(traverse_up) <- v
  names(visited_up) <- v
  names(traverse_down) <- v
  names(visited_down) <- v
  traverse_up[x] <- TRUE
  visit <- FALSE
  el_name <- NULL
  while (any(traverse_up) || any(traverse_down)) {
    visit <- FALSE
    for (j in 1:n) {
      if (traverse_up[j]) {
        traverse_up[j] <- FALSE
        if (!visited_up[j]) {
          visit <- TRUE
          direction <- TRUE
          el_name <- v[j]
          break
        }
      }
      if (traverse_down[j]) {
        traverse_down[j] <- FALSE
        if (!visited_down[j]) {
          visit <- TRUE
          direction <- FALSE
          el_name <- v[j]
          break
        }
      }
    }
    if (visit) {
      if (el_name %in% y) return(FALSE)
      if (direction) {
        visited_up[el_name] <- TRUE
      } else {
        visited_down[el_name] <- TRUE
      }
      if (direction && !(el_name %in% z)) {
        visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
        visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
        traverse_up[visitable_parents] <- TRUE
        traverse_down[visitable_children] <- TRUE
      } else if (!direction) {
        if (!(el_name %in% z)) {
          visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
          traverse_down[visitable_children] <- TRUE
        }
        if (el_name %in% an_z) {
          visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
          traverse_up[visitable_parents] <- TRUE
        }
      }
    }
  }
  return(TRUE)
}