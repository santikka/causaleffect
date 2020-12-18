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
  stack_top <- length(x)
  stack_size <- max(stack_top, 64)
  stack <- rep(FALSE, stack_size)
  stack[1:stack_top] <- TRUE
  names(stack)[1:stack_top] <- x
  visited_top <- 0
  visited_size <- 64
  visited <- rep(FALSE, visited_size)
  names(visited) <- rep(NA, visited_size)
  is_visited <- FALSE
  while (stack_top > 0) {
    is_visited <- FALSE
    el <- stack[stack_top]
    el_name <- names(el)
    stack_top <- stack_top - 1
    if (visited_top > 0) {
      for (i in 1:visited_top) {
        if (el == visited[i] && identical(el_name, names(visited[i]))) {
          is_visited <- TRUE
          break
        }
      }
    }
    if (!is_visited) {
      if (el_name %in% y) return(FALSE)
      visited_top <- visited_top + 1
      if (visited_top > visited_size) {
        visited_old <- visited
        visited_size_old <- visited_size
        visited_size <- visited_size * 2
        visited <- rep(FALSE, visited_size)
        visited[1:visited_size_old] <- visited_old
        names(visited[1:visited_size_old]) <- names(visited_old)
      }
      visited[visited_top] <- el
      names(visited)[visited_top] <- el_name
      if (el && !(el_name %in% z)) {
        visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
        visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
        n_vis_pa <- length(visitable_parents)
        n_vis_ch <- length(visitable_children)
        if (n_vis_pa + n_vis_ch > 0) {
          while (n_vis_pa + n_vis_ch + stack_top > stack_size) {
            stack_old <- stack
            stack_size_old <- stack_size
            stack_size <- stack_size * 2
            stack <- rep(FALSE, stack_size)
            stack[1:stack_size_old] <- stack_old
            names(stack[1:stack_size_old]) <- names(stack_old)
          }
          stack_add <- stack_top + n_vis_pa + n_vis_ch
          stack[(stack_top + 1):(stack_add)] <- c(rep(TRUE, n_vis_pa), rep(FALSE, n_vis_ch))
          names(stack)[(stack_top + 1):(stack_add)] <- c(visitable_parents, visitable_children)
          stack_top <- stack_add
        }
      } else if (!el) {
        if (!(el_name %in% z)) {
          visitable_children <- intersect(setdiff(children_unsrt(el_name, G), el_name), an_xyz)
          n_vis_ch <- length(visitable_children)
          if (n_vis_ch > 0) {
            while (n_vis_ch + stack_top > stack_size) {
              stack_old <- stack
              stack_size_old <- stack_size
              stack_size <- stack_size * 2
              stack <- rep(FALSE, stack_size)
              stack[1:stack_size_old] <- stack_old
              names(stack[1:stack_size_old]) <- names(stack_old)
            }
            stack_add <- stack_top + n_vis_ch
            stack[(stack_top + 1):(stack_add)] <- rep(FALSE, n_vis_ch)
            names(stack)[(stack_top + 1):(stack_add)] <- visitable_children
            stack_top <- stack_add
          }
        }
        if (el_name %in% an_z) {
          visitable_parents <- intersect(setdiff(parents_unsrt(el_name, G), el_name), an_xyz)
          n_vis_pa <- length(visitable_parents)
          if (n_vis_pa > 0) {
            while (n_vis_pa + stack_top > stack_size) {
              stack_old <- stack
              stack_size_old <- stack_size
              stack_size <- stack_size * 2
              stack <- rep(FALSE, stack_size)
              stack[1:stack_size_old] <- stack_old
              names(stack[1:stack_size_old] <- stack_old)
            }
            stack_add <- stack_top + n_vis_pa
            stack[(stack_top + 1):(stack_add)] <- rep(TRUE, n_vis_pa)
            names(stack)[(stack_top + 1):(stack_add)] <- visitable_parents
            stack_top <- stack_add
          }
        }
      }
    }
  }
  return(TRUE)
}