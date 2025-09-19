#' Insert
#'
#' \code{Insert} will insert a missing variable into a joint distribution \eqn{P(J|D)}
#' using d-separation criteria in a given graph \code{G}.
#' It is called when there are variables without corresponding terms in the expression
#'
#' @param J character vector. The set of variables representing the joint distribution.
#' @param D character vector. The set of variables representing the conditioning set of the joint distribution.
#' @param M character vector. The variable to be inserted.
#' @param cond character vector. The set of conditioning variables.
#' @param S character vector. The current summation variable.
#' @param O character vector. The set of observed variables.
#' @param G.unobs igraph object created with \code{igraph::unobserved.graph(G)}. Separate graph that turns bidirected edges into explicit nodes for unobserved confounders.
#' @param G igraph object created with \code{igraph::graph.formula()}. Main graph G. Includes bidirected edges.
#' @param G.obs igraph object created with \code{igraph::observed.graph(G)}. Separate graph that does not contain bidirected edges (only contains the directed edges with observed nodes).
#' @param topo igraph list object created with \code{igraph::topological.sort} and \code{igraph::get.vertex.attribute}. The topological ordering of the vertices in graph G.
#'
#' @dependencies This function depends on several functions from the causaleffect package, including: \link{powerset} and \link{wrap.dSep}.
#'
#' @return \code{Insert} returns a list with:
#' \code{J.new}{character vector. An updated set of joint distribution variables.}
#' \code{D.new}{character vector. An updated set of conditioning variables.}
#' \code{M}{character vector. The inserted variable.}
#' \code{ds[[i]]}{character vector. The subset from the power set used in the insertion.}
#' However, if no conditions were met, \code{insert} will return the original \code{J} and \code{D}.
#'
#' @references Tikka, S., & Karvanen, J. (2017). Simplifying probabilistic expressions in causal inference. Journal of Machine Learning Research, 18(36), 1-30.
#'
#' @author Haley Hummel,
#' Psychology PhD student at Oregon State University
#'
#' @examples
#' \dontrun{
#' # defining graph information for G_1 using igraph
#' G_1 <- graph.formula(x -+ y, z -+ x, z -+ y , x -+ z, z -+ x, simplify = FALSE)
#' G_1 <- set.edge.attribute(graph = G_1, name = "description", index = c(4,5), value = "U")
#'
#' # defining observed nodes of graph G_1 using igraph
#' G_1.obs <- observed.graph(G_1)
#'
#' # defining unobserved nodes of graph G_1 using igraph
#' G_1.unobs <- unobserved.graph(G_1)
#'
#' # defining topological sort of graph G_1 using igraph
#' topo_1 <- igraph::topological.sort(G_1.obs)
#' topo_1 <- igraph::get.vertex.attribute(G_1, "name")[topo_1]
#'
#' # we can obtain the following from running simplify(P_1, topo_1, G_1.unobs, G_1,
#' # G_1.obs) with break points (the browser() function). I added print statements
#' # after step #5 in simplify():
#' # Step 6 - Inside nested while loop before join operation
#' # P$children[[k]]$var: y (this represents vari in simplify())
#' # P$children[[k]]$cond: z x (this represents cond in simplify())
#' # P$sumset[j]: z (this reprensents S in simplify())
#'
#' J_1 <- character()
#' D_1 <- character()
#' M_1 <- "x"
#' cond_1 <- c("z", "x")
#' S_1 <- "z"
#' O_1 <- c("z", "y")
#'
#' # we can obtain the following from the graph information:
#' # G.unobs = G_1.unobs
#' # G = G_1
#' # G.obs = G_1.obs
#' # topo = topo_1
#'
#' # we expect the output from this (representing J, D) to be:
#' # [1] character(0)
#' # [2] character(0)
#'
#' insert(J_1, D_1, M_1, cond_1, S_1, O_1, G_1.unobs, G_1, G_1.obs, topo_1)
#' }
#'
#' @seealso \code{\link{join}}, \code{\link{simplify}}, \code{\link{wrap.dSep}}, \code{\link{powerset}}
#'
#' @keywords models manip math utilities
#' @keywords graphs methods multivariate distribution probability
#' @concept probabilistic expressions
#' @concept graph theory
#' @concept joint distribution
#' @concept causal inference
#' @concept d-separation

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
