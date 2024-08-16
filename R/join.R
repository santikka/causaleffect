#' Join
#'
#' \code{Join} and \code{insert} are essentially two variations of the underlying procedure of determining whether the terms of the atomic expression actually represent a joint distribution. \code{Join} is called when we are processing terms that already exist in the expression.
#' Attempts to combine two terms: the joint term \code{P(J|D)} obtained from \code{simplify()} and the
#' term \code{P(V|C) := P(Vk|Ck)} of the current iteration step. The goal is to
#' determine if these terms can be combined based on the d-separation criteria in the graph \code{G}.
#'
#'
#'
#' @param J character vector. Joint set \code{P(J|D)}; already processed and included in joint distribution
#' from previous \code{\link{simplify}} iteration. Initially, may be empty for the starting point of
#' the joint distribution. \code{vari} is added to expand it if d-separation conditions are met.
#' @param D character vector. Term \code{P(V|C) := P(Vk|Ck)}; set of variables that condition the joint distribution.
#' \code{Join} checks and updates \code{D} as necessary to maintain the validity of the joint distribution
#' when combined with \code{vari}.
#' @param vari character scalar. Current variable being considered for inclusion in the joint distribution.
#' @param cond character vector. Set of variables that condition the current variable \code{vari}. \code{Join} uses \code{cond}
#' to evaluate conditional independence and determine if \code{vari} can be added to \code{J}.
#' @param S likely a character vector. Not used directly in \code{join}. Current summation variable.
#' @param M character vector. Missing variables (variables not contained within the expression).
#' @param O character vector. Observed variables (variables contained within the expression).
#' @param G.unobs igraph object created with \code{igraph::unobserved.graph(G)}. Separate graph that turns bidirected edges into explicit nodes for unobserved confounders.
#' @param G igraph object created with \code{igraph::graph.formula()}. Main graph G. Includes bidirected edges.
#' @param G.obs igraph object created with \code{igraph::observed.graph(G)}. Separate graph that does not contain bidirected edges (only contains the directed edges with observed nodes).
#' @param topo igraph list object created with \code{igraph::topological.sort} and \code{igraph::get.vertex.attribute}. The topological ordering of the vertices in graph G.
#'
#' @dependencies This function depends on several functions from the causaleffect package, including: \link{powerset}, \link{wrap.dSep}, and \link{insert}.
#'
#' @return \code{Join} returns the joint result, or the original result if none of the conditions for joining were met.
#'
#' @references Tikka, S., & Karvanen, J. (2017). Simplifying probabilistic expressions in causal inference. Journal of Machine Learning Research, 18(36), 1-30.
#'
#' @author Haley Hummel,
#' Psychology PhD student at Oregon State University
#'
#' @seealso \code{\link{causal.effect}}, \code{\link{parse.expression}}, \code{\link{get.expression}}, \code{\link{probability}}
#'
#'
#' @examples
#' \dontrun{
#'
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
#'   # Step 6 - Inside nested while loop before join operation
#'   # P$children[[k]]$var: y (this represents vari in simplify())
#'   # P$children[[k]]$cond: z x (this represents cond in simplify())
#'   # P$sumset[j]: z (this reprensents S in simplify())
#'
#' J_1 <- character()
#' D_1 <- character()
#' vari_1 <- "y"
#' cond_1 <- c("z", "x")
#' S_1 <- "z"
#' M_1 <- "x"
#' O_1 <- c("z", "y")
#'
#' # we can obtain the following from the graph information:
#' # G.unobs = G_1.unobs
#' # G = G_1
#' # G.obs = G_1.obs
#' # topo = topo_1
#'
#' # we expect the output from this to be:
#' # [1] "y"
#' # [2] "z" "x"
#'
#' join(J_1, D_1, vari_1, cond_1, S_1, M_1, O_1, G_1.unobs, G_1, G_1.obs, topo_1)
#' }
#'
#' @seealso \code{\link{simplify}}, \code{\link{wrap.dSep}}, \code{\link{insert}}
#'
#' @keywords models manip math utilities
#' @concept probabilistic expressions
#' @concept graph theory
#' @concept causal inference


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
