#' Simplify
#'
#' This function algebraically simplifies probabilistic expressions given by the ID algorithm from \link{causal.effect}. It always attempts to perform maximal simplification, meaning that as many variables of the set are removed as possible. If the simplification in terms of the entire set cannot be completed, the intermediate result with as many variables simplified as possible should be returned.
#'
#' Run \link{causal.effect} with the graph information first, then use the output of \link{causal.effect} as the \code{P} in \link{parse.expression}. Use the output from \link{parse.expression} as the \code{P} in \code{simplify}.
#'
#' For further information, see Tikka & Karvanen (2017) "Simplifying Probabilistic Expressions in Causal Inference" Algorithm 1.
#'
#' @param P probability object created with \link{probability()}. The probabilistic expression that will be simplified.
#' @param topo igraph list object created with \code{igraph::topological.sort} and \code{igraph::get.vertex.attribute}. The topological ordering of the vertices in graph G.
#' @param G.unobs object created with \link{unobserved.graph(G)}. Separate graph that turns bidirected edges into explicit nodes for unobserved confounders.
#' @param G  object created with \code{igraph::graph.formula()}. Main graph G. Includes bidirected edges.
#' @param G.obs object created with \link{observed.graph(G)}. Separate graph that does not contain bidirected edges (only contains the directed edges with observed nodes).
#'
#' @dependencies This function depends on several functions from the causaleffect package, including: \link{irrelevant}, \link{wrap.dSep}, \link{dSep}, \link{join}, \link{ancestors}, \link{factorize}, \link{parents}, \link{children}, and \link{powerset}.
#'
#' @return \code{simplify()} will return the simplified atomic expression in a list structure. For example (from example below):
#'  \preformatted{
#'  $var: character(0)
#'
#'  $cond: character(0)
#'
#'  $sumset: [1] "z"
#'
#'  $do: character(0)
#'
#'  $product: [1] TRUE
#'
#'  $fraction: [1] FALSE
#'
#'  $sum: [1] FALSE
#'
#'  $children: list()
#'
#'  $den: list()
#'
#'  $num: list()
#'
#'  $domain: [1] 0
#'
#'  $weight: [1] 0
#'
#'  $attr(,"class"): [1] "probability"}
#'
#'
#' This long list structure can be converted into a string formatted in LaTeX syntax by the \code{get.expression} function. For example:
#'
#'
#' \preformatted{string_expression <- simplify(P, topo, G.unobs, G, G.obs)
#' get.expression(string_expression)
#'
#' The resulting string should look like (from example below): "\\sum_{w}P(y|w,x)P(w)"}
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
#' # run causal.effect. simp = TRUE vs. simp = FALSE matters — as a simplification
#' # procedure is applied to the resulting probability object if simp = TRUE.
#' # d-separation and the rules of do-calculus are applied repeatedly to simplify
#' # the expression. The procedure is NOT applied if simp = FALSE.
#' causal.effect("y", "x", G = G_1, expr = FALSE, simp = TRUE)
#'
#' # causal.effect generates a probability structure, which can then be applied to be the
#' # input of the function parse.expression.
#' parse.expression(causal_effect_output, topo_1, G_1.unobs, G_1, G_1.obs)
#'
#' # parse.expression generates a list structure, which can then be applied to be the
#' # input of the simplify function.
#' # call simplify function, which will print out the simplified list structure
#' simplify(parse_expression_output, topo_1, G_1.unobs, G_1, G_1.obs)
#' }
#'
#' @keywords models manip math utilities
#' @concept probabilistic expressions
#' @concept graph theory
#' @concept causal inference


simplify <- function(P, topo, G.unobs, G, G.obs) {
# initialize j to 0
  j <- 0
# WHILE loop runs until all elements in P['sumset'] are processed
  while (j < length(P$sumset)) {
# make a copy of original expression P (P.orig) used to go back to original
# expression if simplification does not work
    P.orig <- P
    irl.len <- 0
    irrel <- NULL
    terms <- list()
    vars <- sapply(P$children, "[[", "var")
# initialize all other variables
    j <- j + 1
    i <- which(vars == P$sumset[j])
    k <- 1
    R.var <- character()
    R.var <- character()
    R.cond <- list()
    J <- character()
    D <- character()
# remove irrelevant terms from expression to reduce complexity
    if (i > 1) {
      irrel <- irrelevant(P$children[1:(i-1)], P$sumset[j], P$sumset, G.unobs)
      irl.len <- length(irrel)
      if (irl.len > 0) {
        i <- i - irl.len
        terms <- P$children[irrel]
        P$children[irrel] <- NULL
        vars <- vars[-irrel]
      }
    }
# topological sorting - separate variables into Missing (M) and Observed (O)
    M <- topo[!(topo %in% vars)]
    O <- topo[(topo %in% vars)]
# perform join operation (join components of expression to simplify). If successful,
# update the components accordingly. If fails, break loop & reset expression.
    while (k <= i) {
      joint <- join(J, D, P$children[[k]]$var, P$children[[k]]$cond, P$sumset[j], M, O, G.unobs, G, G.obs, topo)
      if (length(joint[[1]]) <= length(J)) {
        J <- character()
        D <- character()
        k <- 1
        break
      } else {
        J <- joint[[1]]
        D <- joint[[2]]
        if (length(joint) > 2) {
          R.var <- union(R.var, joint[[3]])
          R.cond <- c(R.cond, list(joint[[4]]))
          M <- setdiff(M, R.var)
        } else {
          k <- k + 1
        }
      }
    }
# if simplification possible, factorize expression using intermediate sets and
# update sumset.Check for further elimination of redundant terms using cancel().
    if (k == i + 1) {
      P <- factorize(J, D, P, topo, i)
      S <- P$sumset[j]
      P$sumset <- P$sumset[-j]
      if (length(R.var) > 0) {
        P.cancel <- cancel(P, R.var, R.cond, S)
        if (identical(P.cancel, P)) P <- P.orig
        else {
          P <- P.cancel
          j <- 0
        }
      } else j <- 0
      if (irl.len > 0) P$children <- c(terms, P$children)
    } else P <- P.orig
  }
# return simplified expression, or the original if simplification was not possible.
  return(P)
}
