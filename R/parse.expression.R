#' Parse.expression
#'
#' The \code{`parse.expression`} function takes a probabilistic expression and processes it based on the topological order, unobserved and observed graphs, and the underlying graph structure to simplify or modify the expression.
#'
#' @param P probability object. The identified probabilistic expression taken from the output of \code{`causal.effect`}. Typically includes components such as numerator (`num`), denominator (`den`), product (`product`), summation set (`sumset`), and a fraction indicator (`fraction`).
#' @param topo igraph list object created with \code{igraph::topological.sort} and \code{igraph::get.vertex.attribute}. The topological ordering of the vertices in graph G.
#' @param G.unobs object created with \link{unobserved.graph(G)}. Separate graph that turns bidirected edges into explicit nodes for unobserved confounders.
#' @param G  object created with \code{igraph::graph.formula()}. Main graph G. Includes bidirected edges.
#' @param G.obs object created with \link{observed.graph(G)}. Separate graph that does not contain bidirected edges (only contains the directed edges with observed nodes).
#'
#' @dependencies This function depends on several functions from the causaleffect package, including: \link{simplify} and \link{probability}.
#'
#' @return A parsed probability object, potentially with adjusted summation sets and children, or `NULL` if the expression can be fully simplified away. This output can be used as the `P` for \link{simplify}.
#'
#' @details
#' The function recursively processes the input probability object (`P`) by applying rules based on the topological order and the graph structures. The function handles fractions, products, and summation sets, simplifying the expression where possible.
#'
#' If the expression involves a fraction, the function attempts to cancel out terms and simplify both the numerator and the denominator. It also handles product terms by recursively parsing the children of the product and adjusting the summation sets accordingly.
#'
#' The function ultimately returns a simplified expression or `NULL` if the expression reduces entirely.
#'
#' #' @references Tikka, S., & Karvanen, J. (2017). Simplifying probabilistic expressions in causal inference. Journal of Machine Learning Research, 18(36), 1-30.
#'
#' @author Haley Hummel,
#' Psychology PhD student at Oregon State University
#'
#' @examples
#' \dontrun{
#'
#'# defining graph information for G_1 using igraph
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
#' # For this example, the outputs for simp = TRUE vs. simp = FALSE are the same.
#'
#' causal.effect("y", "x", G = G_1, expr = FALSE, simp = TRUE)
#'
#' # causal.effect generates a probability structure, which can then be applied to be the
#' # input of the function parse.expression.
#' # the initial probabilistic expression should be: ∑z P(y|z,x)P(z)
#' # the simplified expression should look like: ∑z P(y|z,x)P(z)
#' # The expr = FALSE is key to NOT printing a string (e.g. in the above 2 lines) to generate a longer output.
#' P_1 <- probability(
#'   sumset = c("z"),
#'   product = TRUE,
#'   fraction = FALSE,
#'   sum = FALSE,
#'   children = list(
#'     probability(var = "y", cond = c("z", "x")),
#'     probability(var = "z", cond = character(0))
#'   ),
#'   den = list(),
#'   num = list(),
#'   domain = 0,
#'   weight = 0
#' )
#'
#' # now must define expected output from parse.expression
#' expected_output_1 <- probability(
#'   sumset = "z",
#'   product = TRUE,
#'   fraction = FALSE,
#'   sum = FALSE,
#'   children = list(
#'     probability(var = "y", cond = c("z", "x")),
#'     probability(var = "z", cond = character(0))
#'   ),
#'   den = list(),
#'   num = list(),
#'   domain = 0,
#'   weight = 0
#' )
#'
#' parse.expression(P_1, topo_1, G_1.unobs, G_1, G_1.obs), expected_output_1)
#'
#' }
#'
#' @export


parse.expression <- function(P, topo, G.unobs, G, G.obs) {
  # Check if the expression is a fraction
  if (P$fraction) {
    # If so, attempt to cancel out common terms in the fraction
    P <- cancel.out(P)
    # If it's still a fraction after cancellation, recursively parse the denominator
    if (P$fraction) {
      P$den <- parse.expression(P$den, topo, G.unobs, G, G.obs)
      # If the denominator becomes empty, update the summation set and children
      if (length(P$den) == 0) {
        sum_p <- P$sumset
        P <- P$num
        P$sumset <- union(sum_p, P$sumset) %ts% topo
        # If the numerator is a product, update its children
        if (P$product) {
          if (length(P$children) == 1) {
            sum_p <- P$sumset
            P <- P$children[[1]]
            P$sumset <- union(sum_p, P$sumset) %ts% topo
          }
        }
        return(P)
      }
      # Adjust the summation set if there are nodes that are independent of the denominator
      if (length(P$sumset) > 0 && length(P$den) > 0) {
        nodep <- setdiff(P$sumset, dependencies(P$den))
        if (length(nodep) > 0) {
          P$num$sumset <- union(P$num$sumset, nodep) %ts% topo
          P$sumset <- setdiff(P$sumset, nodep) %ts% topo
        }
      }
      # Recursively parse the numerator and attempt to cancel out terms again
      P$num <- parse.expression(P$num, topo, G.unobs, G, G.obs)
      P <- cancel.out(P)
    }
    return(P)
  }
  # Initialize flag to determine if terms should be simplified
  simplify_terms <- TRUE
  # If the expression is a product of other expressions, then identify non-atomic children (products, sums, or fractions)
  if (P$product) {
    non_atomic <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
    # If there are non-atomic children, parse them recursively
    if (sum(non_atomic) > 0) {
      parse_children <- P$children[non_atomic]
      P$children <- P$children[!non_atomic]
      for (i in 1:length(parse_children)) {
        P.parse <- parse.expression(parse_children[[i]], topo, G.unobs, G, G.obs)
        # If the parsed child has a collapse flag, merge its children into the current expression
        if (!is.null(P.parse$collapse)) {
          P$children <- c(P$children, P.parse$children)
        } else {
          P$children[[length(P$children) + 1]] <- P.parse
        }
      }
    }
    # If there are still non-atomic children after parsing, do NOT simplify terms
    if (length(P$children) > 0) {
      non_atomic <- sapply(P$children, FUN = function(x) (x$product || length(x$sumset) > 0 || x$fraction || x$sum))
      if (sum(non_atomic) > 0) simplify_terms <- FALSE
    } else return(NULL)
  }
  # If there are no variables left in the summation set, return the expression
  if (length(P$sumset) == 0) return(P)
  # If the expression is not a product and the summation set matches the variables, return NULL
  if (!P$product) {
    if (identical(P$sumset, P$var)) return(NULL)
    else return(P)
  }
  # If simplification is possible, order children and summation set, and simplify the expression
  if (simplify_terms) {
    ord.children <- order(unlist(lapply(P$children, FUN = function(x) which(topo == x$var))), decreasing = TRUE)
    ord.sum <- order(sapply(P$sumset, FUN = function(x) which(topo == x)), decreasing = TRUE)
    P$children <- P$children[ord.children]
    P$sumset <- P$sumset[ord.sum]
    P <- simplify(P, topo, G.unobs, G, G.obs)
    # If all children have been simplified away, return NULL
    if (length(P$children) == 0) return(NULL)
  }
  # Initialize a new probability object to hold any children that can be removed from summation
  P.parse <- probability(product = TRUE, children = list())
  remove <- c()
  j <- 0
  # Iterate over children to identify those independent of the summation set
  if (length(P$sumset) > 0) {
    for (i in 1:length(P$children)) {
      dep <- dependencies(P$children[[i]])
      if (length(intersect(dep, P$sumset)) == 0) {
        remove <- c(remove, i)
        j <- j + 1
      }
    }
  } else return(P)
  # If any children can be removed, add them to the new probability object
  if (j > 0) {
    P.parse$children <- P$children[remove]
    P.parse$collapse <- TRUE
    P$children <- P$children[-remove]
    # If only one child remains, update the summation set and recursively parse the child
    if (length(P$sumset) > 0) {
      if (length(P$children) == 1) {
        sum_p <- P$sumset
        P <- P$children[[1]]
        P$sumset <- union(sum_p, P$sumset) %ts% topo
        P <- parse.expression(P, topo, G.unobs, G, G.obs)
      }
    }
    # Add the remaining child to the new probability object and return it
    if (length(P$children) > 0) P.parse$children[[j + 1]] <- P
    return(P.parse)
  }
  return(P)
}
