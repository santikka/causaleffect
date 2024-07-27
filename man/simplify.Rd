\name{simplify}
\alias{simplify}
\title{
Simplify
}
\description{
This function algebraically simplifies probabilistic expressions given by the ID algorithm from causal.effect. It always attempts to perform maximal simplification, meaning that as many variables of the set are removed as possible. If the simplification in terms of the entire set cannot be completed, the intermediate result with as many variables simplified as possible should be returned.

Run causal.effect with the graph information first, then use the output of causal.effect as the P in parse.expression. Use the output from parse.expression as the P in simplify.

For further information, see Tikka & Karvanen (2017) "Simplifying Probabilistic Expressions in Causal Inference" Algorithm 1.
}
\usage{
simplify(P, topo, G.unobs, G, G.obs)
}
\arguments{
  \item{P}{
    Probabilistic expression that will be simplified.
  }
  \item{topo}{
    Topological ordering of the vertices in graph G.
  }
  \item{G.unobs}{
    Unobserved nodes in graph G.
  }
  \item{G}{
    Graph G.
  }
  \item{G.obs}{
    Observed nodes in graph G.
  }
}
}
\details{
This function depends on several functions from the \code{causal.effect} package, including: \code{irrelevant}, \code{wrap.dSep}, \code{dSep}, \code{join}, \code{ancestors}, \code{factorize}, \code{parents}, \code{children}, and \code{powerset}.
}
\value{
simplify() will return the simplified atomic expression in a list structure. For example (from example below):
$var
character(0)

$cond
character(0)

$sumset
[1] "z"

$do
character(0)

$product
[1] TRUE

$fraction
[1] FALSE

$sum
[1] FALSE

$children
$children[[1]]
$var
[1] "y"

$cond
[1] "z" "x"

$sumset
character(0)

$do
character(0)

$product
[1] FALSE

$fraction
[1] FALSE

$sum
[1] FALSE

$children
list()

$den
list()

$num
list()

$domain
[1] 0

$weight
[1] 0

attr(,"class")
[1] "probability"

$children[[2]]
$var
[1] "z"

$cond
character(0)

$sumset
character(0)

$do
character(0)

$product
[1] FALSE

$fraction
[1] FALSE

$sum
[1] FALSE

$children
list()

$den
list()

$num
list()

$domain
[1] 0

$weight
[1] 0

attr(,"class")
[1] "probability"


$den
list()

$num
list()

$domain
[1] 0

$weight
[1] 0

attr(,"class")
[1] "probability"

This long list structure can be converted into a string by the \code{get.expression} function. For example:

string_expression <- simplify(P, topo, G.unobs, G, G.obs)
get.expression(string_expression)

The resulting string should look like (from example below): "\\sum_{w}P(y|w,x)P(w)" 
}
\references{
Tikka, S., & Karvanen, J. (2017). Simplifying probabilistic expressions in causal inference. Journal of Machine Learning Research, 18(36), 1-30.
}
\author{
Haley Hummel
Psychology PhD student at Oregon State University
}
\note{
}
\seealso{\code{\link{causal.effect}}, \code{\link{parse.expression}}, \code{\link{get.expression}} }
}
\examples{
# defining graph information for G_1 using igraph
G_1 <- graph.formula(x -+ y, z -+ x, z -+ y , x -+ z, z -+ x, simplify = FALSE)
G_1 <- set.edge.attribute(graph = G_1, name = "description", index = c(4,5), value = "U")

# defining observed nodes of graph G_1 using igraph
G_1.obs <- observed.graph(G_1)

#defining unobserved nodes of graph G_1 using igraph
G_1.unobs <- unobserved.graph(G_1)

# defining topological sort of graph G_1 using igraph
topo_1 <- igraph::topological.sort(G_1.obs)
topo_1 <- igraph::get.vertex.attribute(G_1, "name")[topo_1]

# run causal.effect. simp = TRUE vs. simp = FALSE matters — as a simplification 
# procedure is applied to the resulting probability object if simp = TRUE. 
# d-separation and the rules of do-calculus are applied repeatedly to simplify 
# the expression. The procedure is NOT applied if simp = FALSE.
\code{causal.effect("y", "x", G = G_1, expr = FALSE, simp = TRUE)}

# causal.effect generates a probability structure, which can then be applied to be the
# input of the function parse.expression. 
parse.expression(causal_effect_output, topo_1, G_1.unobs, G_1, G_1.obs)

# parse.expression generates a list structure, which can then be applied to be the
# input of the simplify function. 
# call simplify function, which will print out the simplified list structure
simplify(parse_expression_output, topo_1, G_1.unobs, G_1, G_1.obs)

{
  }
}
\keyword{models}
\keyword{manip}
\keyword{math}
\keyword{utilities}
\concept{probabilistic expressions}
\concept{graph theory}
