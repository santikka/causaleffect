\name{causal.effect}
\alias{causal.effect}
\title{Identify a causal effect}
\description{This function returns an expression for the joint distribution of the set of variables (\code{y})
given the intervention on the set of variables (\code{x}) conditional on (\code{z}) if the effect is identifiable.  Otherwise
an error is thrown describing the graphical structure that witnesses non-identifiability.  If \code{steps = TRUE}, returns instead
a list where the first element is the expression and the second element is a list of the intermediary steps taken by the algorithm.}

\usage{causal.effect(y, x, z = NULL, G, expr = TRUE, simp = FALSE, steps = FALSE, primes = FALSE, prune = FALSE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{z}{A character vector of the conditioning variables. }
  \item{G}{An \code{igraph} object describing the directed acyclic graph induced by the causal model that matches the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}. }
  \item{simp}{A logical value. If \code{TRUE}, a simplification procedure is applied to the resulting probability object. d-separation and the rules of do-calculus are applied repeatedly to simplify the expression. }
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the causal effect and the second to the a list describing intermediary steps taken by the algorithm.}
  \item{primes}{A logical value. If \code{TRUE}, prime symbols are appended to summation variables to make them distinct from their other instantiations. }
  \item{prune}{A logical value. If \code{TRUE}, additional steps are taken to remove variables that are not necessary for identification. }
}

\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the interventional distribution. Otherwise, a list as described in the arguments.}
\references{

Shpitser I., Pearl J. 2006 Identification of Joint Interventional Distributions in Recursive semi-Markovian Causal Models.
\emph{Proceedings of the 21st National Conference on Artificial Intelligence}, \bold{2}, 1219--1226. 

Shpitser I., Pearl J. 2006 Identification of Conditional Interventional Distributions. 
\emph{Proceedings of the 22nd Conference on Uncertainty in Artificial Intelligence}, 427--444. }

\author{Santtu Tikka}

\seealso{ \code{\link{parse.graphml}}, \code{\link{get.expression}} }

\examples{
library(igraph)

# simplify = FALSE to allow multiple edges
g <- graph.formula(x -+ y, z -+ x, z -+ y , x -+ z, z -+ x, simplify = FALSE)

# Here the bidirected edge between X and Z is set to be unobserved in graph g
# This is denoted by giving them a description attribute with the value "U"
# The edges in question are the fourth and the fifth edge
g <- set.edge.attribute(graph = g, name = "description", index = c(4,5), value = "U")
causal.effect(y = "y", x = "x", G = g)

# Examples on pruning
p1 <- graph.formula(w_1 -+ w_2, w_1 -+ x, w_2 -+ x, z_2 -+ x, x -+ z_1,
  z_2 -+ z_1, z_2 -+ z_3, z_4 -+ z_1, z_1 -+ y, z_3 -+ y, 
  w_1 -+ w_2, w_2 -+ w_1, w_1 -+ x, x -+ w_1, w_2 -+ x, x -+ w_2, 
  z_4 -+ z_1, Z_1 -+ z_4, x -+ y, y -+ x, z_2 -+ y, y -+ z_2, x -+ z_3,
  z_3 -+ x, x -+ z_2, z_2 -+ x, simplify = FALSE)
p1 <- set.edge.attribute(p1, "description", 11:26, "U")
causal.effect("y", "x", G = p1, simp = FALSE, primes = TRUE, prune = TRUE)

p2 <- graph.formula(w_1 -+ x_1, x_1 -+ y_1, w_2 -+ w_1, w_2 -+ x_2,
  x_2 -+ z, z -+ y_2, z -+ y_1, w_1 -+ w_2, w_2 -+ w_1, w_1 -+ x_1, x_1 -+ w_1,
  w_2 -+ x_2, x_2 -+ w_2, x_1 -+ z, z -+ x_1, x_2 -+ y_2, y_2 -+ x_2, x_1 -+ x_2, x_2 -+ x_1, simplify = FALSE)
p2 <- set.edge.attribute(p2, "description", 8:19, "U")
causal.effect(c("y_1", "y_2"), c("x_1", "x_2"), G = p2, simp = TRUE, primes = TRUE, prune = TRUE)

p3 <- graph.formula(x -+ y, z_1 -+ x, z_2 -+ z_1, w_1 -+ z_2, w_1 -+ w_2, w_2 -+ z_2,
  x -+ z_2, z_2 -+ x, z_2 -+ y, y -+ z_2, w_1 -+ w_2, w_2 -+ w_1, w_2 -+ z_2, z_2 -+ w_2, simplify = FALSE)
p3 <- set.edge.attribute(p3, "description", 7:14, "U")
causal.effect("y", "x", G = p3, simp = FALSE, primes = TRUE, aug = TRUE)

p4 <- graph.formula(x -+ z_1, z_1 -+ y, z_3 -+ y, z_2 -+ x, z_2 -+ z_1, z_2 -+ z_3,
  x -+ y, y -+ x, x -+ z_3, z_3 -+ x, x -+ z_2, z_2 -+ x, y -+ z_2, z_2 -+ y, simplify = FALSE)
p4 <- set.edge.attribute(graph = p4, name = "description", index = 7:14, value = "U")
causal.effect("y", "x", G = p4, primes = TRUE, primes = TRUE, prune = TRUE)

p5 <- graph.formula(x -+ z_4, z_4 -+ y, z_1 -+ x, z_2 -+ z_1, z_3 -+ z_2, z_3 -+ x,
  z_5 -+ z_1, z_5 -+ z_4, x -+ z_2, z_2 -+ x, z_3 -+ z_2, z_2 -+ z_3, z_2 -+ y, y -+ z_2,
  z_4 -+ y, y -+ z_4, z_5 -+ z_4, z_4 -+ z_5, simplify = FALSE)
p5 <- set.edge.attribute(p5, "description", 9:18, "U")
causal.effect("y", "x", G = p5, primes = TRUE, simp = TRUE, prune = TRUE)