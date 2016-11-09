\name{get.expression}
\alias{get.expression}
\title{Get the expression of a probability object}
\description{This function converts an object of class \code{probability} returned by \code{aux.effect}, \code{causal.effect}, \code{generalize}, \code{meta.transport}, \code{recover} or \code{transport} with \code{expr = FALSE} into a string which represents the probability distribution.  Currently only LaTeX syntax is available.  }

\usage{get.expression(x)}

\arguments{
  \item{x}{An object of class \code{probability} which is an internal list structure describing the interventional distribution.}
}
\value{A character string that describes the resulting distribution in LaTeX syntax. }

\author{Santtu Tikka}

\seealso{ \code{\link{aux.effect}}, \code{\link{causal.effect}}, \code{\link{generalize}}, \code{\link{meta.transport}}, \code{\link{recover}}, \code{\link{transport}} }

\examples{
library(igraph)

# simplify = FALSE to allow multiple edges
g <- graph.formula(X -+ Y, Z -+ X, Z -+ Y , X -+ Z, Z -+ X, simplify = FALSE) 

# Here the bidirected edge between X and Z is set to be unobserved in graph g
# This is denoted by giving them a description attribute with the value "U"
# The edges in question are the fourth and the fifth edge
g <- set.edge.attribute(graph = g, name = "description", index = c(4,5), value = "U") 

x <- causal.effect(y = "Y", x = "X", z = NULL, G = g, expr = FALSE)
get.expression(x)
}

