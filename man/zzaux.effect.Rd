\name{aux.effect}
\alias{aux.effect}
\title{Identify a causal effect using surrogate experiments}
\description{This function returns an expression for the joint distribution of the set of variables (\code{y})
given the intervention on the set of variables (\code{x}) using auxiliary experiments on a set (\code{z}) if the effect is identifiable.  Otherwise
an error is thrown describing the graphical structure that witnesses unidentifiability. }

\usage{aux.effect(y, x, z, G, expr = TRUE, simp = TRUE, steps = FALSE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{z}{A character vector describing the additional set available for manipulation. }
  \item{G}{An \code{igraph} object describing the directed acyclic graph induced by the causal model that matches the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}}
  \item{simp}{A logical value. If \code{TRUE}, a simplification procedure is applied to the resulting probability object. d-separation and the rules of do-calculus are applied repeatedly to simplify the expression. }
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the causal effect and the second to the a list describing intermediary steps taken by the algorithm. }
}
\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the interventional distribution. Otherwise, a list as described in the arguments.}
\references{

Bareinboim E., Pearl J. 2012 Causal Inference by Surrogate Experiments: z-identifiability. \emph{Proceedings of the 28th Conference on Uncertainty in Artificial Intelligence}, 113--120.

}

\author{Santtu Tikka}

\seealso{ \code{\link{parse.graphml}}, \code{\link{get.expression}} }

\examples{
library(igraph)

# simplify = FALSE to allow multiple edges
f <- graph.formula(W -+ Z, Z -+ X, X -+ Y, W -+ Y, # Observed edges
  W -+ Y, Y -+ W, Z -+ Y, Y -+ Z, Z -+ X, X -+ Z, simplify = FALSE)

# Here the bidirected edges are set to be unobserved in graph g
# This is denoted by giving them a description attribute with the value "U"
# The first 4 edges correspond to the observed edges, the rest are unobserved

f <- set.edge.attribute(f, "description", 5:10, "U")
res <- aux.effect(y = "Y", x = "X", z = "Z", G = f)
}

