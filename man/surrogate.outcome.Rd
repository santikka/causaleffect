\name{surrogate.outcome}
\alias{surrogate.outcome}
\title{Derive a formula for a causal effect using surrogate outcomes}
\description{This function returns an expression for the causal effect of interest using surrogate outcomes. The formula is returned for the interventional distribution of the set of variables (\code{y}) given the intervention on the set of variables (\code{x}). Available experimental data are depicted by a list (\code{S}) where each element is a list with two elements, \code{Z} and \code{W}, that are character vectors describing the experiments and the outcome variables, respectively. }
\usage{surrogate.outcome(y, x, S, G, expr = TRUE, steps = FALSE, primes = FALSE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{S}{A list describing the available experimental data. }
  \item{G}{An \code{igraph} object describing the directed acyclic graph induced by the causal model that matches the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}}
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the transport formula and the second to the a list describing intermediary steps taken by the algorithm.}
  \item{primes}{A logical value. If \code{TRUE}, prime symbols are appended to summation variables to make them distinct from their other instantiations. }
}

\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the causal effect. Otherwise, a list as described in the arguments.}
\references{

Bareinboim E., Pearl J. 2014 Transportability from Multiple Environments with Limited Experiments: Completeness Results. \emph{Proceedings of the 27th Annual Conference on Neural Information Processing Systems}, 280--288.

}

\author{Santtu Tikka}

\seealso{ \code{\link{generalize}}, \code{\link{causal.effect}}, \code{\link{get.expression}} }

\examples{
library(igraph)

# We set simplify = FALSE to allow multiple edges.
g <-  graph.formula(W -+ X, W -+ Z, X -+ Z, Z -+ Y, # Observed edges
  X -+ Z, Z -+ X, simplify = FALSE)

# We set the bidirected edges
g <- set.edge.attribute(g, "description", 5:6, "U")

# We construct the set of available experimental data
s <- list(
  list(Z = c("X"), W = c("Z"))
)

surrogate.outcome(y = "Y", x = "X", S = s, G = g)
}

