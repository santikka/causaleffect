\name{recover}
\alias{recover}
\title{Recover a causal effect from selection bias}
\description{This function attempts to recover the causal effect of the set of variables (\code{y})
given the intervention on the set of variables (\code{x}) in graph (\code{G}) containing a single selection variable. Otherwise
an error is thrown describing the graphical structure that witnesses non-identifiability. The vertex of (\code{G}) that corresponds to the selection variable must have a description parameter of a single character "S" (shorthand for "selection"). If \code{steps = TRUE}, returns instead
a list where the first element is the expression and the second element is a list of the intermediary steps taken by the algorithm. }

\usage{recover(y, x, G, expr = TRUE, simp = TRUE, 
  steps = FALSE, primes = FALSE, stop_on_nonid = TRUE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{G}{An \code{igraph} object describing a causal model with a single selection variable in the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}}
  \item{simp}{A logical value. If \code{TRUE}, a simplification procedure is applied to the resulting probability object. d-separation and the rules of do-calculus are applied repeatedly to simplify the expression. }
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the causal effect and the second to the a list describing intermediary steps taken by the algorithm. }
  \item{primes}{A logical value. If \code{TRUE}, prime symbols are appended to summation variables to make them distinct from their other instantiations. }
  \item{stop_on_nonid}{A logical value. If \code{TRUE}, an error is produced when a non-identifiable effect is discovered. Otherwise recursion continues normally. }
}

\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the interventional distribution. Otherwise, a list as described in the arguments. }
\references{

Bareinboim E., Tian J. 2015 Recovering Causal Effects From Selection Bias. \emph{In Proceedings of the 29th AAAI Conference on Artificial Intelligence}, 3475--3481.

}

\author{Santtu Tikka}

\seealso{ \code{\link{parse.graphml}}, \code{\link{get.expression}}, \code{\link{generalize}}, \code{\link{meta.transport}} }

\examples{
library(igraph)

# We set simplify = FALSE to allow multiple edges.
g <- graph.formula(W_1 -+ X, W_2 -+ X, X -+ Y, # Observed edges
  W_2 -+ S, # The selection variable S
  W_1 -+ W_2, W_2 -+ W_1, W_1 -+ Y, Y -+ W_1, simplify = FALSE)

# Here the bidirected edges are set to be unobserved in the selection diagram d.
# This is denoted by giving them a description attribute with the value "U".
# The first five edges are observed, the rest are unobserved.
g <- set.edge.attribute(g, "description", 5:8, "U")

# The variable "S" is a selection variable. This is denoted by giving it
# a description attribute with the value "S".
g <- set.vertex.attribute(g, "description", 5, "S")

recover(y = "Y", x = "X", G = g)
}

