\name{meta.transport}
\alias{meta.transport}
\title{Derive a transport formula for a causal effect between a target domain and multiple source domains}
\description{This function returns an expression for the transport formula of a causal effect between a target domain and multiple source domains. The formula is returned for the interventional distribution of the set of variables (\code{y}) given the intervention on the set of variables (\code{x}). The multiple source domains are given as a list of selection diagrams (\code{D}). If the effect is non-transportable, an error is thrown describing the graphical structure that witnesses non-transportability. The vertices of any diagram in (\code{D}) that correspond to selection variables must have a description parameter of a single character "S" (shorthand for "selection"). }
\usage{meta.transport(y, x, D, expr = TRUE, simp = TRUE, 
  steps = FALSE, primes = FALSE, stop_on_nonid = TRUE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{D}{A list of \code{igraph} objects describing the selection diagrams in the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}}
  \item{simp}{A logical value. If \code{TRUE}, a simplification procedure is applied to the resulting probability object. d-separation and the rules of do-calculus are applied repeatedly to simplify the expression. }
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the transport formula and the second to the a list describing intermediary steps taken by the algorithm. }
  \item{primes}{A logical value. If \code{TRUE}, prime symbols are appended to summation variables to make them distinct from their other instantiations. }
  \item{stop_on_nonid}{A logical value. If \code{TRUE}, an error is produced when a non-identifiable effect is discovered. Otherwise recursion continues normally. }
}

\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the transport formula. Otherwise, a list as described in the arguments.}
\references{

Bareinboim E., Pearl J. 2013b Meta-Transportability of Causal Effects: A Formal Approach. \emph{Proceedings of the 16th International Conference on Artificial Intelligence and
Statistics}, 135--143.

}

\author{Santtu Tikka}

\seealso{ \code{\link{parse.graphml}}, \code{\link{get.expression}}, \code{\link{transport}}, \code{\link{generalize}}}

\examples{
library(igraph)

# Selection diagram corresponding to the first source domain.
# We set simplify = FALSE to allow multiple edges.
d1 <- graph.formula(X -+ Z, W_1 -+ W_2, W_2 -+ Z, 
  W_3 -+ Z, X -+ W_3, W_2 -+ X, Z -+ Y, # Observed edges
  S_1 -+ X, S_2 -+ W_2, S_3 -+ W_3, S_4 -+ Y, # Edges related to selection variables
  X -+ W_3, W_3 -+ X, X -+ W_2, W_2 -+ X, X -+ W_1, 
  W_1 -+ X, W_1 -+ Z, Z -+ W_1, simplify = FALSE)

# Here the bidirected edges are set to be unobserved in the selection diagram d1.
# This is denoted by giving them a description attribute with the value "U".
# The first 7 edges are observed and the next 4 are related to the selection variables.
# The rest of the edges are unobserved.
d1 <- set.edge.attribute(d1, "description", 12:19, "U")

# The variables "S_1", "S_2", "S_3" and "S_4" are selection variables.
# This is denoted by giving them a description attribute with the value "S".
d1 <- set.vertex.attribute(d1, "description", 7:10, "S")

# Selection diagram corresponding to the second 
# source domain is constructed in a similar fashion.
d2 <- graph.formula(X -+ Z, W_1 -+ W_2, W_2 -+ Z, W_3 -+ Z, 
 X -+ W_3, W_2 -+ X, Z -+ Y, # Observed edges
 S_1 -+ X, S_2 -+ W_2, S_3 -+ W_1, 
 S_4 -+ Y, S_5 -+ Z, # Edges related to selection variables
 X -+ W_3, W_3 -+ X, X -+ W_2, W_2 -+ X, X -+ W_1, 
 W_1 -+ X, W_1 -+ Z, Z -+ W_1, simplify = FALSE)
d2 <- set.edge.attribute(d2, "description", 13:20, "U")
d2 <- set.vertex.attribute(d2, "description", 7:11, "S")

# We combine the diagrams as a list.
d.comb <- list(d1, d2)
meta.transport(y = "Y", x = "X", D = d.comb)
}
