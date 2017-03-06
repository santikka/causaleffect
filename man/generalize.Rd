\name{generalize}
\alias{generalize}
\title{Derive a transport formula for a causal effect between a target domain and multiple source domains with limited experiments}
\description{This function returns an expression for the transport formula of a causal effect between a target domain and multiple source domains with limited experiments. The formula is returned for the interventional distribution of the set of variables (\code{y}) given the intervention on the set of variables (\code{x}). Available experiments are depicted by a list (\code{Z}) where the first element describes the elements available at the target and the rest at the sources. The multiple domains are given as a list (\code{D}) where the first element is the underlying causal diagram without selection variables, and the rest correspond to the selection diagrams. If the effect is non-transportable, an error is thrown describing the graphical structure that witnesses non-transportability. The vertices of any diagram in (\code{D}) that correspond to selection variables must have a description parameter of a single character "S" (shorthand for "selection").  }
\usage{generalize(y, x, Z, D, expr = TRUE, simp = FALSE, steps = FALSE, primes = FALSE)}

\arguments{
  \item{y}{A character vector of variables of interest given the intervention. }
  \item{x}{A character vector of the variables that are acted upon. }
  \item{Z}{A list of character vectors describing the available interventions at each domain. }
  \item{D}{A list of \code{igraph} objects describing the selection diagrams in the internal syntax. }
  \item{expr}{A logical value. If \code{TRUE}, a string is returned describing the expression in LaTeX syntax. Else, a list structure is returned which can be manually parsed by the function \code{get.expression}}
  \item{simp}{A logical value. If \code{TRUE}, a simplification procedure is applied to the resulting probability object. d-separation and the rules of do-calculus are applied repeatedly to simplify the expression. }
  \item{steps}{A logical value. If \code{TRUE}, returns a list where the first element corresponds to the expression of the transport formula and the second to the a list describing intermediary steps taken by the algorithm.}
  \item{primes}{A logical value. If \code{TRUE}, prime symbols are appended to summation variables to make them distinct from their other instantiations. }
}

\value{If \code{steps = FALSE}, A character string or an object of class \code{probability} that describes the transport formula. Otherwise, a list as described in the arguments.}
\references{

Bareinboim E., Pearl J. 2014 Transportability from Multiple Environments with Limited Experiments: Completeness Results. \emph{Proceedings of the 27th Annual Conference on Neural Information Processing Systems}, 280--288.

}

\author{Santtu Tikka}

\seealso{ \code{\link{aux.effect}}, \code{\link{causal.effect}}, \code{\link{get.expression}}, \code{\link{meta.transport}}, \code{\link{parse.graphml}}, \code{\link{recover}}, \code{\link{transport}} }

\examples{
library(igraph)

# Selection diagram corresponding to the target domain (no selection variables).
# We set simplify = FALSE to allow multiple edges.
d1 <-  graph.formula(Z_1 -+ X, Z_2 -+ X, X -+ Z_3, Z_3 -+ W, 
 Z_3 -+ U, U -+ Y, W -+ U, Z_1 -+ Z_3, # Observed edges
 Z_1 -+ Z_2, Z_2 -+ Z_1, Z_1 -+ X, X -+ Z_1, 
 Z_2 -+ Z_3, Z_3 -+ Z_2, Z_2 -+ U, U -+ Z_2, 
 W -+ Y, Y -+ W, simplify = FALSE)

# Here the bidirected edges are set to be unobserved in the selection diagram d1.
# This is denoted by giving them a description attribute with the value "U".
# The first 8 edges are observed and the next 10 are unobserved.
d1 <- set.edge.attribute(d1, "description", 9:18, "U")

# We can use the causal diagram d1 to create selection diagrams 
# for two source domains, a and b. %u% is the igraph union operator.
d1a <- d1 %u% graph.formula(S_1 -+ Z_2, S_2 -+ Z_3, S_3 -+ W)

# The variables "S_1", "S_2", and "S_3" are selection variables.
# This is denoted by giving them a description attribute with the value "S".
# The graph already has 7 vertices, so the last three depict the new ones.
d1a <- set.vertex.attribute(d1a, "description", 8:10, "S")

# Selection diagram corresponding to the second 
# source domain is constructed in a similar fashion.
d1b <- d1 %u% graph.formula(S_1 -+ Z_1, S_2 -+ W, S_3 -+ U)
d1b <- set.vertex.attribute(d1b, "description", 8:10, "S")

# We combine the diagrams as a list.
d.comb <- list(d1, d1a, d1b)

# We still need the available experiments at each domain.
z <- list(c("Z_1"), c("Z_2"), c("Z_1"))
# This denotes that the variable "Z_1" is available for intervention 
# in both the target domain, and the second source domain. 
# The variable "Z_2" is available for intervention in the first source domain.

res <- generalize(y = "Y", x = "X", Z = z, D = d.comb)
}

