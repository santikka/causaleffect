\name{verma.constraints}
\alias{verma.constraints}
\title{Find Verma constraints for a given graph}
\description{
This functions computes functional constraints known as Verma constraints for a joint distribution of a given semi-Markovian causal model.
}
\usage{verma.constraints(G)}
\arguments{
  \item{G}{An \code{igraph} object describing the directed acyclic graph induced by the causal model that matches the internal syntax. }
}

\value{A list of lists, each with five components corresponding to the functional constraint. The two equal c-factors that imply the functional independence are described by \code{lhs.cfactor} and \code{rhs.cfactor} and their expressions are given by \code{lhs.expr} and \code{rhs.expr} respectively. The independent variables are given by \code{vars}.}

\references{

Tian, J., Pearl J. 2002 On Testable Implications of Causal Models with Hidden variables.
\emph{Proceedings of the Eighteenth Conference on Uncertainty in Artificial Intelligence}, 519--527.

}

\author{Santtu Tikka}