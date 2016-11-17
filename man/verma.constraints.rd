\name{verma.constraints}
\alias{verma.constraints}
\title{Find Verma constraints for a given graph}
\description{
This functions computes functional constraints known as Verma constraints for a joint distribution of a given semi-Markovian causal model.
}
\usage{verma.constraints(G, to)}
\arguments{
  \item{G}{An \code{igraph} object describing the directed acyclic graph induced by the causal model that matches the internal syntax. }
}

\value{A list of named vectors, each with two components named \code{X} and \code{Y} corresponding to a functional constraint where \code{X} is independent of \code{Y}.}

\references{

Tian, J., Pearl J. 2002 On Testable Implications of Causal Models with Hidden variables.
\emph{Proceedings of the Eighteenth Conference on Uncertainty in Artificial Intelligence}, 519--527.

}

\author{Santtu Tikka}