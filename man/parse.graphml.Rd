\name{parse.graphml}
\alias{parse.graphml}
\title{Prepare GraphML files for internal use}
\description{
This function reads GraphML files created by a graphical editor, which describe directed acyclic graphs. The R package XML is utilized to parse the contents of the files to suit the internal format used by causal inference functions. Bidirected arcs are replaced by two unobserved directed arcs, and the resulting XML file is coerced into an igraph object. This function also serves as a wrapper for files that already correspond to the internal format. Names for the nodes of the graph can be supplied or read directly from the input file.
}
\usage{parse.graphml(file, format = c("standard", "internal"), 
  nodes = c(), use.names = TRUE)}
\arguments{
  \item{file}{The connection to read from. }
  \item{format}{
A character constant describing how bidirected arcs are denoted in the GraphML file.
Option \code{standard} corresponds to bidirected arcs that are notated with a graphical parameter describing an arrow at each end of the arc or no arrows at all.
Option \code{internal} matches the format that \code{standard} graphs are coerced into. This option should be used only if all bidirected arcs in the graph are denoted by two directed arcs which have a description parameter of a single character "U" (shorthand for "unobserved"). Selection variables should have a description parameter of a single character "S". 
}
  \item{nodes}{A character vector that describes the names of the nodes in the graph. This is ignored if \code{use.names} is \code{TRUE}. }
  \item{use.names}{A logical value indicating whether the names of the nodes should be read from the file or not. }
}

\value{An object of class igraph that describes the causal diagram. The parsed graph can now be used by other functions of the package.}

\author{Santtu Tikka}