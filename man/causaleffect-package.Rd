\name{causaleffect-package}
\alias{causaleffect-package}
\alias{causaleffect}
\docType{package}
\title{Deriving Expressions of Joint Interventional Distributions and Transport Formulas in Causal Models}

\description{
Do-calculus is concerned with estimating the interventional distribution of some action from the observed
joint probability distribution of the variables in a given causal structure.
All identifiable causal effects can be derived using the rules of do-calculus, but
the rules themselves do not give any direct indication whether the effect in question is identifiable or not.
Shpitser and Pearl (2006a) constructed an algorithm for identifying joint interventional distributions in causal models, which contain unobserved variables and induce directed acyclic graphs.
A highly similar algorithm was constructed earlier by Tian (2002).
The algorithm of Shpitser and Pearl (2006a) can be seen as a repeated application of the rules of do-calculus and known properties of probabilities,
and it ultimately either derives an expression for the causal distribution or fails to identify the effect, in which case
the effect is unidentifiable. Shpitser and Pearl (2006b) also presented a generalized algorithm for identification of conditional causal effects.
causaleffect provides an implementation of this algorithm.
In addition to ordinary identifiability, implementations of several other algorithms in causal inference are provided. These include algorithms for
z-identifiability, transportability, z-transportability and meta-transportability of causal effects by Bareinboim and Pearl (2012, 2013a, 2013b, 2013c).
Recently, many of these algorithms were combined under a single algorithm by Bareinboim and Pearl (2014), which is also provided along with
an implementation of an algorithm for recovering from selection bias by Bareinboim and Tian (2015).
}

\section{Graphs}{
Every causal model and selection diagram is depicted as an igraph graph with distinct attributes and special notation. Any bidirected edge corresponding to an unobserved variable must be denoted by using two unidirected edges with a \code{description} attribute of value \code{"U"}. Here is an example describing a simple causal model with only two vertices, X and Y, and a bidirected edge between them.

\preformatted{
> g <- graph.formula(X -+ Y, Y -+ X)
> g <- set.edge.attribute(graph = g, 
+ name = "description", index = 1:2, value = "U")
}

For selection diagrams, the vertices that correspond to selection variables must have a \code{description} attribute of value \code{"S"}. Here is an example of a simple selection diagram with a selection node S pointing to a non-selection variable Y. Because S precedes Y in the "-+" notation, S is given index 1 in the vertex sequence.

\preformatted{
> d <- graph.formula(S -+ Y)
> d <- set.vertex.attribute(graph = d, 
+ name = "description", index = 1, value = "S")
}

}

\author{Santtu Tikka <santtuth@gmail.com>}

\references{
Bareinboim E., Pearl J. 2012 Causal Inference by Surrogate Experiments: z-identifiability. \emph{Proceedings of the 28th Conference on Uncertainty in Artificial Intelligence}, 113--120.

Bareinboim E., Pearl J. 2013a A General Algorithm for Deciding Transportability of Experimental Results. \emph{Journal of Causal Inference}, \bold{1}, 107--134.

Bareinboim E., Pearl J. 2013b Meta-Transportability of Causal Effects: A Formal Approach. \emph{Proceedings of the 16th International Conference on Artificial Intelligence and Statistics}, 135--143.

Bareinboim E., Pearl J. 2013c Causal Transportability with Limited Experiments. \emph{Proceedings of the 27th AAAI Conference on Artificial Intelligence}, 95--101. 

Bareinboim E., Pearl J. 2014 Transportability from Multiple Environments with Limited Experiments: Completeness Results. \emph{Proceedings of the 27th Annual Conference on Neural Information Processing Systems}, 280--288.

Bareinboim E., Tian J. 2015 Recovering Causal Effects From Selection Bias. \emph{In Proceedings of the 29th AAAI Conference on Artificial Intelligence}, 3475--3481.

Pearl J. 2009 \emph{Causality: Models, Reasoning and Inference}, New York: Cambridge University Press. 

Shpitser I., Pearl J. 2006a Identification of Joint Interventional Distributions in Recursive semi-Markovian Causal Models. \emph{Proceedings of the 21st National Conference on Artificial Intelligence}, \bold{2}, 1219--1226. 

Shpitser I., Pearl J. 2006b Identification of Conditional Interventional Distributions. \emph{Proceedings of the 22nd Conference on Uncertainty in Artificial Intelligence}, 427--444.

Tian J. 2002 Studies in Causal Reasoning and Learning. PhD thesis, Department of Computer Science, University of California, Los Angeles.

Tian, J., Pearl J. 2002 On Testable Implications of Causal Models with Hidden variables. \emph{Proceedings of the Eighteenth Conference on Uncertainty in Artificial Intelligence}, 519--527.

Tikka, S., Karvanen J. 2017 Identifying Causal Effects with the R Package causaleffect. Journal of Statistical Software, \bold{76(12)}, 1--30.

Tikka, S., Karvanen J. 2017 Simplifying Probabilistic Expressions in Causal Inference. Journal of Machine Learning Research, \bold{18(36)}, 1--30.

Tikka, S., Karvanen J. 2018 Enhancing Identification of Causal Effects by Pruning. Journal of Machine Learning Research, \bold{18(194)}, 1--23.
}