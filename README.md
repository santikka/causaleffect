# causaleffect: an R package for causal effect effect identification

Functions for identification and transportation of causal effects. 
Provides a conditional causal effect identification algorithm (IDC) by Shpitser, I. and Pearl, J. (2006) <http://ftp.cs.ucla.edu/pub/stat_ser/r329-uai.pdf>, 
an algorithm for transportability from multiple domains with limited experiments by Bareinboim, E. and Pearl, J. (2014) <http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf> 
and a selection bias recovery algorithm by Bareinboim, E. and Tian, J. (2015) <http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>. 
All of the previously mentioned algorithms are based on a causal effect identification algorithm by Tian , J. (2002) <http://ftp.cs.ucla.edu/pub/stat_ser/r309.pdf>.

For details, see the [package vignettes at CRAN](https://cran.r-project.org/package=causaleffect) and the paper [Identifying Causal Effects with the R Package
causaleffect](https://www.jstatsoft.org/article/view/v076i12)

## Installation
You can install the latest release version from CRAN:
```R
install.packages("causaleffect")
```

Alternatively, you can install the latest development version by using the devtools package:
```R
install.packages("devtools")
devtools::install_github("santikka/causaleffect")
```

## Recent changes (for all changes, see NEWS file).

### Changes from version 1.3.14 to 1.3.15
 * Replaced deprecated igraph edge indexing to avoid future warnings.

### Changes from version 1.3.13 to 1.3.14
 * Fixed a rare issue when using pruning.

### Changes from version 1.3.12 to 1.3.13
 * Fixed an incorrect graph definition in the IDC algorithm.

### Changes from version 1.3.11 to 1.3.12
 * The package no longer depends on the 'ggm' package.
 * The package no longer requires the 'XML' package, now suggests instead.

### Changes from version 1.3.10 to 1.3.11
 * Fixed inconsistency with function arguments when computing causal effects with surrogate experiments using 'aux.effect'.
 * Fixed a rare issue with simplification.