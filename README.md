# causaleffect: an R package for causal effect effect identification
====================================================================

Functions for identification and transportation of causal effects. 
Provides a conditional causal effect identification algorithm (IDC) by Shpitser, I. and Pearl, J. (2006) <http://ftp.cs.ucla.edu/pub/stat_ser/r329-uai.pdf>, 
an algorithm for transportability from multiple domains with limited experiments by Bareinboim, E. and Pearl, J. (2014) <http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf> 
and a selection bias recovery algorithm by Bareinboim, E. and Tian, J. (2015) <http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>. 
All of the previously mentioned algorithms are based on a causal effect identification algorithm by Tian , J. (2002) <http://ftp.cs.ucla.edu/pub/stat_ser/r309.pdf>.

For details, see the [package vignettes at CRAN](https://cran.r-project.org/package=causaleffect) and the paper [Identifying Causal Effects with the R Package
causaleffect](https://www.jstatsoft.org/article/view/v076i12)

You can install the latest release version from CRAN:
```R
install.packages("causaleffect")
```

Alternatively, you can install the latest development version by using the devtools package:
```R
install.packages("devtools")
devtools::install_github("santikka/causaleffect")
```

Recent changes (for all changes, see NEWS file).
====================================================================

Changes from version 1.3.10 to 1.3.11
=====================================
 * Fixed inconsistency with function arguments when computing causal effects with surrogate experiments using 'aux.effect'.
 * Fixed a rare issue with simplification.

Changes from version 1.3.9 to 1.3.10
=====================================
 * Fixed a bug with simplification.

Changes from version 1.3.8 to 1.3.9
=====================================
 * Added a new parameter 'stop_on_nonid' to all identifiability algorithms. Value 'TRUE' mimics old functionality, where an error is produced when a non-identifiable effect is discovered. Value 'FALSE' continues the recursion.
 * Fixed a bug in the recoverability algorithm.
 * Fixed a bug in the transportability algorithm.
 * Fixed a bug related to identification using surrogate outcomes.

Changes from version 1.3.7 to 1.3.8
=====================================
 * Added a new function 'surrogate.outcome' to compute causal effects using surrogate outcomes.

Changes from version 1.3.6 to 1.3.7
=====================================
 * Fixed an issue with simplification introduced in version 1.3.6.

Changes from version 1.3.5 to 1.3.6
=====================================
 * Fixed a bug regarding graphs where zero edges had the 'description' attribute.
 * Fixed a bug when attempting to simplify conditional causal effects.

Changes from version 1.3.4 to 1.3.5
=====================================
 * Added a new identification algorithm. This can be enabled via the new 'prune' argument in the 'causal.effect' function. Examples on usage can be found in the documentation for the 'causal.effect' function.

