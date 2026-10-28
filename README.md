# causaleffect: an R package for causal effect identification

Functions for identification and transportation of causal effects. 
Provides a conditional causal effect identification algorithm (IDC) by Shpitser, I. and Pearl, J. (2006) <http://ftp.cs.ucla.edu/pub/stat_ser/r329-uai.pdf>, 
an algorithm for transportability from multiple domains with limited experiments by Bareinboim, E. and Pearl, J. (2014) <http://ftp.cs.ucla.edu/pub/stat_ser/r443.pdf> 
and a selection bias recovery algorithm by Bareinboim, E. and Tian, J. (2015) <http://ftp.cs.ucla.edu/pub/stat_ser/r445.pdf>. 
All of the previously mentioned algorithms are based on a causal effect identification algorithm by Tian, J. (2002) <http://ftp.cs.ucla.edu/pub/stat_ser/r309.pdf>.

For details, see the [package vignettes at CRAN](https://cran.r-project.org/package=causaleffect) and the paper [Identifying Causal Effects with the R Package causaleffect](https://www.jstatsoft.org/article/view/v076i12)

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

The development version requires R 4.2 or later and caugi 1.4.0 or later.
Installing caugi from source requires Cargo, Rust 1.80 or later, and xz.
These tools are not needed when installing a precompiled caugi binary, or when
installing causaleffect with caugi already installed.

## Graph inputs

`causal.effect()` accepts igraph graphs and caugi DAGs and ADMGs. For example,
these graphs describe the same front-door model:

```R
g <- igraph::graph_from_literal(x -+ m, m -+ y, x -+ y, y -+ x,
                                simplify = FALSE)
g <- igraph::set_edge_attr(g, "description", index = 3:4, value = "U")

cg <- caugi::caugi(x %-->% m, m %-->% y, x %<->% y,
                   nodes = c("x", "m", "y"), class = "ADMG")

causaleffect::causal.effect("y", "x", G = g)
causaleffect::causal.effect("y", "x", G = cg)
```

The identification algorithms, pruning, and simplification use caugi internally.
Existing expression ordering is preserved, and `steps = TRUE` returns igraph
graphs with the usual `description = "U"` encoding for confounding.

All vertices are treated as observed variables. For a DAG with explicit latent
vertices, project them out with `caugi::latent_project()` before passing the graph
to `causal.effect()`. Other causaleffect functions continue to accept igraph inputs.
