# Run from the repository root with an installed package library, output file,
# and input type (igraph or caugi). Package installation is outside the timing.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || !args[3] %in% c("igraph", "caugi")) {
  stop(
    "Usage: Rscript tools/benchmark-caugi.R LIBRARY OUTPUT.csv igraph|caugi [ITERATIONS]"
  )
}
library(causaleffect, lib.loc = args[1])
if (!requireNamespace("bench", quietly = TRUE)) {
  stop("Install bench to run these benchmarks.")
}
source("tests/testthat/helper-caugi.R")
iterations <- if (length(args) >= 4L) as.integer(args[4]) else 20L
cases <- causal.fixtures()[
  c("backdoor", "frontdoor", "conditional", "pruning", "simplification")
]
cases$pruning$prune <- TRUE
cases$simplification$simp <- TRUE
for (n in c(10L, 25L, 50L, 100L)) {
  for (mixed in c(FALSE, TRUE)) {
    v <- paste0("v", seq_len(n))
    from <- head(v, -1L)
    to <- tail(v, -1L)
    df <- data.frame(from = from, to = to, description = NA_character_)
    if (mixed) {
      df <- rbind(
        df,
        data.frame(from = c(v[1], v[n]), to = c(v[n], v[1]), description = "U")
      )
    }
    graph <- list(
      igraph = igraph::graph_from_data_frame(df, directed = TRUE, vertices = v)
    )
    graph$caugi <- local({
      f <- from
      t <- to
      names <- v
      bi <- mixed
      function() {
        caugi::caugi(
          from = c(f, if (bi) names[1]),
          edge = c(rep("-->", length(f)), if (bi) "<->"),
          to = c(t, if (bi) tail(names, 1)),
          nodes = names,
          class = "ADMG",
          simple = FALSE
        )
      }
    })
    cases[[paste(if (mixed) "admg" else "dag", n, sep = "_")]] <- list(
      graph = graph,
      y = v[n],
      x = v[1]
    )
  }
}
results <- lapply(names(cases), function(name) {
  case <- cases[[name]]
  G <- if (args[3] == "igraph") case$graph$igraph else case$graph$caugi()
  call <- list(
    y = case$y,
    x = case$x,
    z = case$z,
    G = G,
    stop_on_nonid = FALSE,
    simp = isTRUE(case$simp),
    prune = isTRUE(case$prune)
  )
  run <- function() do.call(causal.effect, call)
  run()
  timing <- bench::mark(
    run(),
    iterations = iterations,
    check = FALSE,
    filter_gc = FALSE
  )
  row <- data.frame(
    case = name,
    input = args[3],
    nodes = igraph::vcount(case$graph$igraph),
    edges = igraph::ecount(case$graph$igraph),
    median_ms = as.numeric(timing$median) * 1000,
    r_alloc_bytes = as.numeric(timing$mem_alloc),
    iterations = timing$n_itr,
    causaleffect = as.character(packageVersion("causaleffect")),
    igraph = as.character(packageVersion("igraph")),
    caugi = if (requireNamespace("caugi", quietly = TRUE)) {
      as.character(packageVersion("caugi"))
    } else {
      NA_character_
    },
    R = as.character(getRversion())
  )
  print(
    row[c("case", "input", "median_ms", "r_alloc_bytes")],
    row.names = FALSE
  )
  row
})
write.csv(do.call(rbind, results), args[2], row.names = FALSE)
