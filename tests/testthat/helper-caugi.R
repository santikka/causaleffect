causal.fixtures <- function() {
  graph <- function(
    from,
    to,
    bidirected = character(0),
    nodes = unique(c(from, to, bidirected))
  ) {
    u <- matrix(bidirected, ncol = 2, byrow = TRUE)
    directed <- data.frame(
      from = from,
      to = to,
      description = rep(NA_character_, length(from))
    )
    confounding <- data.frame(
      from = c(u[, 1], u[, 2]),
      to = c(u[, 2], u[, 1]),
      description = rep("U", length(bidirected))
    )
    list(
      igraph = igraph::graph_from_data_frame(
        rbind(directed, confounding),
        directed = TRUE,
        vertices = nodes
      ),
      caugi = function() {
        caugi::caugi(
          from = c(from, u[, 1]),
          edge = c(rep("-->", length(from)), rep("<->", nrow(u))),
          to = c(to, u[, 2]),
          nodes = nodes,
          class = "ADMG",
          simple = FALSE
        )
      }
    )
  }
  list(
    backdoor = list(
      graph = graph(
        c("z", "z", "x"),
        c("x", "y", "y"),
        nodes = c("x", "y", "z")
      ),
      y = "y",
      x = "x"
    ),
    frontdoor = list(
      graph = graph(c("x", "m"), c("m", "y"), c("x", "y")),
      y = "y",
      x = "x"
    ),
    nonidentifiable = list(
      graph = graph("x", "y", c("x", "y")),
      y = "y",
      x = "x"
    ),
    conditional = list(
      graph = graph(c("x", "z", "w", "w"), c("z", "y", "x", "y")),
      y = "y",
      x = "x",
      z = "z"
    ),
    conditional_confounding = list(
      graph = graph(c("x", "z", "w"), c("z", "y", "y"), c("x", "y")),
      y = "y",
      x = "x",
      z = "w"
    ),
    simplification = list(
      graph = graph(c("x", "w", "w", "z"), c("y", "x", "z", "y")),
      y = "y",
      x = "x"
    ),
    disconnected = list(
      graph = graph(
        c("x", "q"),
        c("y", "r"),
        nodes = c("q", "x", "r", "y", "isolated")
      ),
      y = "y",
      x = "x"
    ),
    edgeless = list(
      graph = graph(character(0), character(0), nodes = c("x", "y")),
      y = "y",
      x = "x"
    ),
    observation = list(
      graph = graph(c("x", "z"), c("y", "y")),
      y = "y",
      x = character(0)
    ),
    pruning = list(
      graph = graph(
        c("x", "z_4", "z_1", "z_2", "z_3", "z_3", "z_5", "z_5"),
        c("z_4", "y", "x", "z_1", "z_2", "x", "z_1", "z_4"),
        c("x", "z_2", "z_3", "z_2", "z_2", "y", "z_4", "y", "z_5", "z_4")
      ),
      y = "y",
      x = "x"
    )
  )
}

canonical.steps <- function(tree) {
  G <- tree$call$G
  e <- as.data.frame(igraph::ends(G, igraph::E(G)))
  names(e) <- c("from", "to")
  e$unobserved <- rep(FALSE, nrow(e))
  if ("description" %in% igraph::edge_attr_names(G)) {
    e$unobserved <- igraph::edge_attr(G, "description") %in% "U"
  }
  e <- unique(e[order(e$from, e$to, e$unobserved), , drop = FALSE])
  rownames(e) <- NULL
  tree$call$G <- list(nodes = igraph::V(G)$name, edges = e)
  if (length(tree$branch) > 0L) {
    tree$branch <- lapply(tree$branch, canonical.steps)
  }
  tree
}

legacy.fixtures <- function() {
  selection.graph <- function(from, to, unobserved) {
    edges <- data.frame(from = from, to = to, description = NA_character_)
    edges$description[unobserved] <- "U"
    G <- igraph::graph_from_data_frame(edges, directed = TRUE)
    igraph::set_vertex_attr(G, "description", index = "S", value = "S")
  }
  list(
    transport = list(
      y = "Y",
      x = "X",
      D = selection.graph(
        c("X", "Z", "W", "V", "S", "X", "Z", "V", "Y", "X", "Y"),
        c("Z", "W", "V", "Y", "V", "Z", "X", "Y", "V", "Y", "X"),
        6:11
      )
    ),
    recover = list(
      y = "Y",
      x = "X",
      G = selection.graph(
        c("W_1", "W_2", "X", "W_2", "W_1", "W_2", "W_1", "Y"),
        c("X", "X", "Y", "S", "W_2", "W_1", "Y", "W_1"),
        5:8
      )
    )
  )
}

causal.fixture.results <- function(effect) {
  options <- expand.grid(
    expr = c(TRUE, FALSE),
    simp = c(FALSE, TRUE),
    prune = c(FALSE, TRUE),
    primes = c(FALSE, TRUE)
  )
  lapply(
    causal.fixtures(),
    function(case) {
      lapply(seq_len(nrow(options)), function(i) {
        args <- c(
          list(
            y = case$y,
            x = case$x,
            z = case$z,
            G = case$graph$igraph,
            steps = TRUE,
            stop_on_nonid = FALSE
          ),
          as.list(options[i, ])
        )
        result <- do.call(effect, args)
        result$steps <- canonical.steps(result$steps)
        result
      })
    }
  )
}
