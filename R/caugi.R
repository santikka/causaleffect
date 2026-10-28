prepare.causal.graph <- function(G) {
  original <- NULL
  if (igraph::is_igraph(G)) {
    original <- G
    v <- igraph::V(G)$name
    if (igraph::vcount(G) == 0L) {
      v <- character(0)
    }
    if (is.null(v) || anyNA(v) || any(v == "") || anyDuplicated(v)) {
      stop("Graph 'G' must have unique, nonempty vertex names.")
    }
    v <- as.character(v)
    if (!igraph::is_directed(G)) {
      stop("Graph 'G' must be directed.")
    }
    ends <- igraph::ends(G, igraph::E(G), names = FALSE)
    if (any(ends[, 1] == ends[, 2])) {
      stop("Graph 'G' cannot contain self-loops.")
    }
    unobserved <- igraph::edge_attr(G, "description") %in% "U"
    if (length(unobserved) == 0L) {
      unobserved <- rep(FALSE, nrow(ends))
    }
    u <- ends[unobserved, , drop = FALSE]
    if (!all(paste(u[, 1], u[, 2]) %in% paste(u[, 2], u[, 1]))) {
      stop(
        "Bidirected edges in 'G' must be reciprocal edges with description 'U'."
      )
    }
    directed <- observed.graph(G)
    if (!igraph::is_dag(directed)) {
      stop("Graph 'G' is not a DAG")
    }
    topo <- v[igraph::topo_sort(directed)]
    keep <- !unobserved | ends[, 1] < ends[, 2]
    e <- unique(data.frame(
      from = v[ends[keep, 1]],
      edge = as.character(ifelse(unobserved[keep], "<->", "-->")),
      to = v[ends[keep, 2]]
    ))
    G <- caugi::caugi(
      from = e$from,
      edge = e$edge,
      to = e$to,
      nodes = v,
      class = "ADMG",
      simple = FALSE
    )
  } else if (caugi::is_caugi(G)) {
    if (!caugi::is_admg(G, force_check = TRUE)) {
      stop("Graph 'G' must be a caugi DAG or ADMG.")
    }
    G <- caugi::mutate_caugi(G, "ADMG")
    v <- caugi::nodes(G)$name
    if (anyNA(v) || any(v == "") || anyDuplicated(v)) {
      stop("Graph 'G' must have unique, nonempty vertex names.")
    }
    e <- as.data.frame(caugi::edges(G))
    directed <- igraph::graph_from_data_frame(
      e[e$edge == "-->", c("from", "to"), drop = FALSE],
      directed = TRUE,
      vertices = v
    )
    # Preserve the factor order used by the igraph interface.
    topo <- v[igraph::topo_sort(directed)]
  } else {
    stop("Graph 'G' must be an igraph object or a caugi DAG or ADMG.")
  }
  list(graph = G, topo = topo, original = original)
}

causal.subgraph <- function(G, nodes) {
  v <- caugi::nodes(G)$name
  # igraph retains vertex order regardless of the order of the requested subset.
  caugi::subgraph(G, v[v %in% nodes])
}

intervene.graph <- function(
  G,
  incoming = character(0),
  outgoing = character(0)
) {
  e <- as.data.frame(caugi::edges(G))
  remove <- (e$edge == "-->" & (e$to %in% incoming | e$from %in% outgoing)) |
    (e$edge == "<->" & (e$from %in% incoming | e$to %in% incoming))
  if (!any(remove)) {
    return(G)
  }
  caugi::remove_edges(
    G,
    from = e$from[remove],
    edge = e$edge[remove],
    to = e$to[remove]
  )
}

caugi.neighborhood <- function(G, node, query) {
  if (length(node) == 0L) {
    return(character(0))
  }
  result <- switch(
    query,
    ancestors = caugi::ancestors(G, node, open = FALSE),
    descendants = caugi::descendants(G, node, open = FALSE),
    parents = caugi::parents(G, node),
    children = caugi::children(G, node)
  )
  # The legacy neighborhood queries include the starting vertices.
  unique(c(node, unlist(result, use.names = FALSE)))
}

expand.caugi <- function(G) {
  v <- caugi::nodes(G)$name
  e <- as.data.frame(caugi::edges(G))
  bidirected <- e[e$edge == "<->", , drop = FALSE]
  directed <- e[e$edge == "-->", , drop = FALSE]
  latent.parents <- utils::tail(
    make.unique(c(v, rep(".latent", nrow(bidirected)))),
    nrow(bidirected)
  )
  dag <- caugi::caugi(
    from = c(directed$from, latent.parents, latent.parents),
    edge = rep("-->", nrow(directed) + 2L * nrow(bidirected)),
    to = c(directed$to, bidirected$from, bidirected$to),
    nodes = c(v, latent.parents),
    class = "DAG"
  )
  list(graph = dag, latent = latent.parents)
}

caugi.projection <- function(G, latent) {
  if (length(latent) == 0L) {
    return(G)
  }
  # caugi requires a DAG for latent projection.
  expanded <- expand.caugi(G)
  caugi::latent_project(expanded$graph, c(latent, expanded$latent))
}

legacy.graph <- function(G, original = NULL) {
  v <- caugi::nodes(G)$name
  e <- as.data.frame(caugi::edges(G))
  bidirected <- e$edge == "<->"
  edges <- data.frame(
    from = c(e$from, e$to[bidirected]),
    to = c(e$to, e$from[bidirected]),
    description = c(
      ifelse(bidirected, "U", NA_character_),
      rep("U", sum(bidirected))
    )
  )
  out <- igraph::graph_from_data_frame(edges, directed = TRUE, vertices = v)
  if (is.null(original)) {
    return(out)
  }
  for (name in igraph::graph_attr_names(original)) {
    out <- igraph::set_graph_attr(out, name, igraph::graph_attr(original, name))
  }
  vertex.index <- match(v, igraph::V(original)$name)
  for (name in setdiff(igraph::vertex_attr_names(original), "name")) {
    out <- igraph::set_vertex_attr(
      out,
      name,
      value = igraph::vertex_attr(original, name)[vertex.index]
    )
  }
  old.ends <- igraph::ends(original, igraph::E(original), names = FALSE)
  old.u <- igraph::edge_attr(original, "description") %in% "U"
  if (length(old.u) == 0L) {
    old.u <- rep(FALSE, nrow(old.ends))
  }
  old.key <- paste(old.ends[, 1], old.ends[, 2], old.u)
  new.key <- paste(
    match(edges$from, igraph::V(original)$name),
    match(edges$to, igraph::V(original)$name),
    edges$description %in% "U"
  )
  edge.index <- match(new.key, old.key)
  for (name in igraph::edge_attr_names(original)) {
    value <- igraph::edge_attr(original, name)[edge.index]
    if (name == "description") {
      value <- as.character(value)
      value[is.na(edge.index)] <- edges$description[is.na(edge.index)]
    }
    out <- igraph::set_edge_attr(out, name, value = value)
  }
  out
}

legacy.steps <- function(tree, original) {
  tree$call$G <- legacy.graph(tree$call$G, original)
  if (length(tree$branch) > 0L) {
    tree$branch <- lapply(tree$branch, legacy.steps, original = original)
  }
  tree
}
