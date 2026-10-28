test_that(
  "core identification preserves baseline results for both graph interfaces",
  {
    expected <- readRDS(test_path("fixtures", "causal-effect-baseline.rds"))
    options <- expand.grid(
      expr = c(TRUE, FALSE),
      simp = c(FALSE, TRUE),
      prune = c(FALSE, TRUE),
      primes = c(FALSE, TRUE)
    )
    fixtures <- causal.fixtures()
    for (name in names(fixtures)) {
      case <- fixtures[[name]]
      for (i in seq_len(nrow(options))) {
        for (G in list(case$graph$igraph, case$graph$caugi())) {
          args <- c(
            list(
              y = case$y,
              x = case$x,
              z = case$z,
              G = G,
              steps = TRUE,
              stop_on_nonid = FALSE
            ),
            as.list(options[i, ])
          )
          actual <- do.call(causal.effect, args)
          expect_s3_class(actual$steps$call$G, "igraph")
          actual$steps <- canonical.steps(actual$steps)
          expect_equal(actual, expected[[name]][[i]], info = paste(name, i))
        }
      }
    }
  }
)

test_that(
  "native DAG inputs and nonidentifiability keep the public return contract",
  {
    G <- caugi::caugi(from = "x", edge = "-->", to = "y", class = "DAG")
    expect_identical(causal.effect("y", "x", G = G), "P(y|x)")
    expect_s3_class(causal.effect("y", "x", G = G, expr = FALSE), "probability")
    G <- causal.fixtures()$nonidentifiable$graph$caugi()
    expect_error(
      causal.effect("y", "x", G = G),
      "Not identifiable.",
      fixed = TRUE
    )
    expect_identical(causal.effect("y", "x", G = G, stop_on_nonid = FALSE), "")
    expect_null(causal.effect(
      "y",
      "x",
      G = G,
      expr = FALSE,
      stop_on_nonid = FALSE
    ))
  }
)

test_that("graph conversion validates the legacy confounding convention", {
  G <- igraph::make_graph(c("x", "y"))
  expect_equal(causal.effect("y", "x", G = G), "P(y|x)")
  G <- igraph::set_edge_attr(G, "color", value = "blue")
  expect_equal(causal.effect("y", "x", G = G), "P(y|x)")
  expect_error(
    prepare.causal.graph(igraph::set_edge_attr(G, "description", value = "U")),
    "reciprocal"
  )
  expect_error(
    prepare.causal.graph(igraph::make_graph(c("x", "y", "y", "x"))),
    "not a DAG"
  )
  expect_error(
    prepare.causal.graph(igraph::make_graph(c("x", "x"))),
    "self-loops"
  )
  expect_error(
    prepare.causal.graph(igraph::set_vertex_attr(
      G,
      "name",
      value = c("x", "x")
    )),
    "unique"
  )
  expect_error(
    prepare.causal.graph(igraph::make_ring(3, directed = TRUE)),
    "vertex names"
  )
  expect_error(
    prepare.causal.graph(igraph::make_graph(c("x", "y"), directed = FALSE)),
    "must be directed"
  )
  expect_error(prepare.causal.graph(list()), "must be an igraph")
  undirected <- caugi::caugi(from = "x", edge = "---", to = "y", class = "PDAG")
  expect_error(prepare.causal.graph(undirected), "DAG or ADMG")
  expect_error(causal.effect("missing", "x", G = G), "Set 'y'")
  expect_error(causal.effect("y", "missing", G = G), "Set 'x'")
  expect_error(causal.effect("y", "x", "missing", G = G), "Set 'z'")
  expect_error(causal.effect("y", "y", G = G), "not disjoint")
})

test_that("igraph vertex names accept numeric and factor values", {
  G <- igraph::make_graph(c(1, 2))
  for (names in list(1:2, c(1, 2), factor(c("1", "2"), levels = c("2", "1")))) {
    graph <- igraph::set_vertex_attr(G, "name", value = names)
    expect_identical(causal.effect("2", "1", G = graph), "P(2|1)")
    expect_identical(igraph::V(graph)$name, names)
  }
})

test_that("graph operations preserve inclusive queries and ordering", {
  G <- caugi::caugi(
    from = c("a", "b", "a"),
    edge = c("-->", "-->", "<->"),
    to = c("c", "c", "d"),
    nodes = c("d", "b", "a", "c", "isolated"),
    class = "ADMG",
    simple = FALSE
  )
  topo <- c("d", "b", "a", "c", "isolated")
  expect_identical(ancestors(c("a", "c"), G, topo), c("b", "a", "c"))
  expect_identical(descendants("a", G, topo), c("a", "c"))
  expect_identical(parents("c", G, topo), c("b", "a", "c"))
  expect_identical(children("a", G, topo), c("a", "c"))
  expect_identical(ancestors(character(0), G, topo), character(0))
  expect_identical(parents(character(0), G, topo), character(0))
  expect_identical(connected(character(0), G, topo), character(0))
  expect_identical(
    caugi::nodes(causal.subgraph(G, c("a", "d")))$name,
    c("d", "a")
  )
  expect_equal(c_components(G, topo), c_components(legacy.graph(G), topo))
  expect_identical(connected("d", G, topo), c("d", "b", "a", "c"))
  old <- caugi::caugi_options("use_open_graph_definition")
  on.exit(caugi::caugi_options(use_open_graph_definition = old), add = TRUE)
  caugi::caugi_options(use_open_graph_definition = !old)
  expect_identical(ancestors("a", G, topo), "a")
  expect_identical(descendants("a", G, topo), c("a", "c"))
})

test_that(
  "intervention and separation distinguish directed and bidirected edges",
  {
    G <- caugi::caugi(
      from = c("x", "x", "z"),
      edge = c("-->", "<->", "-->"),
      to = c("y", "y", "x"),
      class = "ADMG",
      simple = FALSE
    )
    before <- caugi::edges(G)
    incoming <- intervene.graph(G, incoming = "x")
    outgoing <- intervene.graph(G, outgoing = "x")
    expect_equal(nrow(caugi::edges(incoming)), 1L)
    expect_equal(caugi::edges(incoming)$edge, "-->")
    expect_equal(sort(caugi::edges(outgoing)$edge), sort(c("<->", "-->")))
    expect_false(wrap.dSep(outgoing, "x", "y", character(0)))
    expect_true(wrap.dSep(G, character(0), "y", character(0)))
    expect_false(wrap.dSep(G, "x", "x", character(0)))
    expect_equal(caugi::edges(G), before)
  }
)

test_that(
  "latent projection preserves bows and avoids latent name collisions",
  {
    G <- caugi::caugi(
      from = c("x", "l", "l"),
      edge = c("-->", "-->", "<->"),
      to = c("l", "y", "x"),
      nodes = c("x", "l", "y", ".latent"),
      class = "ADMG",
      simple = FALSE
    )
    projected <- latent.projection(G, "l")
    expect_equal(caugi::nodes(projected)$name, c("x", "y", ".latent"))
    expect_equal(sort(caugi::edges(projected)$edge), sort(c("-->", "<->")))
    expect_true(compare.graphs(G, latent.projection(G, character(0))))
  }
)

test_that(
  "separation respects collider paths through multiple bidirected edges",
  {
    G <- caugi::caugi(
      from = c("a", "b", "c"),
      edge = c("-->", "<->", "<->"),
      to = c("b", "c", "d"),
      class = "ADMG"
    )
    expect_false(wrap.dSep(G, "a", "d", c("b", "c")))
    expect_true(wrap.dSep(G, "a", "d", character(0)))
    expect_true(wrap.dSep(G, "a", "d", "b"))
    expect_true(wrap.dSep(G, "a", "d", "c"))
    expect_false(wrap.dSep(G, "d", "a", c("b", "c")))
    expect_false(wrap.dSep(unobserved.graph(G), "a", "d", c("b", "c")))
  }
)

test_that("step graphs preserve source attributes without changing the input", {
  G <- causal.fixtures()$frontdoor$graph$igraph
  G <- igraph::set_graph_attr(G, "title", "frontdoor")
  G <- igraph::set_vertex_attr(G, "color", value = c("red", "green", "blue"))
  G <- igraph::set_edge_attr(G, "weight", value = seq_len(igraph::ecount(G)))
  before <- G
  result <- causal.effect("y", "x", G = G, steps = TRUE)
  visit <- function(tree) {
    graph <- tree$call$G
    expect_s3_class(graph, "igraph")
    expect_equal(igraph::graph_attr(graph, "title"), "frontdoor")
    index <- match(igraph::V(graph)$name, igraph::V(G)$name)
    expect_equal(igraph::V(graph)$color, igraph::V(G)$color[index])
    expect_false(anyNA(igraph::E(graph)$weight))
    invisible(lapply(tree$branch, visit))
  }
  visit(result$steps)
  expect_identical(G, before)
  native <- causal.fixtures()$frontdoor$graph$caugi()
  before <- caugi::edges(native)
  causal.effect("y", "x", G = native, prune = TRUE, simp = TRUE)
  expect_equal(caugi::edges(native), before)
})

test_that("projected step graphs preserve confounding with factor descriptions", {
  G <- igraph::graph_from_data_frame(
    data.frame(
      from = c("x", "m", "z", "z"),
      to = c("m", "y", "m", "y"),
      description = factor(rep("O", 4))
    ),
    directed = TRUE
  )
  expect_no_warning(
    result <- causal.effect(c("m", "y"), "x", G = G, prune = TRUE, steps = TRUE)
  )
  projected <- result$steps$branch[[1]]$call$G
  edges <- igraph::as_data_frame(projected, what = "edges")
  confounding <- edges[edges$description %in% "U", c("from", "to")]
  expect_setequal(
    paste(confounding$from, confounding$to),
    c("m y", "y m")
  )
  expect_false(anyNA(edges$description))
  expect_identical(igraph::E(G)$description, factor(rep("O", 4)))
})

test_that("transport and recovery retain their igraph behavior", {
  expected <- readRDS(test_path("fixtures", "legacy-results.rds"))
  cases <- legacy.fixtures()
  for (name in names(cases)) {
    actual <- do.call(getExportedValue("causaleffect", name), cases[[name]])
    expect_identical(actual, expected[[name]], info = name)
  }
})
