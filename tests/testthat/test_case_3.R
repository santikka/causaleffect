library(testthat)
library(igraph)
library(causaleffect)

causal_effect_files <- list.files("~/Projects/causaleffect/R", pattern = "\\.R$", full.names = TRUE)
lapply(causal_effect_files, source)

#-------------------------------------------------------------------
# test case #3 from pp. 6-7 of causaleffect on CRAN - only observed variables
#-------------------------------------------------------------------
# unit tests for functions:
# (1) topo,
# (2) causal.effect with simp = FALSE,
# (3) parse.expression from causal.effect simp = FALSE,
# (4) simplify from causal.effect simp = FALSE,
# (5) causal.effect with simp = TRUE,
# (6) parse.expression from causal.effect simp = TRUE,
# (7) simplify from causal.effect simp = TRUE

#-------------------------------------------------------------------
# defining graphs, nodes, and topological ordering using igraph package

G_3 <- graph.formula(x -+ y, w -+ x, w -+ z, z -+ y)
G_3.obs <- observed.graph(G_3)
G_3.unobs <- unobserved.graph(G_3)
topo_3 <- igraph::topological.sort(G_3.obs)
topo_3 <- igraph::get.vertex.attribute(G_3, "name")[topo_3]

plot(G_3)
plot(G_3.obs)
plot(G_3.unobs)

#-------------------------------------------------------------------
# (1) testing that topo works with test case #3.
  # currently PASSES

test_that("topo works on simple observed graph G_3", {
  expect_equal(topo_3, c("w", "x", "z", "y"))
})

#-------------------------------------------------------------------
# (2) testing that causal.effect works with test case #3 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on simple observed graph G_3", {
  expect_equal(causal.effect("y", "x", G = G_3, simp = FALSE),
               "\\sum_{w,z}P(y|w,x,z)P(z|w)P(w)")

})

#-------------------------------------------------------------------
# (3) testing that parse.expression works with test case #3
  # causal.effect simp = FALSE
  # currently PASSES

# define P_3_pe1 for parse.expression() using the output from causal.effect with
  # expr = FALSE and simp = FALSE
  # P needs to be a probability object.
  # the initial probabilistic expression should be: ∑w,z P(y∣w,x,z)P(z∣w)P(w).
  # the simplified expression should look like: ∑w P(y∣w,x)P(w)
P_3_pe1 <- probability(
  sumset = c("w", "z"),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    probability(var = "y", cond = c("w", "x", "z")),
    probability(var = "z", cond = c("w")),
    probability(var = "w", cond = character(0))
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)

# must define expected output object to match output from parse.expression: ∑w P(y|w,x)P(w)
expected_output_3_pe1 <- probability(
  sumset = "w",
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    probability(var = "y", cond = c("w", "x")),
    probability(var = "w", cond = character(0))
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)

# now running testthat
test_that("parse.expression works on simple observed graph G_3", {
  expect_equal(parse.expression(P_3_pe1, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_3_pe1)

})

#-------------------------------------------------------------------
# (4) testing that simplify works with test case #3
  # causal.effect with simp = FALSE
  # currently PASSES

# define P_3_s1 for simplify() using the output of parse.expression.
  # P needs to be a list object.
  # the simplified expression should look like: ∑w P(y∣w,x)P(w)
P_3_s1 <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    structure(
      list(
        var = "y",
        cond = c("w", "x"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    ),
    structure(
      list(
        var = "w",
        cond = character(0),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
attr(P_3_s1, "class") <- "probability"

# must define expected output object to match output from simplify: ∑w P(y|w,x)P(w)
expected_output_3_s1 <- probability(
  sumset = "w",
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    probability(var = "y", cond = c("w", "x")),
    probability(var = "w", cond = character(0))
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
attr(expected_output_3_s1, "class") <- "probability"

#now running testthat
test_that("simplify works on simple observed graph G_3", {
  expect_equal(simplify(P_3_s1, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_3_s1)
})

#-------------------------------------------------------------------
# (5) testing that causal.effect works with test case #3 when simp = TRUE
  # expression should be simplified.
  # currently PASSES

test_that("causal.effect works on simple observed graph G_3", {
  expect_equal(causal.effect("y", "x", G = G_3, simp = TRUE),
               "\\sum_{w}P(y|w,x)P(w)")
})

#-------------------------------------------------------------------
# (6) testing that parse.expression works with test case #3
  # causal.effect simp = TRUE
  # currently PASSES

# define P_3_pe2 for parse.expression() using the output from causal.effect with
  # expr = FALSE and simp = TRUE
  # P needs to be a probability object.
  # the initial probabilistic expression should be: ∑w P(y|w,x)P(w)
  # the simplified expression should look like: P(y∣w,x)P(w)
P_3_pe2 <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    structure(
      list(
        var = "y",
        cond = c("w", "x"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    ),
    structure(
      list(
        var = "w",
        cond = character(0),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)

# must define expected output object to match output from parse.expression: P(y∣w,x)P(w)
expected_output_3_pe2 <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    structure(
      list(
        var = "y",
        cond = c("w", "x"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    ),
    structure(
      list(
        var = "w",
        cond = character(0),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)

# now running testthat
test_that("parse.expression works on simple observed graph G_3", {
  expect_equal(parse.expression(P_3_pe2, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_3_pe2)

})

#-------------------------------------------------------------------
# (7) testing that simplify works with test case #3
  # causal.effect with simp = TRUE
  # currently PASSES

# define P_3_s2 for simplify() using the output of parse.expression.
  # P needs to be a list object.
  # the simplified expression should look like: P(y∣w,x)P(w)
P_3_s2 <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    structure(
      list(
        var = "y",
        cond = c("w", "x"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    ),
    structure(
      list(
        var = "w",
        cond = character(0),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
attr(P_3_s2, "class") <- "probability"

# must define expected output object to match output from simplify: P(y|w,x)P(w)
expected_output_3_s2 <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    structure(
      list(
        var = "y",
        cond = c("w", "x"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    ),
    structure(
      list(
        var = "w",
        cond = character(0),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0
      ),
      class = "probability"
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
attr(expected_output_3_s2, "class") <- "probability"

# now running testthat
test_that("simplify works on simple observed graph G_3", {
  expect_equal(simplify(P_3_s2, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_3_s2)
})


