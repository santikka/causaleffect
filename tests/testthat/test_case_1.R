library(testthat)
library(igraph)
library(causaleffect)

causal_effect_files <- list.files("~/Projects/causaleffect/R", pattern = "\\.R$", full.names = TRUE)
lapply(causal_effect_files, source)


#-------------------------------------------------------------------
# test case #1 from pp. 6-7 of causaleffect - includes unobserved confounders.
#-------------------------------------------------------------------
# unit tests for functions:
# (1) topo,
# (2) causal.effect with simp = TRUE,
# (3) causal.effect with simp = FALSE,
# (4) parse.expression from causal.effect,
# (5) simplify from causal.effect

# causal.effect with simp = TRUE and simp = FALSE yield the same expression, so
# there are only 5 unit tests compared to 7 unit tests for test cases #2 and #3

#-------------------------------------------------------------------
# defining graphs, nodes, and topological ordering using igraph package
G_1 <- graph.formula(x -+ y, z -+ x, z -+ y , x -+ z, z -+ x, simplify = FALSE)
G_1 <- set.edge.attribute(graph = G_1, name = "description", index = c(4,5), value = "U")
G_1.obs <- observed.graph(G_1)
G_1.unobs <- unobserved.graph(G_1)
topo_1 <- igraph::topological.sort(G_1.obs)
topo_1 <- igraph::get.vertex.attribute(G_1, "name")[topo_1]

print(topo_1)

plot(G_1)
# ^^ plotting this gives us a bidirected edge, which represents a latent confounder we can see in unobserved.graph
plot(observed.graph(G_1.obs))
plot(unobserved.graph(G_1.unobs))
# ^^ unobserved.graph plots observed graph, plus unobserved node(s)


#-------------------------------------------------------------------
# testing that topo works with test case #1
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_1", {
  expect_equal(topo_1, c("z", "x", "y"))
})


#-------------------------------------------------------------------
# testing that causal.effect works with test case #1 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_1", {
  expect_equal(causal.effect("y", "x", G = G_1, simp = FALSE),
               "\\sum_{z}P(y|z,x)P(z)")

})

#-------------------------------------------------------------------
# testing that causal.effect works with test case #1 when simp = TRUE
# expression should be the same, since it cannot be simplified.
# currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_1", {
  expect_equal(causal.effect("y", "x", G = G_1, simp = TRUE),
               "\\sum_{z}P(y|z,x)P(z)")
})

#-------------------------------------------------------------------
# testing that parse.expression works with test case #1
  # causal.effect with simp = TRUE and simp = FALSE (they are the same)
  # currently PASSES

# define P_1 for parse.expression(). P needs to be a probability object.
# the initial probabilistic expression should be: ∑z P(y|z,x)P(z)
# the simplified expression should look like: ∑z P(y|z,x)P(z)

# I used the output from causal.effect("y", "x", G = G_1, expr = FALSE, simp = TRUE).
# The expr = FALSE is key to NOT printing a string!
P_1 <- probability(
  sumset = c("z"),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    probability(var = "y", cond = c("z", "x")),
    probability(var = "z", cond = character(0))
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)


#now must define expected output from parse.expression
expected_output_1 <- probability(
  sumset = "z",
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    probability(var = "y", cond = c("z", "x")),
    probability(var = "z", cond = character(0))
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)


# now running testthat
test_that("parse.expression works on graph with unobserved confounders G_1", {
  expect_equal(parse.expression(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)

})

#-------------------------------------------------------------------
# testing that simplify works with test case #1
  # currently PASSES


# we can use the same P_1 and expected_output_1 as we used for parse.expression, as the expression
# passes through parse.expression unchanged.

test_that("simplify works on graph with unobserved confounders G_1", {
  expect_equal(simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)
})


