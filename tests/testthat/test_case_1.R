library(testthat)
library(igraph)
library(causaleffect)

causal_effect_files <- list.files("~/Projects/causaleffect/R", pattern = "\\.R$", full.names = TRUE)
lapply(causal_effect_files, source)

#-------------------------------------------------------------------
# test case #1 from pp. 6-7 of causaleffect on CRAN - includes unobserved confounders.
#-------------------------------------------------------------------
# unit tests for functions:
# (1) topo,
# (2) causal.effect with simp = FALSE,
# (3) causal.effect with simp = TRUE,
# (4) parse.expression (same for causal.effect simp = TRUE vs. FALSE; no need for duplicate unit tests),
# (5) simplify (same for causal.effect simp = TRUE vs. FALSE),
# (6) join (same for causal.effect simp = TRUE vs. FALSE)
# (7) insert (same for causal.effect simp = TRUE vs. FALSE)

# causal.effect with simp = TRUE and simp = FALSE yield the same expression, so
# there are only 7 unit tests compared to 9 unit tests for test cases #2 and #3

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
# (1) testing that topo works with test case #1
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_1", {
  expect_equal(topo_1, c("z", "x", "y"))
})

#-------------------------------------------------------------------
# (2) testing that causal.effect works with test case #1 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_1", {
  expect_equal(causal.effect("y", "x", G = G_1, simp = FALSE),
               "\\sum_{z}P(y|z,x)P(z)")

})

#-------------------------------------------------------------------
# (3) testing that causal.effect works with test case #1 when simp = TRUE
  # expression should be the same, since it cannot be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_1", {
  expect_equal(causal.effect("y", "x", G = G_1, simp = TRUE),
               "\\sum_{z}P(y|z,x)P(z)")
})

#-------------------------------------------------------------------
# (4) testing that parse.expression works with test case #1
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


# now must define expected output from parse.expression
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
# (5) testing that simplify works with test case #1
  # currently PASSES

# we can use the same P_1 and expected_output_1 as we used for parse.expression, as the expression
# passes through parse.expression unchanged.

test_that("simplify works on graph with unobserved confounders G_1", {
  expect_equal(simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)
})

#-------------------------------------------------------------------
# (6) testing that join works with test case #1
  # currently PASSES

# we can obtain the following from running simplify(P_1, topo_1, G_1.unobs, G_1,
# G_1.obs) with break points (the browser() function). I added print statements
# after step #5 in simplify():
  # Step 6 - Inside nested while loop before join operation
  # P$children[[k]]$var: y (this represents vari in simplify())
  # P$children[[k]]$cond: z x (this represents cond in simplify())
  # P$sumset[j]: z (this reprensents S in simplify())

J_1 <- character()
D_1 <- character()
vari_1 <- "y"
cond_1 <- c("z", "x")
S_1 <- "z"
M_1 <- "x"
O_1 <- c("z", "y")

# we can obtain the following from the graph information:
  # G.unobs = G_1.unobs
  # G = G_1
  # G.obs = G_1.obs
  # topo = topo_1

# we expect the output from this to be:
# [1] "y"
# [2] "z" "x"

join_output_1 <- list(
  c("y"),
  c("z", "x")
  )

test_that("join works on graph with unobserved confounders G_1", {
  expect_equal(join(J_1, D_1, vari_1, cond_1, S_1, M_1, O_1, G_1.unobs, G_1, G_1.obs, topo_1),
               join_output_1)
})

#-------------------------------------------------------------------
# (7) testing that insert works with test case #1
  # currently PASSES

# we can obtain the following from running simplify(P_1, topo_1, G_1.unobs, G_1,
# G_1.obs) with break points (the browser() function). I added print statements
# after step #5 in simplify():
  # Step 6 - Inside nested while loop before join operation
  # P$children[[k]]$cond: z x (this represents cond in simplify())
  # P$sumset[j]: z (this represents S in simplify())

J_1 <- character()
D_1 <- character()
M_1 <- "x"
cond_1 <- c("z", "x")
S_1 <- "z"
O_1 <- c("z", "y")

# we can obtain the following from the graph information:
# G.unobs = G_1.unobs
# G = G_1
# G.obs = G_1.obs
# topo = topo_1

# we expect the output from this (representing J, D) to be:
# [1] character(0)
# [2] character(0)

insert_output_1 <- list(character(0), character(0))

test_that("insert works on graph with unobserved confounders G_1", {
  expect_equal(insert(J_1, D_1, M_1, cond_1, S_1, O_1, G_1.unobs, G_1, G_1.obs, topo_1),
               insert_output_1)
})
