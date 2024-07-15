library(testthat)
library(igraph)
library(causaleffect)

causal_effect_files <- list.files("~/Projects/causaleffect/R", pattern = "\\.R$", full.names = TRUE)
lapply(causal_effect_files, source)


#-------------------------------------------------------------------
# test case #1 from pp. 6-7 of causaleffect - includes unobserved confounders.
#-------------------------------------------------------------------
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


#define P_1 for simplify(). P needs to be a list.
  # the initial probabilistic expression should be: ∑x,y,z P(x∣z)P(y∣x,z)P(z)
  # the simplified expression should look like: P(y∣x,z)P(z)
P_1 <- list(
  sumset = c("z"),
  children = list(
    list(var = "y", cond = c("x", "z")),
    list(var = "z", cond = character(0))
  )
)

"\\sum_{z}P(y|z,x)P(z)"

simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs)


#-------------------------------------------------------------------
# testing that topo works with test case #1
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_1", {
  expect_equal(topo_1, c("z", "x", "y"))
})

#-------------------------------------------------------------------
# testing that simplify works with test case #1

expected_output_1 <- list(
  sumset = character(0),
  cond = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
class(expected_output_1) <- "probability"


test_that("simplify works on graph with unobserved confounders G_1", {
  expect_equal(simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)
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
# testing that causal.effect works with test case #1 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_1", {
  expect_equal(causal.effect("y", "x", G = G_1, simp = FALSE),
               "\\sum_{z}P(y|z,x)P(z)")

})


#-------------------------------------------------------------------
# test case #2 from pp. 6-7 of causaleffect - pruning.
#-------------------------------------------------------------------
G_2 <- graph.formula(x -+ z_4, z_4 -+ y, z_1 -+ x, z_2 -+ z_1,
                     z_3 -+ z_2, z_3 -+ x, z_5 -+ z_1, z_5 -+ z_4, x -+ z_2, z_2 -+ x,
                     z_3 -+ z_2, z_2 -+ z_3, z_2 -+ y, y -+ z_2,
                     z_4 -+ y, y -+ z_4, z_5 -+ z_4, z_4 -+ z_5, simplify = FALSE)
G_2 <- set.edge.attribute(graph = G_2, "description", 9:18, "U")
G_2.obs <- observed.graph(G_2)
G_2.unobs <- unobserved.graph(G_2)
topo_2 <- igraph::topological.sort(G_2.obs)
topo_2 <- igraph::get.vertex.attribute(G_2, "name")[topo_2]

print(topo_2)

plot(G_2)
# ^^ plotting this gives us a bidirected edge, which represents a latent confounder we can see in unobserved.graph
plot(observed.graph(G_2.obs))
plot(unobserved.graph(G_2.unobs))
# ^^ unobserved.graph plots observed graph, plus unobserved node(s)


#define P_2 for simplify(). P needs to be a list.
  # the initial probabilistic expression should be: ∑ x,z4,y,z1,z2,z3,z5 P(x∣z1,z3)⋅P(z4∣x)⋅P(y∣z4,z2)⋅P(z1∣z2,z5)⋅P(z2∣z1,z3)⋅P(z3∣x,z2)⋅P(z5∣z4)
  # the simplified expression should look like: ∑ z1,z2,z3,z4,z5 P(x∣z1,z3)⋅P(y∣z2,z4)⋅P(z4∣x)⋅P(z1∣z2,z5)

P_2 <- list(
  sumset = c("x", "z_4", "y", "z_1", "z_2", "z_3", "z_5"),
  children = list(
    list(var = "x", cond = c("z_1", "z_3")),
    list(var = "z_4", cond = c("x")),
    list(var = "y", cond = c("z_4", "z_2")),
    list(var = "z_1", cond = c("z_2", "z_5")),
    list(var = "z_2", cond = c("z_1", "z_3")),
    list(var = "z_3", cond = c("x", "z_2")),
    list(var = "z_5", cond = c("z_4"))
  )
)

print(P_2)
simplify(P_2, topo_2, G_2.unobs, G_2, G_2.obs)


#-------------------------------------------------------------------
# testing that topo works with test case #2
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_2", {
  expect_equal(topo_2, c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y"))
})

#-------------------------------------------------------------------
# testing that simplify works with test case #2

expected_output_2 <- list(
  sumset = c("z_1", "z_2", "z_3", "z_4", "z_5"),
  children = list(
    list(var = "x", cond = c("z_1", "z_3")),
    list(var = "y", cond = c("z_2", "z_4")),
    list(var = "z_4", cond = c("x")),
    list(var = "z_1", cond = c("z_2", "z_5"))
  )
)


test_that("simplify works on graph with unobserved confounders G_2", {
  expect_equal(simplify(P_2, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2)
})

#-------------------------------------------------------------------
# testing that causal.effect works with test case #2 when simp = TRUE
  # expression should be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_2", {
  expect_equal(causal.effect("y", "x", G = G_2, primes = TRUE, prune = TRUE, simp = TRUE),
               "\\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}")
})

#-------------------------------------------------------------------
# testing that causal.effect works with test case #2 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_2", {
  expect_equal(causal.effect("y", "x", G = G_2, primes = TRUE, prune = TRUE, simp = FALSE),
               "\\frac{\\sum_{z_3,z_5,z_2,z_4}P(y|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}{\\sum_{z_3,z_5,z_2,z_4,y^{\\prime}}P(y^{\\prime}|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}")

})



#-------------------------------------------------------------------
# test case #3 from pp. 6-7 of causaleffect - simplify with only observed variables
#-------------------------------------------------------------------
G_3 <- graph.formula(x -+ y, w -+ x, w -+ z, z -+ y)
G_3.obs <- observed.graph(G_3)
G_3.unobs <- unobserved.graph(G_3)
topo_3 <- igraph::topological.sort(G_3.obs)
topo_3 <- igraph::get.vertex.attribute(G_3, "name")[topo_3]

plot(G_3)
plot(G_3.obs)
plot(G_3.unobs)

#define P_3 for simplify(). P needs to be a list.
# the initial probabilistic expression should be: ∑w,z P(y∣w,x,z)P(z∣w)P(w).
# the simplified expression should look like: ∑w P(y∣w,x)P(w)
P_3 <- list(
  sumset = c("w", "z"),
  children = list(
    list(var = "y", cond = c("w", "x", "z")),
    list(var = "z", cond = c("w")),
    list(var = "w", cond = character(0))
  )
)

simplify(P_3, topo_3, G_3.unobs, G_3, G_3.obs)

#-------------------------------------------------------------------
# testing that topo works with test case #3.
# currently passes

test_that("topo works on simple observed graph G_3", {
  expect_equal(topo_3, c("w", "x", "z", "y"))
})

#-------------------------------------------------------------------
# testing that simplify works with test case #3

#must define expected output object to match output from simplify: ∑w P(y|w,x)P(w)
expected_output_3 <- list(
  var = character(0),
  cond = character(0),
  sumset = c("w"),
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(
    list(
      var = "y",
      cond = c("w", "x")
    ),
    list(
      var = "w",
      cond = character(0)
    )
  ),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
class(expected_output_3) <- "probability"

#now running testthat
test_that("simplify works on simple observed graph G_3", {
  expect_equal(simplify(P_3, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_3)
})

#-------------------------------------------------------------------
# testing that causal.effect works with test case #3 when simp = TRUE
  # expression should be simplified.
  # currently PASSES

test_that("causal.effect works on simple observed graph G_3", {
  expect_equal(causal.effect("y", "x", G = G_3, simp = TRUE),
               "\\sum_{w}P(y|w,x)P(w)")
})

#-------------------------------------------------------------------
# testing that causal.effect works with test case #3 when simp = FALSE
  # expression should NOT be simplified.
  # currently PASSES

test_that("causal.effect works on simple observed graph G_3", {
  expect_equal(causal.effect("y", "x", G = G_3, simp = FALSE),
               "\\sum_{w,z}P(y|w,x,z)P(z|w)P(w)")

})

