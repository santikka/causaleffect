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


#define P_1 for simplify(). P needs to be a probability object.
  # the initial probabilistic expression should be: ∑z P(y|z,x)P(z)
  # the simplified expression should look like: ∑z P(y|z,x)P(z)
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

simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs)


#-------------------------------------------------------------------
# testing that topo works with test case #1
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_1", {
  expect_equal(topo_1, c("z", "x", "y"))
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
# testing that simplify works with test case #1
  # currently PASSES

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


test_that("simplify works on graph with unobserved confounders G_1", {
  expect_equal(simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)
})

#-------------------------------------------------------------------
# testing that parse.expression works with test case #1
  # currently PASSES

test_that("parse.expression works on graph with unobserved confounders G_1", {
  expect_equal(parse.expression(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)

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
topo_2 <- igraph::topo_sort(G_2.obs)
topo_2 <- igraph::get.vertex.attribute(G_2, "name")[topo_2]

print(topo_2)

plot(G_2)
# ^^ plotting this gives us a bidirected edge, which represents a latent confounder we can see in unobserved.graph
plot(observed.graph(G_2.obs))
plot(unobserved.graph(G_2.unobs))
# ^^ unobserved.graph plots observed graph, plus unobserved node(s)


#-------------------------------------------------------------------
# testing that topo works with test case #2
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_2", {
  expect_equal(topo_2, c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y"))
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
# testing that parse.expression works with test case #2


# Trying to do set.primes before parse.expression
vars <- "y'"

set.primes(vars, new = FALSE, counter = TRUE)

print(P_2_pe)
get.expression(P_2_pe, primes = TRUE)
parse.expression(P_2_pe, topo_2, G_2.unobs, G_2, G_2.obs)


#define P_2 for parse.expression(). P needs to be a probability object.
  # the initial probabilistic expression should be:
  # \\frac{\\sum_{z_3,z_5,z_2,z_4}P(y|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}
  # {\\sum_{z_3,z_5,z_2,z_4,y^{\\prime}}P(y^{\\prime}|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}
P_2_pe <- probability(
  fraction = TRUE,
  num = probability(
    sumset = c("z_3", "z_5", "z_2", "z_4"),
    product = TRUE,
    children = list(
      probability(var = "y", cond = c("z_3", "z_5", "z_2", "z_1", "x", "z_4")),
      probability(var = "z_4", cond = c("z_3", "z_5", "z_2", "z_1", "x")),
      probability(var = "x", cond = c("z_3", "z_5", "z_2", "z_1")),
      probability(var = "z_2", cond = c("z_3", "z_5")),
      probability(var = "z_5", cond = c("z_3")),
      probability(var = "z_3")
    )
  ),
  den = probability(
    sumset = c("z_3", "z_5", "z_2", "z_4", "y'"),
    product = TRUE,
    children = list(
      probability(var = "y'", cond = c("z_3", "z_5", "z_2", "z_1", "x", "z_4")),
      probability(var = "z_4", cond = c("z_3", "z_5", "z_2", "z_1", "x")),
      probability(var = "x", cond = c("z_3", "z_5", "z_2", "z_1")),
      probability(var = "z_2", cond = c("z_3", "z_5")),
      probability(var = "z_5", cond = c("z_3")),
      probability(var = "z_3")
    )
  )
)


#must define expected output object to match output from parse.expression:
expected_output_pe2 <- probability(
  fraction = TRUE,
  num = probability(
    sumset = c("z_2", "z_5"),
    product = TRUE,
    children = list(
      probability(var = "y", cond = c("x", "z_1", "z_2", "z_5")),
      probability(var = "x", cond = c("z_1", "z_2", "z_5")),
      probability(var = "z_2", cond = c("z_5")),
      probability(var = "z_5")
    )
  ),
  den = probability(
    sumset = c("z_2"),
    product = TRUE,
    children = list(
      probability(var = "x", cond = c("z_1", "z_2")),
      probability(var = "z_2")
    )
  )
)


# now running testthat
test_that("parse.expression works on graph with unobserved confounders G_2", {
  expect_equal(parse.expression(P_2_pe, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_pe2)

})


#-------------------------------------------------------------------
# testing that simplify works with test case #2

# the simplified expression should look like:
#\\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}

#expected_output_s2 <-

test_that("simplify works on graph with unobserved confounders G_2", {
  expect_equal(simplify(P_2, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2)
})

simplified_P_2 <- simplify(P_2, topo_2, G_2.unobs, G_2, G_2.obs)
get.expression(simplified_P_2, primes = TRUE)
get.expression(expected_output_2, primes = TRUE)



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


#-------------------------------------------------------------------
# testing that topo works with test case #3.
  # currently PASSES

test_that("topo works on simple observed graph G_3", {
  expect_equal(topo_3, c("w", "x", "z", "y"))
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

#-------------------------------------------------------------------
# testing that parse.expression works with test case #3
  # currently PASSES

#define P_3 for parse.expression(). P needs to be a probability object.
# the initial probabilistic expression should be: ∑w,z P(y∣w,x,z)P(z∣w)P(w).
# the simplified expression should look like: ∑w P(y∣w,x)P(w)
P_3_pe <- probability(
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

#must define expected output object to match output from parse.expression: ∑w P(y|w,x)P(w)
expected_output_pe3 <- probability(
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
  expect_equal(parse.expression(P_3_pe, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_pe3)

})

#-------------------------------------------------------------------
# testing that simplify works with test case #3
  # currently PASSES

#define P_3 for simplify(). P needs to be a list object.
# the simplified expression should look like: ∑w P(y∣w,x)P(w)
child1 <- list(
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
)
attr(child1, "class") <- "probability"

child2 <- list(
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
)
attr(child2, "class") <- "probability"

# Create the main probability object
P_3_s <- list(
  var = character(0),
  cond = character(0),
  sumset = "w",
  do = character(0),
  product = TRUE,
  fraction = FALSE,
  sum = FALSE,
  children = list(child1, child2),
  den = list(),
  num = list(),
  domain = 0,
  weight = 0
)
attr(P_3_s, "class") <- "probability"


#must define expected output object to match output from simplify: ∑w P(y|w,x)P(w)
expected_output_s3 <- probability(
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

#now running testthat
test_that("simplify works on simple observed graph G_3", {
  expect_equal(simplify(P_3_s, topo_3, G_3.unobs, G_3, G_3.obs),
               expected_output_s3)
})

#-------------------------------------------------------------------

?causal.effect

#get.expression must have an object of class probability!

simplified_P_3 <- simplify(P_3_s, topo_3, G_3.unobs, G_3, G_3.obs)
get.expression(simplified_P_3, primes = FALSE)
