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
# (7) simplify from causal.effect simp = TRUE,
# (8) join (same for causal.effect simp = TRUE vs. FALSE; no need for duplicate unit tests)
# (9) insert (same for causal.effect simp = TRUE vs. FALSE; no need for duplicate unit tests)

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

#-------------------------------------------------------------------
# (8) testing that join works with test case #3
  # produces identical results with simp = TRUE vs. simp = FALSE
  # (no need for duplicate unit tests)
  # currently PASSES

# we can obtain the following from running simplify(P_3_s1 (or s2), topo_3, G_3.unobs, G_3, G_3.obs) with break points
# (the browser() function). I added print statements
# after step #5 in simplify():
  # Step 6 - Inside nested while loop before join operation
  # P$children[[k]]$var: y (this represents vari in simplify())
  # P$children[[k]]$cond: w x (this represents cond in simplify())
  # P$sumset[j]: w (this reprensents S in simplify())

J_3 <- character(0)
D_3 <- character(0)
vari_3 <- "y"
cond_3 <- c("w", "x")
S_3 <- "w"
M_3 <- c("x", "z")
O_3 <- c("w", "y")

# we can obtain the following from the graph information:
  # G.unobs = G_3.unobs
  # G = G_3
  # G.obs = G_3.obs
  # topo = topo_3

# we expect the output from this to be:
  # [1] "y"
  # [2] "w" "x"

join_output_3 <- list(
  c("y"),
  c("w", "x")
)

test_that("join works on simple observed graph G_3 with simp = FALSE", {
  expect_equal(join(J_3, D_3, vari_3, cond_3, S_3, M_3, O_3, G_3.unobs, G_3, G_3.obs, topo_3),
               join_output_3)
})

#-------------------------------------------------------------------
# (9) testing that insert works with test case #3
  # produces identical results with simp = TRUE vs. simp = FALSE
  # (no need for duplicate unit tests)
  # currently PASSES

# we can obtain the following from running simplify(P_3_s1 (or s2), topo_3, G_3.unobs, G_3, G_3.obs) with break points
# (the browser() function). I added print statements
# after step #5 in simplify():
  # Step 6 - Inside nested while loop before join operation
  # P$children[[k]]$var: y (this represents vari in simplify())
  # P$children[[k]]$cond: w x (this represents cond in simplify())
  # P$sumset[j]: w (this reprensents S in simplify())

J_3 <- character(0)
D_3 <- character(0)
M_3 <- c("x", "z")
cond_3 <- c("w", "x")
S_3 <- "w"
O_3 <- c("w", "y")

# we can obtain the following from the graph information:
  # G.unobs = G_3.unobs
  # G = G_3
  # G.obs = G_3.obs
  # topo = topo_3

# we expect the output from this (representing J, D) to be:
  # [1] character(0)
  # [2] character(0)

insert_output_3 <- list(character(0), character(0))

test_that("insert works on simple observed graph G_3 with simp = FALSE", {
  expect_equal(insert(J_3, D_3, M_3, cond_3, S_3, O_3, G_3.unobs, G_3, G_3.obs, topo_3),
               insert_output_3)
})
