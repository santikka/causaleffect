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
# (4) parse.expression from causal.effect,
# (5) simplify from causal.effect,
# (6) join from causal.effect

# causal.effect with simp = TRUE and simp = FALSE yield the same expression, so
# there are only 6 unit tests compared to 7 unit tests for test cases #2 and #3

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
# ^^ plotting this gives us a bidirected edge, which represents a latent
# confounder we can see in unobserved.graph
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

# we can use the same P_1 and expected_output_1 as we used for parse.expression,
# as the expression passes through parse.expression unchanged.

test_that("simplify works on graph with unobserved confounders G_1", {
  expect_equal(simplify(P_1, topo_1, G_1.unobs, G_1, G_1.obs),
               expected_output_1)
})

#-------------------------------------------------------------------
# (6) testing that join works with test case #1

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
# test case #2 from pp. 6-7 of causaleffect on CRAN - pruning.
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
# (1) testing that topo works with test case #2
# currently PASSES

test_that("topo works on graph with unobserved confounders G_2", {
  expect_equal(topo_2, c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y"))
})


#-------------------------------------------------------------------
# (2) testing that causal.effect works with test case #2 when simp = FALSE
# expression should NOT be simplified.
# currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_2", {
  expect_equal(causal.effect("y", "x", G = G_2, primes = TRUE, prune = TRUE, simp = FALSE),
               "\\frac{\\sum_{z_3,z_5,z_2,z_4}P(y|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}{\\sum_{z_3,z_5,z_2,z_4,y^{\\prime}}P(y^{\\prime}|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}")

})

#-------------------------------------------------------------------
# (3) testing that parse.expression works with test case #2
# causal.effect with simp = FALSE
# currently PASSES

# Trying to do set.primes before parse.expression
vars <- c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y")
counter <- setNames(rep(0, length(vars)), vars)
counter["y"] <- 2
set.primes(vars, FALSE, counter)


# define P_2 for parse.expression() using the output from
# causal.effect("y", "x", G = G_2, expr = FALSE, primes = TRUE, prune = TRUE, simp = TRUE).
# expr = FALSE and simp = TRUE
# the initial probabilistic expression should be:
# \\frac{\\sum_{z_3,z_5,z_2,z_4}P(y|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}
# {\\sum_{z_3,z_5,z_2,z_4,y^{\\prime}}P(y^{\\prime}|z_3,z_5,z_2,z_1,x,z_4)P(z_4|z_3,z_5,z_2,z_1,x)P(x|z_3,z_5,z_2,z_1)P(z_2|z_3,z_5)P(z_5|z_3)P(z_3)}

P_2_pe1 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = "z_5",
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# must define expected output object to match output from parse.expression:
expected_output_2_pe1 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = c("z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now running testthat
test_that("parse.expression works on graph with unobserved confounders G_2", {
  expect_equal(parse.expression(P_2_pe1, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2_pe1)

})


#-------------------------------------------------------------------
# (4) testing that simplify works with test case #2
# causal.effect with simp = FALSE
# currently PASSES

# the simplified expression should look like:
# \\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}
P_2_s1 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = c("z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now must define the expected output object for simplify()
expected_output_2_s1 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = "z_5",
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now running testthat
test_that("simplify works on graph with unobserved confounders G_2", {
  expect_equal(simplify(P_2_s1, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2_s1)
})


#-------------------------------------------------------------------
# (5) testing that causal.effect works with test case #2 when simp = TRUE
# expression should be simplified.
# currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_2", {
  expect_equal(causal.effect("y", "x", G = G_2, primes = TRUE, prune = TRUE, simp = TRUE),
               "\\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}")
})

#-------------------------------------------------------------------
# (6) testing that parse.expression works with test case #2
# causal.effect with simp = TRUE
# currently PASSES


# Trying to do set.primes before parse.expression
vars <- c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y")
counter <- setNames(rep(0, length(vars)), vars)
counter["y"] <- 2
set.primes(vars, FALSE, counter)


# define P_2 for parse.expression() using the output from
# causal.effect("y", "x", G = G_2, expr = FALSE, primes = TRUE, prune = TRUE, simp = TRUE).
# expr = FALSE and simp = TRUE
# the initial probabilistic expression should be:
# \\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}

P_2_pe2 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = "z_5",
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# must define expected output object to match output from parse.expression:
expected_output_2_pe2 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = c("z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now running testthat
test_that("parse.expression works on graph with unobserved confounders G_2", {
  expect_equal(parse.expression(P_2_pe2, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2_pe2)

})


#-------------------------------------------------------------------
# (7) testing that simplify works with test case #2
# causal.effect with simp = TRUE
# currently PASSES

# the simplified expression should look like:
#"\frac{\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\sum_{z_2}P(x|z_1,z_2)P(z_2)}"
P_2_s2 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = c("z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now must define the expected output object for simplify()
expected_output_2_s2 <- list(
  var = character(0),
  cond = character(0),
  sumset = character(0),
  do = character(0),
  product = FALSE,
  fraction = TRUE,
  sum = FALSE,
  children = list(),
  den = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "x",
        cond = c("z_1", "z_2"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  num = list(
    var = character(0),
    cond = character(0),
    sumset = c("z_2", "z_5"),
    do = character(0),
    product = TRUE,
    fraction = FALSE,
    sum = FALSE,
    children = list(
      list(
        var = "y",
        cond = c("x", "z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "x",
        cond = c("z_1", "z_2", "z_5"),
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_2",
        cond = "z_5",
        sumset = character(0),
        do = character(0),
        product = FALSE,
        fraction = FALSE,
        sum = FALSE,
        children = list(),
        den = list(),
        num = list(),
        domain = 0,
        weight = 0,
        class = "probability"
      ),
      list(
        var = "z_5",
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
        weight = 0,
        class = "probability"
      )
    ),
    den = list(),
    num = list(),
    domain = 0,
    weight = 0,
    class = "probability"
  ),
  domain = 0,
  weight = 0,
  class = "probability",
  algorithm = "pid",
  query = list(y = "y", x = "x", z = NULL)
)


# now running testthat
test_that("simplify works on graph with unobserved confounders G_2", {
  expect_equal(simplify(P_2_s2, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2_s2)
})


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


