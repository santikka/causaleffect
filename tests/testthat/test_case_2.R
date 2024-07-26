library(testthat)
library(igraph)
library(causaleffect)

causal_effect_files <- list.files("~/Projects/causaleffect/R", pattern = "\\.R$", full.names = TRUE)
lapply(causal_effect_files, source)

#-------------------------------------------------------------------
# test case #2 from pp. 6-7 of causaleffect - pruning.
#-------------------------------------------------------------------
# unit tests for functions: 
# (1) topo, 
# (2) causal.effect with simp = TRUE, 
# (3) parse.expression from causal.effect simp = TRUE, 
# (4) simplify from causal.effect simp = TRUE, 
# (5) causal.effect with simp = FALSE, 
# (6) parse.expression from causal.effect simp = FALSE, 
# (7) simplify from causal.effect simp = FALSE

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
# testing that topo works with test case #2
  # currently PASSES

test_that("topo works on graph with unobserved confounders G_2", {
  expect_equal(topo_2, c("z_3", "z_5", "z_2", "z_1", "x", "z_4", "y"))
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


#must define expected output object to match output from parse.expression:
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
# testing that simplify works with test case #2
  # causal.effect with simp = FALSE
  # currently PASSES

# the simplified expression should look like:
  #\\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}
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
# testing that causal.effect works with test case #2 when simp = TRUE
  # expression should be simplified.
  # currently PASSES

test_that("causal.effect works on graph with unobserved confounders G_2", {
  expect_equal(causal.effect("y", "x", G = G_2, primes = TRUE, prune = TRUE, simp = TRUE),
               "\\frac{\\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\\sum_{z_2}P(x|z_1,z_2)P(z_2)}")
})

causal.effect("y", "x", G = G_2, expr = FALSE, primes = TRUE, prune = TRUE, simp = TRUE)

#-------------------------------------------------------------------
# testing that parse.expression works with test case #2
  # causal.effect with simp = TRUE


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


#must define expected output object to match output from parse.expression:
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
# testing that simplify works with test case #2
  # causal.effect with simp = TRUE

# the simplified expression should look like:
  #"\frac{\sum_{z_2,z_5}P(y|x,z_1,z_2,z_5)P(x|z_1,z_2,z_5)P(z_2|z_5)P(z_5)}{\sum_{z_2}P(x|z_1,z_2)P(z_2)}"
# P_2_s2 <- ____


# now must define the expected output object for simplify()
# expected_output_2_s2 <- ____


# now running testthat
test_that("simplify works on graph with unobserved confounders G_2", {
  expect_equal(simplify(P_2_s2, topo_2, G_2.unobs, G_2, G_2.obs),
               expected_output_2_s2)
})


