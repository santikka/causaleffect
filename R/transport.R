transport <-
function(y, x, z = NULL, D, expr = TRUE, simp = TRUE, steps = FALSE) {
  v <- get.vertex.attribute(D, "name")
  if (is.null(z)) z <- v
  s <- v[which(vertex.attributes(D)$description == "S")]
  D.causal <- induced.subgraph(D, v[!(v %in% s)])
  return(generalize(y = y, x = x, Z = list(character(0), z), D = list(D.causal, D), expr = expr, simp = simp, steps = steps))
}
