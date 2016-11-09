aux.effect <-
function(y, x, z, G, expr = TRUE, simp = TRUE, steps = FALSE) {
  return(generalize(y = y, x = x, Z = list(z), D = list(G), expr = expr, simp = simp, steps = steps))
}
