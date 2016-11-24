probability <- function(var = character(), cond = character(), sumset = character(), do = character(),
  product = FALSE, children = list(), fraction = FALSE, den = list(), num = list(),
  domain = 0, sum = FALSE, weight = numeric(1)) {
  p <- list(var = var, cond = cond, sumset = sumset, do = do,
    product = product, fraction = fraction, sum = sum, children = children,
    den = den, num = num, domain = domain, weight = weight)
  class(p) <- "probability"
  return(p)
}
