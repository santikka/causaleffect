probability <-
function(var = character(), cond = character(), sumset = character(), do = character(), 
	     product = FALSE, children = list(), fraction = FALSE, den = list(), num = list(),
	     domain = 0, sum = FALSE, weight = numeric(1)) {
  p <- list(var = var, cond = cond, sumset = sumset, do = do, 
  	        product = product, children = children, fraction = fraction, 
  	        den = den, num = num, domain = domain, sum = sum, weight = weight)
  class(p) = "probability"
  return(p)
}
