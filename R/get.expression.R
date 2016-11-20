get.expression <- function(x, primes = FALSE) {
  query <- unique(unlist(attr(x, "query")))
  prime.counter <- setNames(rep(1, length(query)), query)
  target.sym <- "^*("
  single.source <- FALSE
  if (!is.null(attr(x, "algorithm"))) {
    if (attr(x, "algorithm") == "zid") target.sym <- "("
    if (attr(x, "algorithm") == "trz") single.source <- TRUE
  }
  return (get.expression.internal(x, primes, prime.counter, FALSE, target.sym, single.source))
}
