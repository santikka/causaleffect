get.expression <- function(x, primes = FALSE) {
  query <- unlist(attr(x, "query"))
  prime.counter <- setNames(rep(1, length(query)), query)
  return (get.expression.internal(x, primes, prime.counter, FALSE))
}
